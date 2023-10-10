use lasso::Spur;

use super::expr::{Expr, Stmt};
use super::Parser;
use super::types::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub generic: Vec<Type>,
    pub args: Vec<(Spur, Option<Type>)>,
    pub return_type: Option<Type>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Terminal {
    Int(i64),
    Float(f64),
    Ident(Spur),
    String(Spur),
    Bool(bool),
    /// parenthesised expression
    Expr(Box<Expr>),
    Function(Function),
    Block(Block),
    Array(Vec<Expr>),
}



const RESERVED_NAMES: &[&str] = &[
    "let", "const", "if", "else", "while", "continue", "break", "return", "true", "false", "match",
    "not", "and", "or", "xor", "shl", "shr", "import", "export", "as", "from", "struct", "enum",
    "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "float32",
    "float64", "string", "bool", "void"
];

impl Parser<'_> {
    fn boolean(&mut self) -> Result<bool, String> {
        self.lexeme(|s| match s.next() {
            Some('t') => s.match_str("rue").map(|_| true),
            Some('f') => s.match_str("alse").map(|_| false),
            _ => Err("expected true or false".to_string()),
        })
    }
    pub fn ident(&mut self) -> Result<Spur, String> {
        self.lexeme(|s| match s.next() {
            Some(c) if unicode_ident::is_xid_start(c) || c == '$' || c == '_' => {
                let identifier = s.next_while(|c| unicode_ident::is_xid_continue(c) || c == '_');
                let identifier = format!("{c}{identifier}");
                if RESERVED_NAMES.contains(&identifier.as_str()) {
                    return Err(format!("identifier '{identifier}' is reserved"));
                }
                let identifier = s.rodeo.get_or_intern(identifier);
                Ok(identifier)
            }
            _ => Err("identifier must start with XID_Start or _".to_string()),
        })
    }
    pub fn int(&mut self) -> Result<i64, &'static str> {
        self.lexeme(|s| {
            let number = s.next_while(|c| c.is_ascii_digit());
            if let Some('.') = s.peek() {
                Err("expected integer, found float")
            } else if let Ok(number) = number.parse::<i64>() {
                Ok(number)
            } else {
                Err("invalid integer")
            }
        })
    }
    pub fn float(&mut self) -> Result<f64, &'static str> {
        self.lexeme(|s| {
            let n1 = s.next_while(|c| c.is_ascii_digit());
            if let Some('.') = s.peek() {
                s.next();
                let n2 = s.next_while(|c| c.is_ascii_digit());
                if let Ok(n) = format!("{n1}.{n2}").parse::<f64>() {
                    Ok(n)
                } else {
                    Err("invalid float")
                }
            } else {
                Err("expected float, found integer")
            }
        })
    }
    pub fn string(&mut self) -> Result<Spur, &'static str> {
        self.lexeme(|s| {
            if let Some('"') = s.peek() {
                s.next();
                let string = s.next_while(|c| c != '"' && c != '\n');
                if let Some('"') = s.peek() {
                    s.next();
                    let string = s.rodeo.get_or_intern(string);
                    Ok(string)
                } else {
                    Err("unterminated string")
                }
            } else {
                Err("expected string")
            }
        })
    }
    fn paren_expr(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            s.match_char('(')?;
            let expr = s.expr()?;
            s.match_char(')')?;
            Ok(expr)
        })
    }

    fn function_argument(&mut self) -> Result<(Spur, Option<Type>), String> {
        self.lexeme(|s| {
            let ident = s.ident()?;
            s.skip_whitespace();
            let r#type = if let Some(':') = s.peek() {
                s.next();
                s.skip_whitespace();
                Some(s.r#type()?)
            } else {
                None
            };
            Ok((ident, r#type))
        })
    }

    fn function_arglist(&mut self) -> Result<Vec<(Spur, Option<Type>)>, String> {
        self.lexeme(|s| {
            s.match_char('(')?;
            let args = s.delimited(',', Parser::function_argument)?;
            s.match_char(')')?;
            Ok(args)
        })
    }

    fn short_function_definition(&mut self) -> Result<Function, String> {
        self.lexeme(|s| {
            let generic = if let Some('<') = s.peek() {
                s.generic()?
            } else {
                Vec::new()
            };
            let args = s.function_arglist()?;
            s.skip_whitespace();
            let return_type = s.function_return_type().ok();
            s.skip_whitespace();
            s.match_str("=>")?;
            s.skip_whitespace();
            let body = Box::new(s.expr()?);
            // if let Expr::Block(_) = expr {
            //     return Err("expected non-block expression, found block".to_string());
            // }
            let function = Function {
                generic,
                args,
                return_type,
                body,
            };
            Ok(function)
        })
    }

    fn long_function_definition(&mut self) -> Result<Function, String> {
        self.lexeme(|s| {
            let generic = if let Some('<') = s.peek() {
                s.generic()?
            } else {
                Vec::new()
            };
            let args = s.function_arglist()?;
            s.skip_whitespace();
            let return_type = s.function_return_type()?;
            s.skip_whitespace();
            s.match_str("=>")?;
            s.skip_whitespace();
            let body = s.block()?;
            let function = Function {
                generic,
                args,
                return_type: Some(return_type),
                body: Box::new(Expr::Terminal(Terminal::Block(body))),
            };
            Ok(function)
        })
    }

    fn function_definition(&mut self) -> Result<Function, String> {
        self.lexeme(|s| {
            if let Ok(function) = s.long_function_definition() {
                return Ok(function);
            }
            if let Ok(function) = s.short_function_definition() {
                return Ok(function);
            }
            Err("expected function definition".to_string())
        })
    }

    pub fn block(&mut self) -> Result<Block, String> {
        self.lexeme(|s| {
            s.match_char('{')?;
            s.skip_whitespace();
            let mut stmts = Vec::new();
            while let Ok(stmt) = s.statement() {
                stmts.push(stmt);
                s.skip_whitespace();
            }
            let expr = if let Some(Stmt::Expr(_)) = stmts.last() {
                match stmts.pop().unwrap() {
                    Stmt::Expr(e) => Some(Box::new(e)),
                    _ => unreachable!(),
                }
            } else {
                None
            };
            s.skip_whitespace();
            s.match_char('}')?;
            s.skip_whitespace();
            Ok(Block {
                statements: stmts,
                expr,
            })
        })
    }

    pub fn array(&mut self) -> Result<Vec<Expr>, String> {
        self.lexeme(|s| {
            s.match_char('[')?;
            let array = s.delimited(',', Parser::expr)?;
            s.match_char(']')?;
            Ok(array)
        })
    }

    pub fn terminal(&mut self) -> Result<Terminal, String> {
        self.lexeme(|s| {
            let terminal = 
            if let Ok(array) = s.array() {
                Terminal::Array(array)
            } else if let Ok(block) = s.block() {
                Terminal::Block(block)
            }else  if let Ok(function) = s.function_definition() {
                Terminal::Function(function)
            } else if let Ok(ident) = s.ident() {
                Terminal::Ident(ident)
            } else if let Ok(int) = s.int() {
                Terminal::Int(int)
            } else if let Ok(float) = s.float() {
                Terminal::Float(float)
            } else if let Ok(string) = s.string() {
                Terminal::String(string)
            } else if let Ok(expr) = s.paren_expr() {
                Terminal::Expr(Box::new(expr))
            } else if let Ok(boolean) = s.boolean() {
                Terminal::Bool(boolean)
            } else {
                if let Some(c) = s.peek() {
                    return Err(s.make_error(format!("unexpected character '{c}'")));
                }
                return Err(s.make_error("unexpected end of file".to_string()));
            };
            s.skip_whitespace();
            Ok(terminal)
        })
    }

    
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::expr::{BinaryOp, Stmt, DeclarationKind};
    #[test]
    fn ident() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.ident().map(|i| parser.rodeo.resolve(&i)), Ok("xyz"));
        assert_eq!(parser.position.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn unicode_ident() {
        let mut parser = Parser::from("ಠ_ಠ");
        assert_eq!(parser.ident().map(|i| parser.rodeo.resolve(&i)), Ok("ಠ_ಠ"));
        assert_eq!(parser.position.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn int() {
        let mut parser = Parser::from("123");
        assert_eq!(parser.int(), Ok(123));
        assert_eq!(parser.position.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());

        assert!(Parser::from("123.").int().is_err());
    }

    #[test]
    fn bool() {
        assert_eq!(Parser::from("true").boolean(), Ok(true));
        assert_eq!(Parser::from("false").boolean(), Ok(false));
    }

    #[test]
    fn float() {
        let mut parser = Parser::from("123.456");
        assert_eq!(parser.float(), Ok(123.456));
        assert_eq!(parser.position.pos, 7);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());

        assert!(Parser::from("123").float().is_err());
    }

    #[test]
    fn string() {
        let mut parser = Parser::from("\"hello world\"");
        assert_eq!(
            parser.string().map(|i| parser.rodeo.resolve(&i)),
            Ok("hello world")
        );
        assert_eq!(parser.position.pos, 13);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());

        assert_eq!(
            Parser::from("\"hello\nworld\"").string(),
            Err("unterminated string")
        );
    }

    #[test]
    fn paren_expr() {
        let mut parser = Parser::from("(1 + 2)");
        assert_eq!(
            parser.paren_expr(),
            Ok(Expr::BinaryOp(
                BinaryOp::Add,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
        assert_eq!(parser.position.pos, 7);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn terminal() {
        let mut parser = Parser::from("ident");
        assert_eq!(
            parser.terminal(),
            Ok(Terminal::Ident(parser.rodeo.get_or_intern("ident")))
        );
        assert_eq!(Parser::from("123").terminal(), Ok(Terminal::Int(123)));
        assert_eq!(
            Parser::from("123.456").terminal(),
            Ok(Terminal::Float(123.456))
        );
        assert_eq!(Parser::from("true").terminal(), Ok(Terminal::Bool(true)));
        assert_eq!(
            Parser::from("(1)").terminal(),
            Ok(Terminal::Expr(Box::new(Expr::Terminal(Terminal::Int(1)))))
        );
        let mut parser = Parser::from("\"hello world\"");
        assert_eq!(
            parser.terminal(),
            Ok(Terminal::String(parser.rodeo.get_or_intern("hello world")))
        );
    }

    #[test]
    #[allow(clippy::too_many_lines)]
    fn function() {
        let mut parser = Parser::from("() => 1");
        assert_eq!(
            parser.function_definition(),
            Ok(Function {
                generic: Vec::new(),
                args: Vec::new(),
                return_type: None,
                body: Box::new(Expr::Terminal(Terminal::Int(1)))
            })
        );
        let mut parser = Parser::from("(a) => 1");
        assert_eq!(
            parser.function_definition(),
            Ok(Function {
                generic: Vec::new(),
                args: vec![(
                    parser.rodeo.get_or_intern("a"),
                    None,
                )],
                return_type: None,
                body: Box::new(Expr::Terminal(Terminal::Int(1)))
            })
        );
        let mut parser = Parser::from("(a: number) => 1");
        assert_eq!(
            parser.function_definition(),
            Ok(Function {
                generic: Vec::new(),
                args: vec![(
                    parser.rodeo.get_or_intern("a"),
                    Some(Type::Number),
                )],
                return_type: None,
                body: Box::new(Expr::Terminal(Terminal::Int(1)))
            })
        );
        let mut parser = Parser::from("<T>(a: number) => 1");
        assert_eq!(
            parser.function_definition(),
            Ok(Function {
                generic: vec![Type::Ident(parser.rodeo.get_or_intern("T"), Vec::new())],
                args: vec![(
                    parser.rodeo.get_or_intern("a"),
                    Some(Type::Number),
                )],
                return_type: None,
                body: Box::new(Expr::Terminal(Terminal::Int(1)))
            })
        );
        let mut parser = Parser::from("(a: number, b: number) => 1");
        assert_eq!(
            parser.function_definition(),
            Ok(Function {
                generic: Vec::new(),
                args: vec![
                    (
                        parser.rodeo.get_or_intern("a"),
                        Some(Type::Number),
                    ),
                    (
                        parser.rodeo.get_or_intern("b"),
                        Some(Type::Number),
                    ),
                ],
                return_type: None,
                body: Box::new(Expr::Terminal(Terminal::Int(1)))
            })
        );
        let mut parser = Parser::from("(a: number, b: number) -> number => 1");
        assert_eq!(
            parser.function_definition(),
            Ok(Function {
                generic: Vec::new(),
                args: vec![
                    (
                        parser.rodeo.get_or_intern("a"),
                        Some(Type::Number),
                    ),
                    (
                        parser.rodeo.get_or_intern("b"),
                        Some(Type::Number),
                    ),
                ],
                return_type: Some(Type::Number),
                body: Box::new(Expr::Terminal(Terminal::Int(1)))
            })
        );
    }

    #[test]
    fn asdf() {
        let mut parser = Parser::from(r"() -> number => {
            let a = 1;
            a
        }");
        assert_eq!(
            parser.function_definition(),
            Ok(Function {
                generic: Vec::new(),
                args: Vec::new(),
                return_type: Some(Type::Number),
                body: Box::new(Expr::Terminal(Terminal::Block(Block {
                    statements: vec![
                        Stmt::Declaration(DeclarationKind::Let, None, parser.rodeo.get_or_intern("a"),  Expr::Terminal(Terminal::Int(1))),
                    ],
                    expr: Some(Box::new(Expr::Terminal(Terminal::Ident(parser.rodeo.get_or_intern("a"))))),
                })))
            })
        );
    }
}
