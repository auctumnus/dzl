use lasso::Spur;

use super::expr::Expr;
use super::Parser;

#[derive(Debug, PartialEq, Clone)]
pub enum Terminal {
    Int(i64),
    Float(f64),
    Ident(Spur),
    String(Spur),
    Bool(bool),
    /// parenthesised expression
    Expr(Box<Expr>),
}

const RESERVED_NAMES: &[&str] = &[
    "let", "const", "if", "else", "while", "continue", "break", "return", "true", "false", "match",
    "not", "and", "or", "xor", "shl", "shr", "import", "export", "as", "from", "struct", "enum",
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
        self.lexeme(|s| match s.peek() {
            Some(c) if unicode_ident::is_xid_start(c) || c == '$' || c == '_' => {
                let identifier = s.next_while(|c| unicode_ident::is_xid_continue(c) || c == '_');
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
    pub fn terminal(&mut self) -> Result<Terminal, String> {
        self.lexeme(|s| {
            let terminal = if let Ok(ident) = s.ident() {
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
                let pos = s.pos;
                if let Some(c) = s.peek() {
                    return Err(format!("unexpected character '{c}' at {pos}"));
                }
                return Err(format!("unexpected end of file at {pos}"));
            };
            s.skip_whitespace();
            Ok(terminal)
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::expr::BinaryOp;
    #[test]
    fn ident() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.ident().map(|i| parser.rodeo.resolve(&i)), Ok("xyz"));
        assert_eq!(parser.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn unicode_ident() {
        let mut parser = Parser::from("ಠ_ಠ");
        assert_eq!(parser.ident().map(|i| parser.rodeo.resolve(&i)), Ok("ಠ_ಠ"));
        assert_eq!(parser.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn int() {
        let mut parser = Parser::from("123");
        assert_eq!(parser.int(), Ok(123));
        assert_eq!(parser.pos, 3);
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
        assert_eq!(parser.pos, 7);
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
        assert_eq!(parser.pos, 13);
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
        assert_eq!(parser.pos, 7);
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
}
