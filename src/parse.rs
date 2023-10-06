use lasso::{Rodeo, Spur};
use std::{iter::Peekable, str::Chars};

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
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
}

impl BinaryOp {
    #[allow(clippy::enum_glob_use)]
    fn precedence(self) -> (u8, Associativity) {
        use Associativity::*;
        use BinaryOp::*;
        match self {
            Add | Sub => (1, Left),
            Mul | Div | Mod => (2, Left),
            Pow => (3, Right),
            Eq | Neq | Lt | Gt | Leq | Geq => (4, Left),
            And => (5, Left),
            Or => (6, Left),
        }
    }
}
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Terminal(Terminal),
    // `expr op expr`
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    /// `op expr`
    UnaryOp(UnaryOp, Terminal),
    /// `expr(expr, expr, ...)`
    Call(Spur, Vec<Expr>),
    /// `expr[index]`
    Index(Box<Expr>, Box<Expr>),
    /// `expr.member`
    Member(Box<Expr>, Spur),
    Block(Vec<Stmt>, Option<Box<Expr>>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    Continue,
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
    // TODO: for loops
    // i like rust's for loops so i probably need to figure out the whole pattern
    // business
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DeclarationKind {
    Let,
    Const,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    /// `let ident = expr`
    Declaration(DeclarationKind, Spur, Expr),
    /// `expr`
    Expr(Expr),
}

pub struct Parser<'src> {
    pos: usize,
    src: Peekable<Chars<'src>>,
    /// Buffer of characters that have been read but not yet consumed.
    /// TODO: Try to make this into a string for memory savings. At the moment,
    /// grabbing the first/last character as a peek is not particularly doable.
    buffer: Vec<char>,
    rodeo: Rodeo,
    /// Stack of (start, lexeme) pairs.
    lexeme_stack: Vec<(usize, String)>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(value: &'a str) -> Self {
        Parser::new(value.chars().peekable())
    }
}

const DEFAULT_LEXEME_CAPACITY: usize = 32;

impl Parser<'_> {
    pub fn new(src: Peekable<Chars<'_>>) -> Parser<'_> {
        Parser {
            pos: 0,
            src,
            buffer: Vec::new(),
            rodeo: Rodeo::new(),
            lexeme_stack: Vec::new(),
        }
    }
    /// Begins a lexeme, saving the current position so parsing can be
    /// rewound if necessary.
    fn start_lexeme(&mut self) {
        self.lexeme_stack
            .push((self.pos, String::with_capacity(DEFAULT_LEXEME_CAPACITY)));
    }
    /// Aborts the current lexeme, rewinding the parser to the start of the
    /// lexeme.
    fn abort_lexeme(&mut self) {
        let (start, lexeme) = self
            .lexeme_stack
            .pop()
            .expect("tried to abort empty lexeme stack");
        self.buffer.extend(lexeme.chars().rev());
        self.pos = start;
    }
    /// Finishes the current lexeme, returning the lexeme as a string.
    fn finish_lexeme(&mut self) -> String {
        let (_, lexeme) = self.lexeme_stack.pop().unwrap();
        if self.lexeme_stack.is_empty() {
            self.buffer.clear();
        }
        lexeme
    }
    /// Takes a closure which returns a result. If the result is Ok, the lexeme
    /// is finished and the closure's result is returned. If the result is Err,
    /// the lexeme is aborted and the error is returned.
    fn lexeme<F, T, E>(&mut self, mut f: F) -> Result<T, E>
    where
        F: FnMut(&mut Self) -> Result<T, E>,
    {
        self.start_lexeme();
        let result = f(self);
        match result {
            Ok(t) => {
                self.finish_lexeme();
                Ok(t)
            }
            Err(e) => {
                self.abort_lexeme();
                Err(e)
            }
        }
    }
    /// Returns the next character in the source, checking the buffer first and
    /// also pushing to the current lexeme.
    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.buffer.pop() {
            if let Some(lexeme) = self.lexeme_stack.last_mut() {
                lexeme.1.push(c);
            }
            self.pos += 1;
            Some(c)
        } else {
            let c = self.src.next();
            if let Some(c) = c {
                self.pos += 1;
                if let Some(lexeme) = self.lexeme_stack.last_mut() {
                    lexeme.1.push(c);
                }
            }
            c
        }
    }
    fn peek(&mut self) -> Option<char> {
        self.buffer.last().or_else(|| self.src.peek()).copied()
    }
    fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }
    fn next_while<F>(&mut self, mut f: F) -> String
    where
        F: FnMut(char) -> bool,
    {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if f(c) {
                s.push(c);
                self.next();
            } else {
                break;
            }
        }
        s
    }
    fn match_char(&mut self, c: char) -> Result<char, String> {
        if let Some(c2) = self.peek() {
            if c == c2 {
                self.next();
                Ok(c)
            } else {
                Err(format!("expected {c}, found {c2}"))
            }
        } else {
            Err("unexpected end of file".to_string())
        }
    }
    fn match_str<'a>(&mut self, s: &'a str) -> Result<&'a str, String> {
        for c in s.chars() {
            if let Some(c2) = self.peek() {
                if c != c2 {
                    return Err(format!("expected {c}, found {c2}"));
                }
                self.next();
            } else {
                return Err("unexpected end of file".to_string());
            }
        }
        Ok(s)
    }
    fn boolean(&mut self) -> Result<bool, String> {
        self.lexeme(|s| match s.next() {
            Some('t') => s.match_str("rue").map(|_| true),
            Some('f') => s.match_str("alse").map(|_| false),
            _ => Err("expected true or false".to_string()),
        })
    }
    fn ident(&mut self) -> Result<Spur, &'static str> {
        self.lexeme(|s| match s.peek() {
            Some(c) if unicode_ident::is_xid_start(c) || c == '$' || c == '_' => {
                let identifier = s.next_while(|c| unicode_ident::is_xid_continue(c) || c == '_');
                let identifier = s.rodeo.get_or_intern(identifier);
                Ok(identifier)
            }
            _ => Err("identifier must start with XID_Start or _"),
        })
    }
    fn int(&mut self) -> Result<i64, &'static str> {
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
    fn float(&mut self) -> Result<f64, &'static str> {
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
    fn string(&mut self) -> Result<Spur, &'static str> {
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
    fn terminal(&mut self) -> Result<Terminal, String> {
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
                let next = s.peek().unwrap_or(' ');
                let pos = s.pos;
                return Err(format!("expected terminal, got '{next}' at {pos}"));
            };
            s.skip_whitespace();
            Ok(terminal)
        })
    }
    fn skip_whitespace(&mut self) {
        self.next_while(|c| c.is_ascii_whitespace());
    }
    fn expect_whitespace(&mut self) -> Result<(), &'static str> {
        if self.next_while(|c| c.is_ascii_whitespace()).is_empty() {
            Err("expected whitespace")
        } else {
            Ok(())
        }
    }
    #[allow(clippy::enum_glob_use)]
    fn binary_operator(&mut self) -> Result<BinaryOp, String> {
        use BinaryOp::*;
        self.lexeme(|s| {
            let op = match s.next() {
                Some('+') => Add,
                Some('-') => Sub,
                Some('*') => Mul,
                Some('/') => Div,
                Some('%') => Mod,
                Some('^') => Pow,
                Some('=') => {
                    if let Some('=') = s.peek() {
                        s.next();
                        Eq
                    } else {
                        return Err("expected =".to_string());
                    }
                }
                Some('!') => {
                    if let Some('=') = s.peek() {
                        s.next();
                        Neq
                    } else {
                        return Err("expected =".to_string());
                    }
                }
                Some('<') => {
                    if let Some('=') = s.peek() {
                        s.next();
                        Leq
                    } else {
                        Lt
                    }
                }
                Some('>') => {
                    if let Some('=') = s.peek() {
                        s.next();
                        Geq
                    } else {
                        Gt
                    }
                }
                Some('&') => {
                    if let Some('&') = s.peek() {
                        s.next();
                        And
                    } else {
                        return Err("expected &".to_string());
                    }
                }
                Some('|') => {
                    if let Some('|') = s.peek() {
                        s.next();
                        Or
                    } else {
                        return Err("expected |".to_string());
                    }
                }
                Some(c) => return Err(format!("unexpected token {c}")),
                None => return Err("unexpected end of file".to_string()),
            };
            s.skip_whitespace();
            Ok(op)
        })
    }
    fn unary_op(&mut self) -> Result<UnaryOp, String> {
        self.lexeme(|s| {
            let op = match s.next() {
                Some('-') => UnaryOp::Neg,
                Some('!') => UnaryOp::Not,
                Some(c) => {
                    return Err(format!("unexpected token {c}"));
                }
                None => {
                    return Err("unexpected end of file".to_string());
                }
            };
            s.skip_whitespace();
            Ok(op)
        })
    }
    fn unary(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            let op = s.unary_op().ok();
            let terminal = s.terminal()?;
            if let Some(op) = op {
                Ok(Expr::UnaryOp(op, terminal))
            } else {
                Ok(Expr::Terminal(terminal))
            }
        })
    }
    fn factor(&mut self) -> Result<Expr, String> {
        // factor         → unary ( ( "/" | "*" ) unary )* ;
        self.lexeme(|s| {
            let mut expr = s.unary()?;
            while let Some(op) = s.peek() {
                match op {
                    '/' | '*' => {
                        s.next();
                        s.skip_whitespace();
                        let rhs = s.unary()?;
                        expr = Expr::BinaryOp(
                            match op {
                                '/' => BinaryOp::Div,
                                '*' => BinaryOp::Mul,
                                _ => unreachable!(),
                            },
                            Box::new(expr),
                            Box::new(rhs),
                        );
                    }
                    _ => break,
                }
            }
            Ok(expr)
        })
    }
    fn term(&mut self) -> Result<Expr, String> {
        // term           → factor ( ( "-" | "+" ) factor )* ;
        self.lexeme(|s| {
            let mut expr = s.factor()?;
            while let Some(op) = s.peek() {
                match op {
                    '-' | '+' => {
                        s.next();
                        s.skip_whitespace();
                        let rhs = s.factor()?;
                        expr = Expr::BinaryOp(
                            match op {
                                '-' => BinaryOp::Sub,
                                '+' => BinaryOp::Add,
                                _ => unreachable!(),
                            },
                            Box::new(expr),
                            Box::new(rhs),
                        );
                    }
                    _ => break,
                }
            }
            Ok(expr)
        })
    }
    fn compare_op(&mut self) -> Result<BinaryOp, &'static str> {
        let r = self.lexeme(|s| match s.next() {
            Some('<') => {
                if let Some('=') = s.peek() {
                    s.next();
                    Ok(BinaryOp::Leq)
                } else {
                    Ok(BinaryOp::Lt)
                }
            }
            Some('>') => {
                if let Some('=') = s.peek() {
                    s.next();
                    Ok(BinaryOp::Geq)
                } else {
                    Ok(BinaryOp::Gt)
                }
            }
            _ => Err("expected comparison operator"),
        });
        self.skip_whitespace();
        r
    }
    fn comparison(&mut self) -> Result<Expr, String> {
        // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        self.lexeme(|s| {
            let mut expr = s.term()?;
            while let Ok(op) = s.compare_op() {
                let rhs = s.term()?;
                expr = Expr::BinaryOp(op, Box::new(expr), Box::new(rhs));
            }
            Ok(expr)
        })
    }
    fn equal_op(&mut self) -> Result<BinaryOp, &'static str> {
        let r = self.lexeme(|s| match s.next() {
            Some('=') => {
                if let Some('=') = s.peek() {
                    s.next();
                    Ok(BinaryOp::Eq)
                } else {
                    Err("expected =")
                }
            }
            Some('!') => {
                if let Some('=') = s.peek() {
                    s.next();
                    Ok(BinaryOp::Neq)
                } else {
                    Err("expected =")
                }
            }
            _ => Err("expected equality op"),
        });
        self.skip_whitespace();
        r
    }
    fn equality(&mut self) -> Result<Expr, String> {
        // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        self.lexeme(|s| {
            let mut expr = s.comparison()?;
            while let Ok(op) = s.equal_op() {
                let rhs = s.comparison()?;
                expr = Expr::BinaryOp(op, Box::new(expr), Box::new(rhs));
            }
            Ok(expr)
        })
    }
    fn expr(&mut self) -> Result<Expr, String> {
        self.equality()
    }
    fn paren_expr(&mut self) -> Result<Expr, String> {
        self.start_lexeme();
        if let Some('(') = self.peek() {
            self.next();
        } else {
            self.abort_lexeme();
            return Err("expected (".to_string());
        }
        let expr = match self.expr() {
            Ok(expr) => expr,
            Err(e) => {
                self.abort_lexeme();
                return Err(e);
            }
        };
        if let Some(')') = self.peek() {
            self.next();
        } else {
            self.abort_lexeme();
            return Err("expected )".to_string());
        }
        self.finish_lexeme();
        Ok(expr)
    }
    fn declaration(&mut self) -> Result<Stmt, String> {
        self.lexeme(|s| {
            let kind = match s.peek() {
                Some('l') => {
                    s.match_str("let")?;
                    DeclarationKind::Let
                }
                Some('c') => {
                    s.match_str("const")?;
                    DeclarationKind::Const
                }
                _ => return Err("expected let or const".to_string()),
            };
            s.skip_whitespace();
            let ident = s.ident()?;
            s.skip_whitespace();
            if let Some('=') = s.peek() {
                s.next();
            } else {
                return Err("expected =".to_string());
            }
            s.skip_whitespace();
            let expr = s.expr()?;
            Ok(Stmt::Declaration(kind, ident, expr))
        })
    }
    fn statement(&mut self) -> Result<Stmt, String> {
        self.lexeme(|s| {
            let statement = if let Ok(declaration) = s.declaration() {
                Ok(declaration)
            } else {
                let expr = s.expr()?;
                Ok(Stmt::Expr(expr))
            };
            if let Some(';') = s.peek() {
                s.next();
            }
            s.skip_whitespace();
            statement
        })
    }
    fn block(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            s.match_char('{')?;
            s.skip_whitespace();
            let mut stmts = Vec::new();
            while let Ok(stmt) = s.statement() {
                stmts.push(stmt);
            }
            let expr = if let Some(Stmt::Expr(_)) = stmts.last() {
                match stmts.pop().unwrap() {
                    Stmt::Expr(e) => Some(Box::new(e)),
                    _ => unreachable!(),
                }
            } else {
                None
            };
            s.match_char('}')?;
            s.skip_whitespace();
            Ok(Expr::Block(stmts, expr))
        })
    }
    pub fn if_expr(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            s.match_str("if")?;
            s.expect_whitespace()?;
            let cond = s.expr()?;
            s.skip_whitespace();
            let then = s.block()?;
            s.skip_whitespace();
            let else_ = if let Some('e') = s.peek() {
                s.match_str("else")?;
                s.skip_whitespace();
                let expr = s.block()?;
                Some(Box::new(expr))
            } else {
                None
            };
            Ok(Expr::If(Box::new(cond), Box::new(then), else_))
        })
    }
}
/*
pub fn parse(src: &str) -> Vec<Stmt> {
    let mut parser = Parser {
        pos: 0,
        src: src.chars().peekable(),
        rodeo: Rodeo::new(),
    };
    parser.parse()
} */

#[cfg(test)]
mod parse_test {
    use super::*;

    #[test]
    fn next() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.next(), Some('x'));
        assert_eq!(parser.pos, 1);
        assert_eq!(parser.next(), Some('y'));
        assert_eq!(parser.pos, 2);
        assert_eq!(parser.next(), Some('z'));
        assert_eq!(parser.pos, 3);
        assert_eq!(parser.next(), None);
        assert_eq!(parser.pos, 3);
    }

    #[test]
    fn peek() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.peek(), Some('x'));
        assert_eq!(parser.pos, 0);
    }

    #[test]
    fn lexeme_basic() {
        let mut parser = Parser::from("xyz");
        parser.start_lexeme();
        assert_eq!(parser.next(), Some('x'));
        assert_eq!(parser.next(), Some('y'));
        assert_eq!(parser.next(), Some('z'));
        assert_eq!(parser.finish_lexeme(), "xyz");
        assert_eq!(parser.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn lexeme_abort() {
        let mut parser = Parser::from("xyz");
        parser.start_lexeme();
        assert_eq!(parser.next(), Some('x'));
        assert_eq!(parser.next(), Some('y'));
        assert_eq!(parser.next(), Some('z'));
        parser.abort_lexeme();
        assert_eq!(parser.pos, 0);
        assert_eq!(parser.buffer, vec!['z', 'y', 'x']);
        assert_eq!(parser.lexeme_stack, Vec::new());
        assert_eq!(parser.next(), Some('x'));
        assert_eq!(parser.next(), Some('y'));
        assert_eq!(parser.next(), Some('z'));
    }

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
    }

    #[test]
    fn float() {
        let mut parser = Parser::from("123.456");
        assert_eq!(parser.float(), Ok(123.456));
        assert_eq!(parser.pos, 7);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
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
    fn factor() {
        let mut parser = Parser::from("2 * 3 / 4");
        assert_eq!(
            parser.factor(),
            Ok(Expr::BinaryOp(
                BinaryOp::Div,
                Box::new(Expr::BinaryOp(
                    BinaryOp::Mul,
                    Box::new(Expr::Terminal(Terminal::Int(2))),
                    Box::new(Expr::Terminal(Terminal::Int(3)))
                )),
                Box::new(Expr::Terminal(Terminal::Int(4)))
            ))
        );
    }

    #[test]
    fn expr() {
        let mut parser = Parser::from("1 + 2 * 3");
        assert_eq!(
            parser.expr(),
            Ok(Expr::BinaryOp(
                BinaryOp::Add,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::BinaryOp(
                    BinaryOp::Mul,
                    Box::new(Expr::Terminal(Terminal::Int(2))),
                    Box::new(Expr::Terminal(Terminal::Int(3)))
                ))
            ))
        );
    }

    #[test]
    fn statement() {
        let mut parser = Parser::from("let x = 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Declaration(
                DeclarationKind::Let,
                parser.rodeo.get_or_intern("x"),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
    }

    #[test]
    fn if_expr() {
        let mut parser = Parser::from("if x { 1 } else { 2 }");
        assert_eq!(
            parser.if_expr(),
            Ok(Expr::If(
                Box::new(Expr::Terminal(Terminal::Ident(
                    parser.rodeo.get_or_intern("x")
                ))),
                Box::new(Expr::Block(
                    vec![],
                    Some(Box::new(Expr::Terminal(Terminal::Int(1))))
                )),
                Some(Box::new(Expr::Block(
                    vec![],
                    Some(Box::new(Expr::Terminal(Terminal::Int(2))))
                )))
            ))
        );
    }
}
