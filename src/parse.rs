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
    fn precedence(&self) -> (u8, Associativity) {
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
}
#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// `let ident = expr`
    Let(Spur, Expr),
    /// `expr`
    Expr(Expr),
    /// `if expr { stmt } else { stmt }`
    If(Expr, Box<Stmt>, Box<Stmt>),
    /// `while expr { stmt }`
    While(Expr, Box<Stmt>),
    /// `return expr`
    Return(Expr),
    /// `break`
    Break,
    /// `continue`
    Continue,
    /// `block`
    Block(Vec<Stmt>),
}

struct Parser<'src> {
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
    fn new(src: Peekable<Chars<'_>>) -> Parser<'_> {
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
    fn match_str(&mut self, s: &str) -> Result<(), String> {
        for c in s.chars() {
            if let Some(c2) = self.next() {
                if c != c2 {
                    return Err(format!("expected {c}, found {c2}"));
                }
            } else {
                return Err("unexpected end of file".to_string());
            }
        }
        Ok(())
    }
    fn boolean(&mut self) -> Result<bool, String> {
        self.lexeme(|s| match s.next() {
            Some('t') => {
                s.match_str("rue");
                Ok(true)
            }
            Some('f') => {
                s.match_str("alse");
                Ok(false)
            }
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
                return Err("expected terminal".to_string());
            };
            s.skip_whitespace();
            Ok(terminal)
        })
    }
    fn skip_whitespace(&mut self) {
        self.next_while(|c| c.is_ascii_whitespace());
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

    fn unary_op(&mut self) -> Result<(UnaryOp, Terminal), String> {
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
            let terminal = match s.terminal() {
                Ok(terminal) => terminal,
                Err(e) => return Err(e),
            };
            Ok((op, terminal))
        })
    }
    fn factor(&mut self) -> Result<Expr, String> {
        // factor         → unary ( ( "/" | "*" ) unary )* ;
        self.lexeme(|s| {
            let mut expr = match s.unary_op() {
                Ok((op, terminal)) => Expr::UnaryOp(op, terminal),
                Err(e) => return Err(e),
            };
            while let Some(op) = s.peek() {
                match op {
                    '/' | '*' => {
                        s.next();
                        let rhs = match s.unary_op() {
                            Ok((op, terminal)) => Expr::UnaryOp(op, terminal),
                            Err(e) => return Err(e),
                        };
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
    fn expr(&mut self) -> Result<Expr, String> {
        // expression     → equality ;
        // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        // term           → factor ( ( "-" | "+" ) factor )* ;
        // factor         → unary ( ( "/" | "*" ) unary )* ;
        // unary          → ( "!" | "-" ) unary
        //                | primary ;
        // primary        → NUMBER | STRING | "true" | "false" | "nil"
        //                | "(" expression ")" ;
        unimplemented!()
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
    /*
    fn stmt(&mut self) -> Stmt {
        match self.peek() {
            Some(&c) if c.is_ascii_alphabetic() || c == '_' => {
                let ident = self.ident();
                match self.peek() {
                    Some(&'=') => {
                        self.next();
                        let expr = self.expr();
                        Stmt::Let(ident, expr)
                    }
                    _ => panic!("unexpected token"),
                }
            }
            _ => panic!("unexpected token"),
        }
    }

    fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.at_end() {
            stmts.push(self.stmt());
        }
        stmts
    }*/
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
    fn test_next() {
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
    fn test_peek() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.peek(), Some('x'));
        assert_eq!(parser.pos, 0);
    }

    #[test]
    fn test_lexeme_basic() {
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
    fn test_lexeme_abort() {
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
    fn test_ident() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.ident().map(|i| parser.rodeo.resolve(&i)), Ok("xyz"));
        assert_eq!(parser.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn test_unicode_ident() {
        let mut parser = Parser::from("ಠ_ಠ");
        assert_eq!(parser.ident().map(|i| parser.rodeo.resolve(&i)), Ok("ಠ_ಠ"));
        assert_eq!(parser.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn test_int() {
        let mut parser = Parser::from("123");
        assert_eq!(parser.int(), Ok(123));
        assert_eq!(parser.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn test_float() {
        let mut parser = Parser::from("123.456");
        assert_eq!(parser.float(), Ok(123.456));
        assert_eq!(parser.pos, 7);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());
    }

    #[test]
    fn test_string() {
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
    fn test_paren_expr() {
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
    fn test_expr() {
        // let mut parser = Parser::from("1 + 2 * 3");
        // assert_eq!(
        //     parser.expr(),
        //     Ok(Expr::BinaryOp(
        //         BinaryOp::Add,
        //         Box::new(Expr::Terminal(Terminal::Int(1))),
        //         Box::new(Expr::BinaryOp(
        //             BinaryOp::Mul,
        //             Box::new(Expr::Terminal(Terminal::Int(2))),
        //             Box::new(Expr::Terminal(Terminal::Int(3)))
        //         ))
        //     ))
        // );
        let mut parser = Parser::from("1 + 2");
        assert_eq!(
            parser.expr(),
            Ok(Expr::BinaryOp(
                BinaryOp::Add,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
    }
}
