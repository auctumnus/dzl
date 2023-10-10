use lasso::Spur;

use super::{terminal::Terminal, types::Type, Parser};

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
    Shl,
    Shr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    As,
}
/*
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
}*/

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
    BitwiseNot,
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
    /// `{ stmt; stmt; ...; (expr) }`
    /// TODO: consider if Vec<Stmt> should be a SmallVec
    /// i.e. is the average block <= 8 statements?
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
    Declaration(DeclarationKind, Option<Type>, Spur, Expr),
    /// `ident = expr`, `ident *= expr`
    Assignment(Spur, Option<BinaryOp>, Expr),
    /// `expr;`
    /// different from Expr because this has no return type
    Statement(Expr),
    /// `expr`
    Expr(Expr),
}

impl Parser<'_> {
    fn unary_op(&mut self) -> Result<UnaryOp, String> {
        self.lexeme(|s| {
            let op = match s.next() {
                Some('-') => UnaryOp::Neg,
                Some('!') => UnaryOp::Not,
                Some('~') => UnaryOp::BitwiseNot,
                Some('n') => {
                    s.match_str("not")?;
                    UnaryOp::Not
                }
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
        self.lexeme(|s| {
            let mut expr = s.unary()?;
            while let Some(op) = s.peek() {
                match op {
                    '/' | '%' => {
                        s.next();
                        s.skip_whitespace();
                        let rhs = s.unary()?;
                        expr = Expr::BinaryOp(
                            match op {
                                '/' => BinaryOp::Div,
                                '%' => BinaryOp::Mod,
                                _ => unreachable!(),
                            },
                            Box::new(expr),
                            Box::new(rhs),
                        );
                    }
                    '*' => {
                        s.next();
                        let op = if let Some('*') = s.peek() {
                            s.next();
                            BinaryOp::Pow
                        } else {
                            BinaryOp::Mul
                        };
                        s.skip_whitespace();
                        let rhs = s.unary()?;
                        expr = Expr::BinaryOp(op, Box::new(expr), Box::new(rhs));
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
    pub fn expr(&mut self) -> Result<Expr, String> {
        self.equality()
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
            let r#type = if let Some(':') = s.peek() {
                s.next();
                s.skip_whitespace();
                let r#type = s.r#type();
                s.skip_whitespace();
                r#type.ok()
            } else {
                None
            };
            if let Some('=') = s.peek() {
                s.next();
            } else {
                return Err("expected =".to_string());
            }
            s.skip_whitespace();
            let expr = s.expr()?;
            Ok(Stmt::Declaration(kind, r#type, ident, expr))
        })
    }
    fn assignment_operator(&mut self) -> Result<Option<BinaryOp>, String> {
        self.lexeme(|s| {
            let op = match s.next() {
                Some('=') => return Ok(None),
                Some('+') => BinaryOp::Add,
                Some('-') => BinaryOp::Sub,
                Some('/') => BinaryOp::Div,
                Some('%') => BinaryOp::Mod,
                Some('^') => BinaryOp::BitwiseXor,
                Some('*') => {
                    if let Some('*') = s.peek() {
                        s.next();
                        BinaryOp::Pow
                    } else {
                        BinaryOp::Mul
                    }
                }
                Some('<') => {
                    if let Some('<') = s.peek() {
                        s.next();
                        BinaryOp::Shl
                    } else {
                        return Err("expected shift left assign".to_string());
                    }
                }
                Some('>') => {
                    if let Some('>') = s.peek() {
                        s.next();
                        BinaryOp::Shr
                    } else {
                        return Err("expected shift right assign".to_string());
                    }
                }
                Some('&') => {
                    if let Some('&') = s.peek() {
                        s.next();
                        BinaryOp::And
                    } else {
                        BinaryOp::BitwiseAnd
                    }
                }
                Some('|') => {
                    if let Some('|') = s.peek() {
                        s.next();
                        BinaryOp::Or
                    } else {
                        BinaryOp::BitwiseOr
                    }
                }
                Some(_) => return Err("expected assignment operator".to_string()),
                None => return Err("unexpected end of file".to_string()),
            };
            if let Some('=') = s.next() {
                s.skip_whitespace();
                Ok(Some(op))
            } else {
                Err("expected =".to_string())
            }
        })
    }
    fn assignment(&mut self) -> Result<Stmt, String> {
        self.lexeme(|s| {
            let ident = s.ident()?;
            s.skip_whitespace();
            let op = s.assignment_operator()?;
            println!("assignment operator: {:?}", op);
            s.skip_whitespace();
            let expr = s.expr()?;
            Ok(Stmt::Assignment(ident, op, expr))
        })
    }
    pub fn statement(&mut self) -> Result<Stmt, String> {
        self.lexeme(|s| {
            let statement = if let Ok(declaration) = s.declaration() {
                Ok(declaration)
            } else if let Ok(assignment) = s.assignment() {
                Ok(assignment)
            } else {
                let expr = s.expr()?;
                if let Some(';') = s.peek() {
                    s.next();
                    Ok(Stmt::Statement(expr))
                } else {
                    Ok(Stmt::Expr(expr))
                }
            };
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

#[cfg(test)]
mod test {
    use super::*;
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
        assert_eq!(
            Parser::from("2 % 3").factor(),
            Ok(Expr::BinaryOp(
                BinaryOp::Mod,
                Box::new(Expr::Terminal(Terminal::Int(2))),
                Box::new(Expr::Terminal(Terminal::Int(3)))
            ))
        );
        assert_eq!(
            Parser::from("2 ** 3").factor(),
            Ok(Expr::BinaryOp(
                BinaryOp::Pow,
                Box::new(Expr::Terminal(Terminal::Int(2))),
                Box::new(Expr::Terminal(Terminal::Int(3)))
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
    fn comparison() {
        assert_eq!(
            Parser::from("1 < 2").comparison(),
            Ok(Expr::BinaryOp(
                BinaryOp::Lt,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
        assert_eq!(
            Parser::from("1 > 2").comparison(),
            Ok(Expr::BinaryOp(
                BinaryOp::Gt,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
        assert_eq!(
            Parser::from("1 <= 2").comparison(),
            Ok(Expr::BinaryOp(
                BinaryOp::Leq,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
        assert_eq!(
            Parser::from("1 >= 2").comparison(),
            Ok(Expr::BinaryOp(
                BinaryOp::Geq,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
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
                None,
                parser.rodeo.get_or_intern("x"),
                Expr::Terminal(Terminal::Int(0))
            ))
        );

        let mut parser = Parser::from("const x = 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Declaration(
                DeclarationKind::Const,
                None,
                parser.rodeo.get_or_intern("x"),
                Expr::Terminal(Terminal::Int(0))
            ))
        );

        let mut parser = Parser::from("let x: T = 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Declaration(
                DeclarationKind::Let,
                Some(Type::Ident(parser.rodeo.get_or_intern("T"), Vec::new())),
                parser.rodeo.get_or_intern("x"),
                Expr::Terminal(Terminal::Int(0))
            ))
        );

        assert!(Parser::from("let x").statement().is_err());
    }
    #[test]
    #[allow(clippy::too_many_lines)]
    fn assignment() {
        let mut parser = Parser::from("x = 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                None,
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x+=0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Add),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x -= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Sub),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x *= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Mul),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x /= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Div),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x %= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Mod),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x **= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Pow),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x <<= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Shl),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x >>= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Shr),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x &&= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::And),
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        let mut parser = Parser::from("x ||= 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                Some(BinaryOp::Or),
                Expr::Terminal(Terminal::Int(0))
            ))
        );

        println!("{:#?}", Parser::from("x =").statement());

        assert!(Parser::from("x =").assignment().is_err());
        assert!(Parser::from("+").assignment_operator().is_err());
        assert!(Parser::from("").assignment_operator().is_err());
        assert!(Parser::from("a").assignment_operator().is_err());
    }
    #[test]
    fn if_expr() {
        let mut parser = Parser::from("if x { 1 }");
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
                None
            ))
        );
    }
    #[test]
    fn block() {
        let mut parser = Parser::from("{ 1; }");
        assert_eq!(
            parser.block(),
            Ok(Expr::Block(
                vec![Stmt::Statement(Expr::Terminal(Terminal::Int(1)))],
                None
            ))
        );
    }
    #[test]
    fn term() {
        assert_eq!(
            Parser::from("1 + 2").term(),
            Ok(Expr::BinaryOp(
                BinaryOp::Add,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
        assert_eq!(
            Parser::from("1 - 2").term(),
            Ok(Expr::BinaryOp(
                BinaryOp::Sub,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
    }
    #[test]
    fn if_else_expr() {
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
    #[test]
    fn pow() {
        let mut parser = Parser::from("2 ** 3");
        assert_eq!(
            parser.expr(),
            Ok(Expr::BinaryOp(
                BinaryOp::Pow,
                Box::new(Expr::Terminal(Terminal::Int(2))),
                Box::new(Expr::Terminal(Terminal::Int(3)))
            ))
        );
    }
    #[test]
    fn eq() {
        let mut parser = Parser::from("1 == 2");
        assert_eq!(
            parser.expr(),
            Ok(Expr::BinaryOp(
                BinaryOp::Eq,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
        assert_eq!(
            Parser::from("1 != 2").expr(),
            Ok(Expr::BinaryOp(
                BinaryOp::Neq,
                Box::new(Expr::Terminal(Terminal::Int(1))),
                Box::new(Expr::Terminal(Terminal::Int(2)))
            ))
        );
    }
    #[test]
    fn unary() {
        let mut parser = Parser::from("-1");
        assert_eq!(
            parser.expr(),
            Ok(Expr::UnaryOp(UnaryOp::Neg, Terminal::Int(1)))
        );
        let mut parser = Parser::from("!1");
        assert_eq!(
            parser.expr(),
            Ok(Expr::UnaryOp(UnaryOp::Not, Terminal::Int(1)))
        );
    }
    // largely for code coverage
    #[test]
    fn weird_failure_states() {
        assert!(Parser::from("-").expr().is_err());
        assert!(Parser::from("").unary_op().is_err());
        assert!(Parser::from("!").equal_op().is_err());
        assert!(Parser::from("{ 1 = 2 }").block().is_err());
    }
}
