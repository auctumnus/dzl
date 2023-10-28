use lasso::Spur;

use super::{
    terminal::{Block, Terminal},
    Parser,
};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
    // As,
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
    UnaryOp(UnaryOp, Box<Expr>),
    /// `expr(expr, expr, ...)`
    Call(Box<Expr>, Vec<Expr>),
    /// `expr[index]`
    Index(Box<Expr>, Box<Expr>),
    /// `expr.member`
    Member(Box<Expr>, Spur),
    /// `{ stmt; stmt; ...; (expr) }`
    /// TODO: consider if Vec<Stmt> should be a SmallVec
    /// i.e. is the average block <= 8 statements?
    If(Box<Expr>, Block, Option<Box<Expr>>),
    While(Box<Expr>, Block),
    // TODO: for loops
    // i like rust's for loops so i probably need to figure out the whole pattern
    // business
}

impl Parser<'_> {
    fn member(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            let expr = s.terminal().map(Expr::Terminal)?;
            s.skip_whitespace();
            s.match_char('.')?;
            s.skip_whitespace();
            let member = s.ident()?;
            Ok(Expr::Member(Box::new(expr), member))
        })
        .or_else(|_: String| self.terminal().map(Expr::Terminal))
    }
    fn index(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            let expr = s.member()?;
            s.skip_whitespace();
            s.match_char('[')?;
            s.skip_whitespace();
            let index = s.expr()?;
            s.skip_whitespace();
            s.match_char(']')?;
            Ok(Expr::Index(Box::new(expr), Box::new(index)))
        })
        .or_else(|_: String| self.member())
    }
    fn call(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            let expr = s.index()?;
            s.skip_whitespace();
            s.match_char('(')?;
            let args = s.delimited(',', Parser::expr)?;
            s.match_char(')')?;
            Ok(Expr::Call(Box::new(expr), args))
        })
        .or_else(|_: String| self.index())
    }

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
            let expr = s.call()?;
            if let Some(op) = op {
                Ok(Expr::UnaryOp(op, Box::new(expr)))
            } else {
                Ok(expr)
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
                        let op = if s.peek() == Some('*') {
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
                if s.peek() == Some('=') {
                    s.next();
                    Ok(BinaryOp::Leq)
                } else {
                    Ok(BinaryOp::Lt)
                }
            }
            Some('>') => {
                if s.peek() == Some('=') {
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
                if s.peek() == Some('=') {
                    s.next();
                    Ok(BinaryOp::Eq)
                } else {
                    Err("expected =")
                }
            }
            Some('!') => {
                if s.peek() == Some('=') {
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
    fn definitely_if_expr(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            s.match_str("if")?;
            s.expect_whitespace()?;
            let cond = s.expr()?;
            s.skip_whitespace();
            let then = s.block()?;
            s.skip_whitespace();
            let else_ = if s.peek() == Some('e') {
                s.match_str("else")?;
                s.skip_whitespace();
                let expr = s
                    .definitely_if_expr()
                    .or_else(|_| Ok::<Expr, String>(Expr::Terminal(Terminal::Block(s.block()?))))?;
                Some(Box::new(expr))
            } else {
                None
            };
            Ok(Expr::If(Box::new(cond), then, else_))
        })
    }

    pub fn if_expr(&mut self) -> Result<Expr, String> {
        self.definitely_if_expr()
            .or_else(|_: String| self.equality())
    }
    pub fn while_expr(&mut self) -> Result<Expr, String> {
        self.lexeme(|s| {
            s.match_str("while")?;
            s.expect_whitespace()?;
            let cond = s.expr()?;
            s.skip_whitespace();
            let block = s.block()?;
            Ok(Expr::While(Box::new(cond), block))
        })
        .or_else(|_: String| self.if_expr())
    }
    pub fn expr(&mut self) -> Result<Expr, String> {
        self.while_expr()
    }
}

#[cfg(test)]
mod test {
    use crate::{interner::get_or_intern, parse::statements::Stmt};

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
    fn if_expr() {
        let mut parser = Parser::from("if x { 1 }");
        assert_eq!(
            parser.if_expr(),
            Ok(Expr::If(
                Box::new(Expr::Terminal(Terminal::Ident(get_or_intern("x")))),
                Block {
                    statements: vec![],
                    expr: Some(Box::new(Expr::Terminal(Terminal::Int(1))))
                },
                None
            ))
        );
    }
    #[test]
    fn block() {
        let mut parser = Parser::from("{ 1; }");
        assert_eq!(
            parser.block(),
            Ok(Block {
                statements: vec![Stmt::Statement(Expr::Terminal(Terminal::Int(1)))],
                expr: None
            })
        );

        let mut parser = Parser::from(
            r"{
            1;
            2; 
        }",
        );
        assert_eq!(
            parser.block(),
            Ok(Block {
                statements: vec![
                    Stmt::Statement(Expr::Terminal(Terminal::Int(1))),
                    Stmt::Statement(Expr::Terminal(Terminal::Int(2)))
                ],
                expr: None
            })
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
                Box::new(Expr::Terminal(Terminal::Ident(get_or_intern("x")))),
                Block {
                    statements: vec![],
                    expr: Some(Box::new(Expr::Terminal(Terminal::Int(1))))
                },
                Some(Box::new(Expr::Terminal(Terminal::Block(Block {
                    statements: vec![],
                    expr: Some(Box::new(Expr::Terminal(Terminal::Int(2))))
                })))),
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
            Ok(Expr::UnaryOp(
                UnaryOp::Neg,
                Box::new(Expr::Terminal(Terminal::Int(1)))
            ))
        );
        let mut parser = Parser::from("!1");
        assert_eq!(
            parser.expr(),
            Ok(Expr::UnaryOp(
                UnaryOp::Not,
                Box::new(Expr::Terminal(Terminal::Int(1)))
            ))
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

    #[test]
    fn index() {
        let mut parser = Parser::from("x[0]");
        assert_eq!(
            parser.expr(),
            Ok(Expr::Index(
                Box::new(Expr::Terminal(Terminal::Ident(get_or_intern("x")))),
                Box::new(Expr::Terminal(Terminal::Int(0)))
            ))
        );
    }

    #[test]
    fn call() {
        let mut parser = Parser::from("x(0)");
        assert_eq!(
            parser.expr(),
            Ok(Expr::Call(
                Box::new(Expr::Terminal(Terminal::Ident(get_or_intern("x")))),
                vec![Expr::Terminal(Terminal::Int(0))]
            ))
        );

        let mut parser = Parser::from("x(0, 1)");
        assert_eq!(
            parser.expr(),
            Ok(Expr::Call(
                Box::new(Expr::Terminal(Terminal::Ident(get_or_intern("x")))),
                vec![
                    Expr::Terminal(Terminal::Int(0)),
                    Expr::Terminal(Terminal::Int(1))
                ]
            ))
        );

        let mut parser = Parser::from("foreachint([1, 2, 3, 4], (n) => stdout.println(n * 2))");
        assert!(parser.expr().is_ok());
    }

    #[test]
    fn member() {
        let mut parser = Parser::from("x.y");
        assert_eq!(
            parser.expr(),
            Ok(Expr::Member(
                Box::new(Expr::Terminal(Terminal::Ident(get_or_intern("x")))),
                get_or_intern("y")
            ))
        );
    }

    #[test]
    fn while_expr() {
        let mut parser = Parser::from("while x { 1 }");
        assert_eq!(
            parser.while_expr(),
            Ok(Expr::While(
                Box::new(Expr::Terminal(Terminal::Ident(get_or_intern("x")))),
                Block {
                    statements: vec![],
                    expr: Some(Box::new(Expr::Terminal(Terminal::Int(1))))
                },
            ))
        );
    }
}
