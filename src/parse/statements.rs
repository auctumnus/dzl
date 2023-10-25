use lasso::Spur;

use super::{Parser, expr::{Expr, BinaryOp}, types::Type};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    Type(Spur, Type),
    /// `expr`
    Expr(Expr),
    Continue,
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
}

impl Parser<'_> {
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
                Some('t') => {
                    s.match_str("type")?;
                    s.skip_whitespace();
                    let ident = s.ident()?;
                    s.skip_whitespace();
                    s.match_char('=')?;
                    s.skip_whitespace();
                    let r#type = s.r#type()?;
                    return Ok(Stmt::Type(ident, r#type));
                }
                _ => return Err("expected let or const".to_string()),
            };
            s.skip_whitespace();
            let ident = s.ident()?;
            s.skip_whitespace();
            let r#type = if s.peek() == Some(':') {
                s.next();
                s.skip_whitespace();
                let r#type = s.r#type();
                s.skip_whitespace();
                r#type.ok()
            } else {
                None
            };
            if s.peek() == Some('=') {
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
                    if s.peek() == Some('*') {
                        s.next();
                        BinaryOp::Pow
                    } else {
                        BinaryOp::Mul
                    }
                }
                Some('<') => {
                    if s.peek() == Some('<') {
                        s.next();
                        BinaryOp::Shl
                    } else {
                        return Err("expected shift left assign".to_string());
                    }
                }
                Some('>') => {
                    if s.peek() == Some('>') {
                        s.next();
                        BinaryOp::Shr
                    } else {
                        return Err("expected shift right assign".to_string());
                    }
                }
                Some('&') => {
                    if s.peek() == Some('&') {
                        s.next();
                        BinaryOp::And
                    } else {
                        BinaryOp::BitwiseAnd
                    }
                }
                Some('|') => {
                    if s.peek() == Some('|') {
                        s.next();
                        BinaryOp::Or
                    } else {
                        BinaryOp::BitwiseOr
                    }
                }
                Some(_) => return Err("expected assignment operator".to_string()),
                None => return Err("unexpected end of file".to_string()),
            };
            if s.next() == Some('=') {
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
            s.skip_whitespace();
            let expr = s.expr()?;
            Ok(Stmt::Assignment(ident, op, expr))
        })
    }
    fn keyword_statement(&mut self) -> Result<Stmt, String> {
        let keyword_statements = &vec![("return", 0), ("break", 1), ("continue", 2)];
        self.lexeme(|s| {
            if let Ok(keyword) = s.match_to(keyword_statements).copied() {
                if keyword == 2 { // continue
                    if s.peek() == Some(';') {
                        s.next();
                    }
                    Ok(Stmt::Continue)
                } else {
                    s.skip_whitespace();
                    let expr = if let Ok(expr) = s.expr() {
                        Some(Box::new(expr))
                    } else {
                        None
                    };
                    if s.peek() == Some(';') {
                        s.next();
                    }
                    if keyword == 0 {
                        Ok(Stmt::Return(expr))
                    } else {
                        Ok(Stmt::Break(expr))
                    }
                }
            } else {
                Err("expected keyword statement".to_string())
            }
        })
    }

    pub fn statement(&mut self) -> Result<Stmt, String> {
        self.lexeme(|s| {
            let statement = if let Ok(kwd) = s.keyword_statement() {
                Ok(kwd)
            } else if let Ok(declaration) = s.declaration() {
                Ok(declaration)
            } else if let Ok(assignment) = s.assignment() {
                Ok(assignment)
            } else {
                // expr needs special handling, because blocks can end with an
                // expression statement, and this becomes their return value
                // therefore this block has an early return so that we don't
                // accidentally handle a semicolon twice and allow for ";;"
                let expr = s.expr()?;
                if s.peek() == Some(';') {
                    s.next();
                    s.skip_whitespace();
                    return Ok(Stmt::Statement(expr))
                }
                s.skip_whitespace();
                return Ok(Stmt::Expr(expr))
            };
            if s.peek() == Some(';') {
                s.next();
            }
            statement
        })
    }
}

#[cfg(test)]
mod test {
    use crate::parse::terminal::Terminal;

    use super::*;
    #[test]
    fn assignment() {
        let kinds = vec![
            ("+", BinaryOp::Add),
            ("-", BinaryOp::Sub),
            ("*", BinaryOp::Mul),
            ("/", BinaryOp::Div),
            ("%", BinaryOp::Mod),
            ("**", BinaryOp::Pow),
            ("<<", BinaryOp::Shl),
            (">>", BinaryOp::Shr),
            ("&", BinaryOp::BitwiseAnd),
            ("|", BinaryOp::BitwiseOr),
            ("^", BinaryOp::BitwiseXor),
            ("&&", BinaryOp::And),
            ("||", BinaryOp::Or),
        ];

        let mut parser = Parser::from("x = 0;");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Assignment(
                parser.rodeo.get_or_intern("x"),
                None,
                Expr::Terminal(Terminal::Int(0))
            ))
        );
        for (op, kind) in kinds {
            let src = format!("x {op}= 0;");
            let mut parser = Parser::from(src.as_str());
            assert_eq!(
                parser.statement(),
                Ok(Stmt::Assignment(
                    parser.rodeo.get_or_intern("x"),
                    Some(kind),
                    Expr::Terminal(Terminal::Int(0))
                ))
            );
        }

        assert!(Parser::from("x =").assignment().is_err());
        assert!(Parser::from("+").assignment_operator().is_err());
        assert!(Parser::from("").assignment_operator().is_err());
        assert!(Parser::from("a").assignment_operator().is_err());
        assert!(Parser::from(">=").assignment_operator().is_err());
        assert!(Parser::from("<=").assignment_operator().is_err());
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
    fn break_stmt() {
        assert_eq!(
            Parser::from("break").statement(),
            Ok(Stmt::Break(None))
        );
        assert_eq!(
            Parser::from("break;").statement(),
            Ok(Stmt::Break(None))
        );
        assert_eq!(
            Parser::from("break 1").statement(),
            Ok(Stmt::Break(Some(Box::new(Expr::Terminal(Terminal::Int(1))))))
        );
    }

    #[test]
    fn continue_stmt() {
        assert_eq!(
            Parser::from("continue").statement(),
            Ok(Stmt::Continue)
        );
        assert_eq!(
            Parser::from("continue;").statement(),
            Ok(Stmt::Continue)
        );
    }

    #[test]
    fn return_stmt() {
        assert_eq!(
            Parser::from("return").statement(),
            Ok(Stmt::Return(None))
        );
        assert_eq!(
            Parser::from("return;").statement(),
            Ok(Stmt::Return(None))
        );
        assert_eq!(
            Parser::from("return 1").statement(),
            Ok(Stmt::Return(Some(Box::new(Expr::Terminal(Terminal::Int(1))))))
        );
    }
    #[test]
    fn type_declaration() {
        let mut parser = Parser::from("type T = int");
        assert_eq!(
            parser.statement(),
            Ok(Stmt::Type(
                parser.rodeo.get_or_intern("T"),
                Type::Ident(parser.rodeo.get_or_intern("int"), vec![])
            ))
        );
    }
}