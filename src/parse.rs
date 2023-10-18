mod expr;
mod lexeme;
mod terminal;
mod types;
mod util;

use lasso::Rodeo;
use std::{iter::Peekable, str::Chars};

use self::expr::Stmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub pos: usize,
    pub row: usize,
    pub column: usize,
}

pub struct Parser<'src> {
    position: Position,
    src: Peekable<Chars<'src>>,
    /// Buffer of characters that have been read but not yet consumed.
    /// TODO: Try to make this into a string for memory savings. At the moment,
    /// grabbing the first/last character as a peek is not particularly doable.
    buffer: Vec<char>,
    /// String interner.
    rodeo: Rodeo,
    /// Stack of (start, lexeme) pairs.
    lexeme_stack: Vec<(Position, String)>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(value: &'a str) -> Self {
        Parser::new(value.chars().peekable())
    }
}

impl Parser<'_> {
    pub fn new(src: Peekable<Chars<'_>>) -> Parser<'_> {
        Parser {
            position: Position {
                pos: 0,
                row: 0,
                column: 0,
            },
            src,
            buffer: Vec::new(),
            rodeo: Rodeo::new(),
            lexeme_stack: Vec::new(),
        }
    }

    pub fn program(&mut self) -> (Vec<Stmt>, Vec<String>) {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        self.skip_whitespace();

        loop {
            match self.statement() {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(error) => {
                    errors.push(error);
                    self.synchronize();
                }
            }
            self.skip_whitespace();
            if self.at_end() {
                break;
            }
        }

        (statements, errors)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn function_decl() {
        let program = r#"let foo = (x: int) => x * 2"#;
        let mut parser = Parser::from(program);
        let (_, errors) = parser.program();
        assert_eq!(errors, Vec::<String>::new());

        let program = r#"let foo = (x: int) -> int => x * 2"#;
        let mut parser = Parser::from(program);
        let (_, errors) = parser.program();
        assert_eq!(errors, Vec::<String>::new());

        let program = r#"let baz = (x: int) -> int => {
            return x * 2
        }"#;
        let mut parser = Parser::from(program);
        let (_, errors) = parser.program();
        assert_eq!(errors, Vec::<String>::new());

        let program = "(n) => stdout.println(n * 2)";
        let mut parser = Parser::from(program);
        let (_, errors) = parser.program();
        assert_eq!(errors, Vec::<String>::new());

        let program = "[1, 2, 3, 4]";
        let mut parser = Parser::from(program);
        let (_, errors) = parser.program();
        assert_eq!(errors, Vec::<String>::new());

        let program = r#"foreachint([1, 2, 3, 4], (n) => stdout.println(n * 2))"#;
        let mut parser = Parser::from(program);
        let (_, errors) = parser.program();
        assert_eq!(errors, Vec::<String>::new());
    }

    #[test]
    fn whole_program() {
        let program = r#"let foo = (x: int) => x * 2
        let foo = (x: int) -> int => x * 2
        let baz = (x: int) -> int => {
            return x * 2
        }
        const main = () => {
            foreachint([1, 2, 3, 4], (n) => stdout.println(n * 2))
        }
        "#;

        let mut parser = Parser::from(program);
        let (statements, errors) = parser.program();
        assert_eq!(errors, Vec::<String>::new());
    }
}
