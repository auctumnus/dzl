use crate::parse::Parser;

impl Parser<'_> {
    /// Returns the next character in the source, checking the buffer first and
    /// also pushing to the current lexeme.
    pub fn next(&mut self) -> Option<char> {
        if let Some(c) = self.buffer.pop() {
            if let Some(lexeme) = self.lexeme_stack.last_mut() {
                lexeme.1.push(c);
            }
            self.position.pos += 1;
            self.position.column += 1;
            if c == '\n' {
                self.position.column = 0;
                self.position.row += 1;
            }
            Some(c)
        } else {
            let c = self.src.next();
            if let Some(c) = c {
                self.position.pos += 1;
                self.position.column += 1;
                if c == '\n' {
                    self.position.column = 0;
                    self.position.row += 1;
                }
                if let Some(lexeme) = self.lexeme_stack.last_mut() {
                    lexeme.1.push(c);
                }
            }
            c
        }
    }
    pub fn peek(&mut self) -> Option<char> {
        self.buffer.last().or_else(|| self.src.peek()).copied()
    }
    pub fn at_end(&mut self) -> bool {
        self.peek().is_none()
    }
    pub fn next_while<F>(&mut self, mut f: F) -> String
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
    pub fn match_char(&mut self, c: char) -> Result<char, String> {
        if let Some(c2) = self.peek() {
            if c == c2 {
                self.next();
                Ok(c)
            } else {
                Err(self.make_error(&format!("expected {c}, found {c2}")))
            }
        } else {
            Err(self.make_error("unexpected end of file"))
        }
    }
    pub fn match_str<'a>(&mut self, string: &'a str) -> Result<&'a str, String> {
        self.lexeme(|s| {
            for c in string.chars() {
                s.match_char(c)?;
            }
            Ok(string)
        })
    }

    pub fn skip_whitespace(&mut self) {
        self.next_while(|c| c.is_ascii_whitespace() || c == '\n');
    }
    pub fn expect_whitespace(&mut self) -> Result<(), String> {
        if self.next_while(|c| c.is_ascii_whitespace()).is_empty() {
            Err(self.make_error("expected whitespace"))
        } else {
            Ok(())
        }
    }

    pub fn match_to<'a, T>(&mut self, options: &'a Vec<(&'static str, T)>) -> Result<&'a T, ()> {
        for (string, value) in options {
            if self.match_str(string).is_ok() {
                return Ok(value);
            }
        }
        Err(())
    }

    pub fn delimited<T>(
        &mut self,
        by: char,
        mut f: impl FnMut(&mut Self) -> Result<T, String>,
    ) -> Result<Vec<T>, String> {
        self.lexeme(|s| {
            let mut items = Vec::new();
            loop {
                if let Ok(item) = f(s) {
                    items.push(item);
                } else {
                    break;
                }
                if s.peek() == Some(by) {
                    s.next();
                    s.skip_whitespace();
                } else {
                    break;
                }
            }
            Ok(items)
        })
    }

    pub fn surrounded<T>(
        &mut self,
        by: (char, char),
        mut f: impl FnMut(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
        self.lexeme(|s| {
            s.match_char(by.0)?;
            s.skip_whitespace();
            let item = f(s)?;
            s.skip_whitespace();
            s.match_char(by.1)?;
            Ok(item)
        })
    }

    pub fn synchronize(&mut self) {
        self.next_while(|c| c != '\n');
        self.next();
        self.skip_whitespace();
    }

    pub fn make_error(&mut self, message: &str) -> String {
        let row = self.position.row;
        let column = self.position.column;

        let last_lexeme_content = self.lexeme_stack.last().map(|(_, lexeme)| lexeme);

        format!("[{row}:{column}] {message} ({last_lexeme_content:#?})")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn next() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.next(), Some('x'));
        assert_eq!(parser.position.pos, 1);
        assert_eq!(parser.next(), Some('y'));
        assert_eq!(parser.position.pos, 2);
        assert_eq!(parser.next(), Some('z'));
        assert_eq!(parser.position.pos, 3);
        assert_eq!(parser.next(), None);
        assert_eq!(parser.position.pos, 3);
    }

    #[test]
    fn peek() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.peek(), Some('x'));
        assert_eq!(parser.position.pos, 0);
    }
    #[test]
    fn match_str() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.match_str("xyz"), Ok("xyz"));
        assert_eq!(parser.position.pos, 3);
        let mut parser = Parser::from("yyyy");
        assert!(parser.match_str("xxxx").is_err());
        assert_eq!(parser.position.pos, 0);
        let mut parser = Parser::from("xyz");
        assert!(parser.match_str("xyzw").is_err());
    }

    #[test]
    fn expect_whitespace() {
        let mut parser = Parser::from("xyz");
        assert!(parser.expect_whitespace().is_err());
        assert_eq!(parser.position.pos, 0);
        let mut parser = Parser::from("   xyz");
        assert_eq!(parser.expect_whitespace(), Ok(()));
        assert_eq!(parser.position.pos, 3);
    }

    #[test]
    fn at_end() {
        let mut parser = Parser::from("xyz");
        assert!(!parser.at_end());
        parser.next();
        parser.next();
        parser.next();
        assert!(parser.at_end());
    }

    #[test]
    fn match_to() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.match_to(&vec![("xyz", 1), ("abc", 2)]), Ok(&1));
        assert_eq!(parser.position.pos, 3);
        let mut parser = Parser::from("abc");
        assert_eq!(parser.match_to(&vec![("xyz", 1), ("abc", 2)]), Ok(&2));
        assert_eq!(parser.position.pos, 3);
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.match_to(&vec![("abc", 1), ("def", 2)]), Err(()));
        assert_eq!(parser.position.pos, 0);
    }

    #[test]
    fn row_and_column() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.position.row, 0);
        assert_eq!(parser.position.column, 0);
        parser.next();
        assert_eq!(parser.position.row, 0);
        assert_eq!(parser.position.column, 1);
        parser.next();
        assert_eq!(parser.position.row, 0);
        assert_eq!(parser.position.column, 2);
        parser.next();
        assert_eq!(parser.position.row, 0);
        assert_eq!(parser.position.column, 3);
        parser.next();
        assert_eq!(parser.position.row, 0);
        assert_eq!(parser.position.column, 3);
        let mut parser = Parser::from("x\ny\nz");
        assert_eq!(parser.position.row, 0);
        assert_eq!(parser.position.column, 0);
        parser.next();
        assert_eq!(parser.position.row, 0);
        assert_eq!(parser.position.column, 1);
        parser.next();
        assert_eq!(parser.position.row, 1);
        assert_eq!(parser.position.column, 0);
        parser.next();
        assert_eq!(parser.position.row, 1);
        assert_eq!(parser.position.column, 1);
        parser.next();
        assert_eq!(parser.position.row, 2);
        assert_eq!(parser.position.column, 0);
        parser.next();
        assert_eq!(parser.position.row, 2);
        assert_eq!(parser.position.column, 1);
        parser.next();
        assert_eq!(parser.position.row, 2);
        assert_eq!(parser.position.column, 1);
        parser.next();
        assert_eq!(parser.position.row, 2);
        assert_eq!(parser.position.column, 1);
    }
}
