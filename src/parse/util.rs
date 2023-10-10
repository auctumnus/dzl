use crate::parse::Parser;

impl Parser<'_> {
    /// Returns the next character in the source, checking the buffer first and
    /// also pushing to the current lexeme.
    pub fn next(&mut self) -> Option<char> {
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
                Err(format!("expected {c}, found {c2}"))
            }
        } else {
            Err("unexpected end of file".to_string())
        }
    }
    pub fn match_str<'a>(&mut self, string: &'a str) -> Result<&'a str, String> {
        self.lexeme(|s| {
            for c in string.chars() {
                if let Some(c2) = s.peek() {
                    if c != c2 {
                        return Err(format!("expected {c}, found {c2}"));
                    }
                    s.next();
                } else {
                    return Err("unexpected end of file".to_string());
                }
            }
            Ok(string)
        })
    }

    pub fn skip_whitespace(&mut self) {
        self.next_while(|c| c.is_ascii_whitespace());
    }
    pub fn expect_whitespace(&mut self) -> Result<(), &'static str> {
        if self.next_while(|c| c.is_ascii_whitespace()).is_empty() {
            Err("expected whitespace")
        } else {
            Ok(())
        }
    }

    pub fn match_to<'a, T>(&mut self, options: &'a Vec<(&'static str, T)>) -> Result<&'a T, ()> {
        for (string, value) in options {
            if let Ok(_) = self.match_str(string) {
                return Ok(value);
            }
        }
        Err(())
    }
}

#[cfg(test)]
mod test {
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
    fn match_str() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.match_str("xyz"), Ok("xyz"));
        assert_eq!(parser.pos, 3);
        let mut parser = Parser::from("yyyy");
        assert_eq!(
            parser.match_str("xxxx"),
            Err("expected x, found y".to_string())
        );
        assert_eq!(parser.pos, 0);
        let mut parser = Parser::from("xyz");
        assert_eq!(
            parser.match_str("xyzw"),
            Err("unexpected end of file".to_string())
        );
    }

    #[test]
    fn expect_whitespace() {
        let mut parser = Parser::from("xyz");
        assert_eq!(parser.expect_whitespace(), Err("expected whitespace"));
        assert_eq!(parser.pos, 0);
        let mut parser = Parser::from("   xyz");
        assert_eq!(parser.expect_whitespace(), Ok(()));
        assert_eq!(parser.pos, 3);
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
}
