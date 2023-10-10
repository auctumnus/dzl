use crate::parse::Parser;

const DEFAULT_LEXEME_CAPACITY: usize = 32;

impl Parser<'_> {
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
    pub fn lexeme<F, T, E>(&mut self, mut f: F) -> Result<T, E>
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
}

#[cfg(test)]
mod test {
    use super::*;
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
}
