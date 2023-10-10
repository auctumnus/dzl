mod expr;
mod lexeme;
mod terminal;
mod types;
mod util;

use lasso::Rodeo;
use std::{iter::Peekable, str::Chars};

pub struct Parser<'src> {
    pos: usize,
    src: Peekable<Chars<'src>>,
    /// Buffer of characters that have been read but not yet consumed.
    /// TODO: Try to make this into a string for memory savings. At the moment,
    /// grabbing the first/last character as a peek is not particularly doable.
    buffer: Vec<char>,
    /// String interner.
    rodeo: Rodeo,
    /// Stack of (start, lexeme) pairs.
    lexeme_stack: Vec<(usize, String)>,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(value: &'a str) -> Self {
        Parser::new(value.chars().peekable())
    }
}

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
}
