use lasso::Spur;

use super::{expr::Expr, Parser};

#[derive(Debug, PartialEq, Clone)]
pub enum TemplateElement {
    String(Spur),
    Expression(Expr),
}
#[derive(Debug, PartialEq, Clone)]
pub enum DzStr {
    Simple(Spur),
    Template(Vec<TemplateElement>),
}

fn is_quote(c: char) -> bool {
    c == '"' || c == '\'' || c == '`'
}

impl Parser<'_> {
    fn hex_digit(&mut self) -> Result<char, String> {
        match self.peek() {
            Some(c) if c.is_ascii_hexdigit() => {
                self.next();
                Ok(c)
            }
            _ => Err(self.make_error("expected hex digit")),
        }
    }

    fn escape(&mut self) -> Result<char, String> {
        self.lexeme(|s| match s.next() {
            Some('\'') => Ok('\''),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('a') => Ok(0x07 as char),
            Some('e') => Ok(0x1b as char),
            Some('\\') => Ok('\\'),
            Some('x') => {
                let top = s.hex_digit()?;
                let bottom = s.hex_digit()?;
                let byte: u8 = format!("{top}{bottom}")
                    .parse()
                    .map_err(|_| s.make_error("invalid hex escape sequence"))?;
                Ok(byte as char)
            }
            Some('u') => {
                s.match_char('{')?;
                let digits = s.next_while(|c| c.is_ascii_hexdigit());
                let codepoint: u32 = u32::from_str_radix(&digits, 16)
                    .map_err(|_| s.make_error("invalid unicode escape sequence"))?;
                s.match_char('}')?;
                std::char::from_u32(codepoint)
                    .ok_or_else(|| s.make_error(&format!("invalid unicode codepoint: {codepoint}")))
            }
            Some(c) => Ok(c),
            None => Err(s.make_error("expected escape character")),
        })
    }

    fn interpolation(&mut self) -> Result<Expr, String> {
        self.surrounded(('{', '}'), Parser::expr)
    }

    fn template_element(&mut self) -> Result<TemplateElement, String> {
        self.lexeme(|s| {
            if let Some('{') = s.peek() {
                let expr = s.interpolation()?;
                Ok(TemplateElement::Expression(expr))
            } else {
                let mut string = String::with_capacity(8);
                loop {
                    match s.peek() {
                        Some('`' | '{') => break,
                        Some('\\') => string.push(s.escape()?),
                        Some(c) => string.push(c),
                        None => return Err(s.make_error("unexpected end of file")),
                    }
                    s.next();
                }
                let string = s.rodeo.get_or_intern(string);
                Ok(TemplateElement::String(string))
            }
        })
    }

    fn template_string(&mut self) -> Result<Vec<TemplateElement>, String> {
        self.lexeme(|s| {
            let mut elements = Vec::new();
            loop {
                if let Ok(element) = s.template_element() {
                    elements.push(element);
                    if s.peek() == Some('`') {
                        s.next();
                        break;
                    }
                } else {
                    return Err(s.make_error("expected template element"));
                }
            }
            Ok(elements)
        })
    }

    fn simple_string(&mut self, kind: char) -> Result<Spur, String> {
        self.lexeme(|s| {
            let mut string = String::with_capacity(8);

            loop {
                match s.next() {
                    Some(c) if c == kind => break,
                    Some('\\') => string.push(s.escape()?),
                    Some('\n') => return Err(s.make_error("unexpected newline")),
                    Some(c) => string.push(c),
                    None => return Err(s.make_error("unexpected end of file")),
                }
            }

            let string = s.rodeo.get_or_intern(string);

            Ok(string)
        })
    }

    pub fn string(&mut self) -> Result<DzStr, String> {
        self.lexeme(|s| {
            let kind = match s.next() {
                Some(c) if is_quote(c) => c,
                _ => return Err(s.make_error("expected string")),
            };

            if kind == '`' {
                let string = s.template_string()?;
                if string.len() == 1 {
                    // it's fine to clone that cause a Spur is just a `usize`
                    if let TemplateElement::String(string) = string[0].clone() {
                        return Ok(DzStr::Simple(string));
                    }
                }
                Ok(DzStr::Template(string))
            } else {
                Ok(DzStr::Simple(s.simple_string(kind)?))
            }
        })
    }
}

#[cfg(test)]
mod test {
    use crate::parse::terminal::Terminal;

    use super::*;
    #[test]
    fn string() {
        let mut parser = Parser::from("\"hello world\"");
        assert_eq!(
            parser.string(),
            Ok(DzStr::Simple(parser.rodeo.get_or_intern("hello world")))
        );
        assert_eq!(parser.position.pos, 13);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());

        assert!(Parser::from("\"hello\nworld\"").string().is_err());
    }

    #[test]
    fn simple_template_string() {
        let mut parser = Parser::from("`hello world`");
        assert_eq!(
            parser.string(),
            Ok(DzStr::Simple(parser.rodeo.get_or_intern("hello world")))
        );
    }

    #[test]
    fn template_string() {
        let mut parser = Parser::from("`hello {x}`");
        assert_eq!(
            parser.string(),
            Ok(DzStr::Template(vec![
                TemplateElement::String(parser.rodeo.get_or_intern("hello ")),
                TemplateElement::Expression(Expr::Terminal(Terminal::Ident(
                    parser.rodeo.get_or_intern("x")
                )))
            ]))
        );

        let mut parser = Parser::from("`{x}`");
        assert_eq!(
            parser.string(),
            Ok(DzStr::Template(vec![TemplateElement::Expression(
                Expr::Terminal(Terminal::Ident(parser.rodeo.get_or_intern("x")))
            )]))
        );
    }

    #[test]
    fn simple_escapes() {
        let mut parser = Parser::from(r#""\n\r\t\"\'\a\e\\\x00""#);
        assert_eq!(
            parser.string(),
            Ok(DzStr::Simple(
                parser.rodeo.get_or_intern("\n\r\t\"\'\x07\x1b\\\x00")
            ))
        );
    }

    #[test]
    fn unicode_escapes() {
        // copilot handed me this emoji. i am cackling
        let mut parser = Parser::from(r#""\u{1f600}\u{1f600}\u{1f600}""#);
        assert_eq!(
            parser.string(),
            Ok(DzStr::Simple(parser.rodeo.get_or_intern("😀😀😀")))
        );

        let mut parser = Parser::from("`\u{1f600}\\\\`");
        assert_eq!(
            parser.string(),
            Ok(DzStr::Simple(parser.rodeo.get_or_intern("😀\\")))
        );
    }

    #[test]
    fn bad_hex_escape() {
        assert!(Parser::from(r#""\x""#).string().is_err());
        assert!(Parser::from(r#""\x0""#).string().is_err());
        assert!(Parser::from(r#""\x0g""#).string().is_err());
    }
    #[test]
    fn empty_escape() {
        assert!(Parser::from(r#""\u{}""#).string().is_err());
        println!("{:?}", Parser::from("\\").escape());
        assert!(Parser::from("").escape().is_err());
    }

    #[test]
    fn unterminated() {
        assert!(Parser::from("\"").string().is_err());
        assert!(Parser::from("`").string().is_err());
    }
}
