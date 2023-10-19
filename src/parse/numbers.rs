use super::Parser;

impl Parser<'_> {
    fn digits(&mut self, base: u32) -> Result<String, String> {
        self.lexeme(|s| {
            let mut digits = String::new();
            while let Some(c) = s.peek() {
                if c.is_digit(base) {
                    digits.push(c);
                    s.next();

                    let n = s.peek();

                    if n == Some('_') || n == Some('\'') {
                        s.next();
                    }
                } else {
                    break;
                }
            }
            let n = s.peek();

            if n == Some('_') || n == Some('\'') {
                return Err(s.make_error("invalid number"));
            }
            if digits.is_empty() {
                Err(s.make_error("expected digits"))
            } else {
                Ok(digits)
            }
        })
    }

    fn base(&mut self) -> u32 {
        *(self
            .match_to(&vec![
                ("0b", 2),
                ("0c", 8),
                ("0x", 16),
                ("0B", 2),
                ("0C", 8),
                ("0X", 16),
            ])
            .unwrap_or(&10))
    }

    fn integer_exponent(&mut self) -> u32 {
        self.lexeme(|s| {
            if s.peek() == Some('e') || s.peek() == Some('E') {
                s.next();
            } else {
                return Err(s.make_error("expected exponent"));
            }

            if s.peek() == Some('+') {
                s.next();
            } else if s.peek() == Some('-') {
                return Err(s.make_error("integers can only have positive exponents"));
            }

            let digits = s.digits(10)?;

            digits.parse().map_err(|_| s.make_error("invalid integer"))
        })
        .unwrap_or(0)
    }

    pub fn int(&mut self) -> Result<i64, String> {
        self.lexeme(|s| {
            let radix = s.base();
            let digits = s.digits(radix)?;
            let n =
                i64::from_str_radix(&digits, radix).map_err(|_| s.make_error("invalid integer"))?;

            let exponent = if radix == 10 { s.integer_exponent() } else { 0 };

            if s.peek() == Some('.') {
                return Err(s.make_error("expected integer, got float"));
            }

            if s.peek() == Some('e') || s.peek() == Some('E') {
                return Err(s.make_error("invalid exponent"));
            }

            let exponent = (10_i64)
                .checked_pow(exponent)
                .ok_or_else(|| s.make_error("integer overflow"))?;

            let n = n
                .checked_mul(exponent)
                .ok_or_else(|| s.make_error("integer overflow"))?;

            Ok(n)
        })
    }

    pub fn float_exponent(&mut self) -> i32 {
        self.lexeme(|s| {
            if s.peek() == Some('e') || s.peek() == Some('E') {
                s.next();
            } else {
                return Err(s.make_error("expected exponent"));
            }

            let sign = if s.peek() == Some('+') {
                s.next();
                1
            } else if s.peek() == Some('-') {
                s.next();
                -1
            } else {
                1
            };

            let n: i32 = s
                .digits(10)?
                .parse()
                .map_err(|_| s.make_error("invalid integer"))?;

            Ok(n * sign)
        })
        .unwrap_or(0)
    }

    pub fn float(&mut self) -> Result<f64, String> {
        self.lexeme(|s| {
            let mut is_definitely_float = false;

            let leading: f64 = s
                .digits(10)
                .map(|n| n.parse())
                .map_err(|_| s.make_error("invalid float"))?
                .map_err(|_| s.make_error("invalid float"))?;
            let following = if s.peek() == Some('.') {
                s.next();
                is_definitely_float = true;
                let following = s.digits(10)?;
                let following = format!("0.{following}");
                following
                    .parse()
                    .map_err(|_| s.make_error("invalid float"))?
            } else {
                0.0f64
            };

            let exponent = s.float_exponent();
            let exponent = (10_f64).powi(exponent);

            if s.peek() == Some('f') || s.peek() == Some('F') {
                is_definitely_float = true;
                s.next();
            }

            if !is_definitely_float {
                return Err(s.make_error("expected float"));
            }

            let n = leading + following;

            Ok(n * exponent)
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn int() {
        let mut parser = Parser::from("123");
        assert_eq!(parser.int(), Ok(123));
        assert_eq!(parser.position.pos, 3);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());

        assert!(Parser::from("123.").int().is_err());
    }

    #[test]
    fn int_with_separators() {
        assert_eq!(Parser::from("1_2_3").int(), Ok(123));
        assert_eq!(Parser::from("1'2'3").int(), Ok(123));
        assert!(Parser::from("1__3").int().is_err());
    }

    #[test]
    fn int_with_exponent() {
        assert_eq!(Parser::from("1e2").int(), Ok(100));
        assert_eq!(Parser::from("1e+2").int(), Ok(100));

        assert!(Parser::from("1e100").int().is_err());
        assert!(Parser::from("1e+100").int().is_err());
        assert!(Parser::from("1e").int().is_err());
        assert!(Parser::from("1e+").int().is_err());
        assert!(Parser::from("1e-").int().is_err());
        assert!(Parser::from("1e_3").int().is_err());
        assert!(Parser::from("1e+_3").int().is_err());
        assert!(Parser::from("1e-_3").int().is_err());
        assert!(Parser::from("1e-2").int().is_err());
        assert!(Parser::from("1e-2_3").int().is_err());
    }

    #[test]
    fn float() {
        let mut parser = Parser::from("123.456");
        assert_eq!(parser.float(), Ok(123.456));
        assert_eq!(parser.position.pos, 7);
        assert_eq!(parser.buffer, Vec::new());
        assert_eq!(parser.lexeme_stack, Vec::new());

        assert!(Parser::from("123").float().is_err());
        assert!(Parser::from("123f").float().is_ok());
    }

    #[test]
    fn float_with_exponent() {
        assert_eq!(Parser::from("12e4f").float(), Ok(120_000.0));
        assert_eq!(Parser::from("12.0e4f").float(), Ok(120_000.0));
    }
}
