use std::fmt::Display;

use lasso::Spur;

use crate::interner::resolve;

use super::Parser;
/*
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntSize {
    _8,
    _16,
    _32,
    _64,
    Size,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FloatSize {
    _32,
    _64,
} */

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// `ident<T,K>`
    Ident(Spur, Vec<Type>),

    /*Int(IntSize),
    Uint(IntSize),*/
    Number,
    Float, /*(FloatSize)*/
    String,
    Bool,
    Void,

    //Ref(Box<Type>),
    Object(Vec<(Spur, Type)>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            /*Type::Int(size) => write!(f, "int{}", size),
            Type::Uint(size) => write!(f, "uint{}", size),*/
            Type::Number => write!(f, "number"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Ident(ident, generics) => {
                write!(f, "{}", resolve(*ident))?;
                if !generics.is_empty() {
                    write!(f, "<")?;
                    for (i, generic) in generics.iter().enumerate() {
                        write!(f, "{generic}")?;
                        if i != generics.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Type::Object(fields) => {
                write!(f, "{{")?;
                for (i, (ident, r#type)) in fields.iter().enumerate() {
                    write!(f, "{}: {}", resolve(*ident), r#type)?;
                    if i != fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    write!(f, "{t}")?;
                    if i != types.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Type::Function(args, ret) => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {ret}")
            }
        }
    }
}

lazy_static::lazy_static! {
    static ref BASIC_TYPES: Vec<(&'static str, Type)> = vec![
        /*("int8", Type::Number),
        ("int16", Type::Number),
        ("int32", Type::Int(IntSize::_32)),
        ("int64", Type::Int(IntSize::_64)),
        ("uint8", Type::Uint(IntSize::_8)),
        ("uint16", Type::Uint(IntSize::_16)),
        ("uint32", Type::Uint(IntSize::_32)),
        ("uint64", Type::Uint(IntSize::_64)),
        ("float32", Type::Float(FloatSize::_32)),
        ("float64", Type::Float(FloatSize::_64)),*/
        ("number", Type::Number),
        ("float", Type::Float),
        ("string", Type::String),
        ("bool", Type::Bool),
        ("void", Type::Void),
    ];
}

impl Parser<'_> {
    pub fn r#type(&mut self) -> Result<Type, String> {
        self.lexeme(|s| {
            if let Ok(basic_type) = s.match_to(&BASIC_TYPES) {
                Ok(basic_type.clone())
            } else if let Ok(ident) = s.ident() {
                let generic = s.generic().unwrap_or_default();
                Ok(Type::Ident(ident, generic))
            } else if let Ok(r#type) = s.function() {
                Ok(r#type)
            } else if let Ok(r#type) = s.tuple() {
                Ok(r#type)
            } else if let Ok(r#type) = s.object() {
                Ok(r#type)
            } else {
                Err("expected type".to_string())
            }
        })
    }
    fn object(&mut self) -> Result<Type, String> {
        self.lexeme(|s| {
            s.match_char('{')?;
            let mut fields = Vec::new();
            loop {
                if let Ok(field) = s.field() {
                    fields.push(field);
                } else {
                    break;
                }
                if s.peek() == Some(',') {
                    s.next();
                    s.skip_whitespace();
                } else {
                    break;
                }
            }
            s.match_char('}')?;
            Ok(Type::Object(fields))
        })
    }
    fn field(&mut self) -> Result<(Spur, Type), String> {
        self.lexeme(|s| {
            let ident = s.ident()?;
            s.match_char(':')?;
            s.skip_whitespace();
            let r#type = s.r#type()?;
            Ok((ident, r#type))
        })
    }
    pub fn generic(&mut self) -> Result<Vec<Type>, String> {
        self.lexeme(|s| {
            s.match_char('<')?;
            let types = s.delimited(',', Parser::r#type)?;
            s.match_char('>')?;
            Ok(types)
        })
    }
    fn tuple(&mut self) -> Result<Type, String> {
        self.lexeme(|s| {
            s.match_char('(')?;
            let types = s.delimited(',', Parser::r#type)?;
            s.match_char(')')?;
            Ok(Type::Tuple(types))
        })
    }

    pub fn function_return_type(&mut self) -> Result<Type, String> {
        self.lexeme(|s| {
            s.match_str("->")?;
            s.skip_whitespace();
            s.r#type()
        })
    }

    fn function(&mut self) -> Result<Type, String> {
        self.lexeme(|s| {
            s.match_char('(')?;
            let args = s.delimited(',', Parser::r#type)?;
            s.match_char(')')?;
            s.skip_whitespace();
            let ret = s.function_return_type()?;
            Ok(Type::Function(args, Box::new(ret)))
        })
    }
}

#[cfg(test)]
mod test {
    use crate::interner::get_or_intern;

    use super::*;

    #[test]
    fn basic_types() {
        assert_eq!(Parser::from("number").r#type(), Ok(Type::Number));
        assert_eq!(Parser::from("string").r#type(), Ok(Type::String));
        assert_eq!(Parser::from("bool").r#type(), Ok(Type::Bool));
        assert_eq!(Parser::from("void").r#type(), Ok(Type::Void));
    }

    #[test]
    fn ident() {
        let ident = get_or_intern("ident");
        let mut parser = Parser::from("ident");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Ident(ident, Vec::new()))
        );
        let mut parser = Parser::from("ident<T>");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Ident(
                get_or_intern("ident"),
                vec![Type::Ident(get_or_intern("T"), Vec::new())]
            ))
        );
        let mut parser = Parser::from("ident<T,K>");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Ident(
                get_or_intern("ident"),
                vec![
                    Type::Ident(get_or_intern("T"), Vec::new()),
                    Type::Ident(get_or_intern("K"), Vec::new())
                ]
            ))
        );
        assert_eq!(
            Parser::from("ident<T,K,>").r#type(),
            Ok(Type::Ident(
                get_or_intern("ident"),
                vec![
                    Type::Ident(get_or_intern("T"), Vec::new()),
                    Type::Ident(get_or_intern("K"), Vec::new())
                ]
            ))
        );
    }

    #[test]
    fn object() {
        let mut parser = Parser::from("{a: number}");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Object(vec![(
                get_or_intern("a"),
                Type::Number
            )]))
        );
        let mut parser = Parser::from("{a: number, b: number}");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Object(vec![
                (get_or_intern("a"), Type::Number),
                (get_or_intern("b"), Type::Number)
            ]))
        );
        let mut parser = Parser::from("{a: number, b: number,}");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Object(vec![
                (get_or_intern("a"), Type::Number),
                (get_or_intern("b"), Type::Number)
            ]))
        );
    }

    #[test]
    fn tuple() {
        let mut parser = Parser::from("(number)");
        assert_eq!(parser.r#type(), Ok(Type::Tuple(vec![Type::Number])));
        let mut parser = Parser::from("(number, number)");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Tuple(vec![Type::Number, Type::Number]))
        );
        let mut parser = Parser::from("(number, number,)");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Tuple(vec![Type::Number, Type::Number]))
        );
        let mut parser = Parser::from("((number))");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Tuple(vec![Type::Tuple(vec![Type::Number])]))
        );
    }

    #[test]
    fn function() {
        let mut parser = Parser::from("(number) -> number");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Function(vec![Type::Number], Box::new(Type::Number)))
        );
        let mut parser = Parser::from("(number, number) -> number");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Function(
                vec![Type::Number, Type::Number],
                Box::new(Type::Number)
            ))
        );
        let mut parser = Parser::from("(number, number,) -> number");
        assert_eq!(
            parser.r#type(),
            Ok(Type::Function(
                vec![Type::Number, Type::Number],
                Box::new(Type::Number)
            ))
        );
    }
}
