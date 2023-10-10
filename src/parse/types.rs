use lasso::Spur;

use super::Parser;
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// `ident<T,K>`
    Ident(Spur, Vec<Type>),

    Int(IntSize),
    Uint(IntSize),
    Float(FloatSize),
    String,
    Bool,
    Void,

    Ref(Box<Type>),
    Object(Vec<(Spur, Type)>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
}

lazy_static::lazy_static! {
    static ref BASIC_TYPES: Vec<(&'static str, Type)> = vec![
        ("int8", Type::Int(IntSize::_8)),
        ("int16", Type::Int(IntSize::_16)),
        ("int32", Type::Int(IntSize::_32)),
        ("int64", Type::Int(IntSize::_64)),
        ("uint8", Type::Uint(IntSize::_8)),
        ("uint16", Type::Uint(IntSize::_16)),
        ("uint32", Type::Uint(IntSize::_32)),
        ("uint64", Type::Uint(IntSize::_64)),
        ("float32", Type::Float(FloatSize::_32)),
        ("float64", Type::Float(FloatSize::_64)),
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
                let generic = s.generic()?;
                Ok(Type::Ident(ident, generic))
            } else if let Ok(r#type) = s.tuple() {
                Ok(r#type)
            } else if let Ok(r#type) = s.function() {
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
                if let Some(',') = s.peek() {
                    s.next();
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
            let r#type = s.r#type()?;
            Ok((ident, r#type))
        })
    }
    fn generic(&mut self) -> Result<Vec<Type>, String> {
        self.lexeme(|s| {
            s.match_char('<')?;
            let mut types = Vec::new();
            loop {
                if let Ok(r#type) = s.r#type() {
                    types.push(r#type);
                } else {
                    break;
                }
                if let Some(',') = s.peek() {
                    s.next();
                } else {
                    break;
                }
            }
            s.match_char('>')?;
            Ok(types)
        })
    }
    fn tuple(&mut self) -> Result<Type, String> {
        self.lexeme(|s| {
            s.match_char('(')?;
            let mut types = Vec::new();
            loop {
                if let Ok(r#type) = s.r#type() {
                    types.push(r#type);
                } else {
                    break;
                }
                if let Some(',') = s.peek() {
                    s.next();
                } else {
                    break;
                }
            }
            s.match_char(')')?;
            Ok(Type::Tuple(types))
        })
    }
    fn function(&mut self) -> Result<Type, String> {
        self.lexeme(|s| {
            s.match_char('(')?;
            let mut args = Vec::new();
            loop {
                if let Ok(r#type) = s.r#type() {
                    args.push(r#type);
                } else {
                    break;
                }
                if let Some(',') = s.peek() {
                    s.next();
                } else {
                    break;
                }
            }
            s.match_char(')')?;
            s.match_str("->")?;
            let ret = s.r#type()?;
            Ok(Type::Function(args, Box::new(ret)))
        })
    }
}
