use lasso::Spur;
use std::collections::HashMap;

use crate::{
    interner::{get_or_intern, resolve},
    parse::{expr::Expr, terminal::Terminal, types::Type},
};

struct TypeEnvironment {
    pub definitions: HashMap<Spur, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            definitions: HashMap::new(),
        }
    }
}

trait TypeCheck {
    // TODO: does this need mut?
    fn type_check(&self, env: &mut TypeEnvironment) -> Result<Type, String>;
}

const ARRAY_TYPE_NAME: &str = "array";

impl TypeCheck for Terminal {
    fn type_check(&self, env: &mut TypeEnvironment) -> Result<Type, String> {
        match self {
            Self::Int(_) => Ok(Type::Number),
            Self::Float(_) => Ok(Type::Float),
            Self::String(_) => Ok(Type::String),
            Self::Bool(_) => Ok(Type::Bool),
            Self::Ident(ident) => env.definitions.get(ident).cloned().ok_or_else(|| {
                format!(
                    "identifier `{}` not found in type environment",
                    resolve(*ident)
                )
            }),
            Self::Expr(e) => e.type_check(env),
            Self::Function(_) => todo!(),
            Self::Block(_) => todo!(),
            Self::Array(exprs) => {
                let first = exprs.first().ok_or("empty array")?;
                let first = first.type_check(env)?;
                for expr in exprs {
                    if expr.type_check(env)? != first {
                        return Err("array elements must be of the same type".into());
                    }
                }
                let array_type = get_or_intern(ARRAY_TYPE_NAME);
                let r#type = Type::Ident(array_type, vec![first]);
                Ok(r#type)
            }
            // Expr(Box<Expr>)
            // Function(Function)
        }
    }
}

impl TypeCheck for Expr {
    fn type_check(&self, env: &mut TypeEnvironment) -> Result<Type, String> {
        match self {
            Self::Terminal(t) => t.type_check(env),
            Self::BinaryOp(_, _, _) => todo!(),
            Self::UnaryOp(op, e) => {
                use crate::parse::expr::UnaryOp::{BitwiseNot, Neg, Not};
                let e_type = e.type_check(env)?;
                match op {
                    Not => Ok(Type::Bool),
                    Neg => {
                        if e_type != Type::Number {
                            return Err(format!("expected number, found {e_type}"));
                        }
                        Ok(Type::Number)
                    }
                    BitwiseNot => {
                        if e_type != Type::Number {
                            return Err(format!("expected number, found {e_type}"));
                        }
                        Ok(Type::Number)
                    }
                }
            }
            Self::Call(func, args) => {
                let args_types = args
                    .iter()
                    .map(|a| a.type_check(env))
                    .collect::<Result<Vec<_>, _>>()?;
                let func_type = func.type_check(env)?;
                match func_type {
                    Type::Function(params, ret) => {
                        if params.len() != args_types.len() {
                            return Err(format!(
                                "expected {} arguments, found {}",
                                params.len(),
                                args_types.len()
                            ));
                        }
                        for (param, arg) in params.iter().zip(args_types.iter()) {
                            if param != arg {
                                return Err(format!(
                                    "expected argument of type {param}, found {arg}"
                                ));
                            }
                        }
                        Ok(*ret)
                    }
                    _ => Err(format!("expected function, found {func_type}",)),
                }
            }
            Self::Index(_, _) => todo!(),
            Self::Member(_, _) => todo!(),
            Self::If(_, _, _) => todo!(),
            Self::While(_, _) => todo!(),
        }
    }
}
