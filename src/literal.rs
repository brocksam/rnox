use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Identifier(String),
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier(value) | Self::String(value) => write!(f, "{:?}", value),
            Self::Number(value) => write!(f, "{:?}", value),
            Self::Bool(value) => write!(f, "{:?}", value),
            Self::Nil => write!(f, "nil"),
        }
    }
}
