use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Identifier(String),
    Number(f64),
    String(String),
    False,
    True,
    Nil,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier(value) | Self::String(value) => write!(f, "{:?}", value),
            Self::Number(value) => write!(f, "{:?}", value),
            Self::False => write!(f, "false"),
            Self::True => write!(f, "true"),
            Self::Nil => write!(f, "nil"),
        }
    }
}
