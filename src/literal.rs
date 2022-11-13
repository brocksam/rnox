use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Literal {
    Identifier(String),
    Number(f64),
    String(String),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Identifier(value) => write!(f, "{:?}", value),
            Literal::Number(value) => write!(f, "{:?}", value),
            Literal::String(value) => write!(f, "{:?}", value),
        }
    }
}
