use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Number(isize),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self {
            Literal::String(value) => write!(f, "{:?}", value),
            Literal::Number(value) => write!(f, "{:?}", value),
        }
    }
}