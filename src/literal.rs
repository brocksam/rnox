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
            Literal::Identifier(value) => write!(f, "{:?}", value),
            Literal::Number(value) => write!(f, "{:?}", value),
            Literal::String(value) => write!(f, "{:?}", value),
            Literal::False => write!(f, "false"),
            Literal::True => write!(f, "true"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}
