#[derive(Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    BangEqual,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    Minus,
    Bang,
}
