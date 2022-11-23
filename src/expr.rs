use crate::{
    literal::Literal,
    op::{BinaryOp, UnaryOp},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>),
    Variable(Literal),
}
