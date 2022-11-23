use crate::{expr::Expr, literal::Literal};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Print(Expr),
    Expression(Expr),
    Variable(Literal, Option<Expr>),
}
