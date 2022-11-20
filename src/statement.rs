use crate::{
    expr::Expr,
    token::Token,
};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Print(Expr),
    Expression(Expr),
    Variable(Token, Expr),
}
