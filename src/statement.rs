use crate::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Print(Expr),
    Expression(Expr),
}
