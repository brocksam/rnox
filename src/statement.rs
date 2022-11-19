use crate::expr::Expr;

pub enum Statement {
    Print(Expr),
    Expression(Expr),
}
