use crate::{
    error::RuntimeError,
    expr::Expr,
    literal::Literal,
    op::{BinaryOp, UnaryOp},
    statement::Statement,
    value::Value,
};

pub struct Interpreter {
    pub statements: Vec<Statement>,
}

impl Interpreter {
    pub const fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    pub fn interpret(&self) -> Option<RuntimeError> {
        for statement in &self.statements {
            if let Some(runtime_error) = self.visit_statement(statement) {
                return Some(runtime_error);
            };
        }
        None
    }

    fn visit_statement(&self, statement: &Statement) -> Option<RuntimeError> {
        match statement {
            Statement::Expression(expression) => self.visit_expression_statement(expression),
            Statement::Print(expression) => self.visit_print_statement(expression),
        }
    }

    fn visit_expression_statement(&self, expression: &Expr) -> Option<RuntimeError> {
        match self.visit_expression(expression) {
            Ok(_) => None,
            Err(runtime_error) => Some(runtime_error),
        }
    }

    fn visit_print_statement(&self, expression: &Expr) -> Option<RuntimeError> {
        match self.visit_expression(expression) {
            Ok(expr) => {
                println!("{}", expr);
                None
            }
            Err(runtime_error) => Some(runtime_error),
        }
    }

    fn visit_expression(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match &expr {
            Expr::Binary(l, op, r) => self.visit_binary(l, op, r),
            Expr::Grouping(e) => self.visit_expression(e),
            Expr::Literal(l) => Ok(Self::visit_literal(l)),
            Expr::Unary(op, e) => self.visit_unary(op, e),
        }
    }

    fn visit_literal(literal: &Literal) -> Value {
        match &literal {
            Literal::Identifier(_s) => todo!(),
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.to_string()),
            Literal::False => Value::Bool(false),
            Literal::True => Value::Bool(true),
            Literal::Nil => Value::Nil,
        }
    }

    fn visit_unary(&self, op: &UnaryOp, expr: &Expr) -> Result<Value, RuntimeError> {
        let value = self.visit_expression(expr)?;

        match (&op, &value) {
            (UnaryOp::Minus, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOp::Minus, _) => Err(RuntimeError {
                message: format!(
                    "Operand to the unary operator {:?} must be a number, not {:?}.",
                    op, value
                ),
            }),
            (UnaryOp::Bang, _) => Ok(Value::Bool(!Self::is_truthy(&value))),
        }
    }

    fn visit_binary(
        &self,
        lhs_expr: &Expr,
        op: &BinaryOp,
        rhs_expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        let lhs = self.visit_expression(lhs_expr)?;
        let rhs = self.visit_expression(rhs_expr)?;

        match (&lhs, &op, &rhs) {
            (Value::Number(l), BinaryOp::Greater, Value::Number(r)) => Ok(Value::Bool(l > r)),
            (Value::Number(l), BinaryOp::GreaterEqual, Value::Number(r)) => {
                Ok(Value::Bool(l >= r))
            }
            (Value::Number(l), BinaryOp::Less, Value::Number(r)) => Ok(Value::Bool(l < r)),
            (Value::Number(l), BinaryOp::LessEqual, Value::Number(r)) => Ok(Value::Bool(l <= r)),
            (_, BinaryOp::BangEqual, _) => Ok(Value::Bool(!Self::is_equal(&lhs, &rhs))),
            (_, BinaryOp::EqualEqual, _) => Ok(Value::Bool(Self::is_equal(&lhs, &rhs))),
            (Value::Number(l), BinaryOp::Plus, Value::Number(r)) => Ok(Value::Number(l + r)),
            (Value::String(l), BinaryOp::Plus, Value::String(r)) => {
                Ok(Value::String(format!("{}{}", l, r)))
            }
            (Value::Number(l), BinaryOp::Minus, Value::Number(r)) => Ok(Value::Number(l - r)),
            (Value::Number(l), BinaryOp::Star, Value::Number(r)) => Ok(Value::Number(l * r)),
            (Value::Number(l), BinaryOp::Slash, Value::Number(r)) => {
                if *r == 0.0 {
                    Err(RuntimeError { message: format!("Cannot divide {:?} by zero.", lhs)})

                } else {
                    Ok(Value::Number(l / r))
                }
            }
            (_, BinaryOp::Plus, _) => Err(RuntimeError {message: format!("Operands to the binary operator {:?} must both be numbers or strings, not {:?} and {:?}.", op, lhs, rhs)}),
            _ => Err(RuntimeError {message: format!("Operands to the binary operator {:?} must both be numbers, not {:?} and {:?}.", op, lhs, rhs)}),
        }
    }

    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Bool(b) => *b,
            Value::String(s) => !matches!(s.as_str(), ""),
            Value::Number(n) => *n == 0.0,
        }
    }

    fn is_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) | (_, Value::Nil) => false,
            (Value::Number(n1), Value::Number(n2)) => (n1 - n2).abs() < f64::EPSILON,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            _ => true,
        }
    }
}
