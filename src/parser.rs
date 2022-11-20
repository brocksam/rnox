use crate::{
    error::ParseError,
    expr::Expr,
    literal::Literal,
    op::{BinaryOp, UnaryOp},
    statement::Statement,
    token::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    // program -> statement* EOF ;
    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(parse_error) => return Err(parse_error),
            };
        }
        Ok(statements)
    }

    // statement -> expressionStatement | printStatement ;
    fn statement(&mut self) -> Result<Statement, ParseError> {
        if self.match_token_type(TokenType::Print) {
            return self.print_statement();
        }
        self.expression_statement()
    }

    // printStatement -> expression ";" ;
    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let value = match self.expression() {
            Ok(expr) => expr,
            Err(parse_error) => return Err(parse_error),
        };
        match self.consume(TokenType::Semicolon, "Expect ';' after value.".to_owned()) {
            Ok(_) => Ok(Statement::Print(value)),
            Err(parse_error) => Err(parse_error),
        }
    }

    // expressionStatement -> "print" expression ";" ;
    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = match self.expression() {
            Ok(expr) => expr,
            Err(parse_error) => return Err(parse_error),
        };
        match self.consume(
            TokenType::Semicolon,
            "Expect ';' after expression.".to_owned(),
        ) {
            Ok(_) => Ok(Statement::Expression(expr)),
            Err(parse_error) => Err(parse_error),
        }
    }

    // expression -> equality ;
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    // equality -> comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_token_type_one_of(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = match self.previous().token_type {
                TokenType::BangEqual => BinaryOp::BangEqual,
                TokenType::EqualEqual => BinaryOp::EqualEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;
        while self.match_token_type_one_of(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = match self.previous().token_type {
                TokenType::Greater => BinaryOp::Greater,
                TokenType::GreaterEqual => BinaryOp::GreaterEqual,
                TokenType::Less => BinaryOp::Less,
                TokenType::LessEqual => BinaryOp::LessEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // term -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while self.match_token_type_one_of(&[TokenType::Minus, TokenType::Plus]) {
            let operator = match self.previous().token_type {
                TokenType::Minus => BinaryOp::Minus,
                TokenType::Plus => BinaryOp::Plus,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while self.match_token_type_one_of(&[TokenType::Slash, TokenType::Star]) {
            let operator = match self.previous().token_type {
                TokenType::Slash => BinaryOp::Slash,
                TokenType::Star => BinaryOp::Star,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // unary -> ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token_type_one_of(&[TokenType::Bang, TokenType::Minus]) {
            let operator = match self.previous().token_type {
                TokenType::Bang => UnaryOp::Bang,
                TokenType::Minus => UnaryOp::Minus,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.primary()
    }

    // primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_token_type(TokenType::False) {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.match_token_type(TokenType::True) {
            return Ok(Expr::Literal(Literal::True));
        }
        if self.match_token_type(TokenType::Nil) {
            return Ok(Expr::Literal(Literal::Nil));
        }
        if self.match_token_type(TokenType::Number) {
            match &self.previous().literal {
                Some(Literal::Number(n)) => {
                    return Ok(Expr::Literal(Literal::Number(*n)));
                }
                Some(l) => panic!(
                    "Internal error in parser, when parsing number found literal {:?}",
                    l
                ),
                None => panic!("Internal error in parser, when parsing number no literal found"),
            }
        }
        if self.match_token_type(TokenType::String) {
            match &self.previous().literal {
                Some(Literal::String(s)) => {
                    return Ok(Expr::Literal(Literal::String(s.clone())));
                }
                Some(l) => panic!(
                    "Internal error in parser, when parsing string found literal {:?}",
                    l
                ),
                None => panic!("Internal error in parser, when parsing string no literal found"),
            }
        }
        if self.match_token_type(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(
                TokenType::RightParen,
                "expected ')' after expression".to_string(),
            )
            .unwrap();
            return Ok(Expr::Grouping(Box::new(expr)));
        }
        Err(ParseError {
            token: self.peek().clone(),
            message: None,
        })
    }

    fn match_token_type_one_of(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types.iter() {
            if self.check(*token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn match_token_type(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            return true;
        }
        false
    }

    fn consume(&mut self, token_type: TokenType, message: String) -> Result<&Token, ParseError> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        Err(ParseError {
            token: self.peek().clone(),
            message: Some(message),
        })
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EndOfFile
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    #[allow(dead_code)]
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek().token_type {
                TokenType::Class
                | TokenType::For
                | TokenType::Fun
                | TokenType::If
                | TokenType::Print
                | TokenType::Return
                | TokenType::Var
                | TokenType::While => return,
                _ => {}
            }
            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! repl_single_expr_tests {
        ($($name:ident: $value:expr,)*) => {
            $(

                #[test]
                fn $name() {
                    let (token_type, lexeme, literal, expr_literal) = $value;
                    let tokens = vec![
                        Token::new(
                            token_type,
                            lexeme.to_string(),
                            literal,
                            1,
                        ),
                        Token::new(
                            TokenType::Semicolon,
                            ";".to_owned(),
                            None,
                            1,
                        ),
                        Token::new(
                            TokenType::EndOfFile,
                            "".to_owned(),
                            None,
                            2,
                        ),
                    ];
                    let mut parser = Parser::new(tokens.to_vec());
                    let expr = parser.parse();
                    let expected = Ok(vec![Statement::Expression(expr_literal)]);
                    assert_eq!(expr, expected);
                }
            )*
        }
    }

    macro_rules! repl_single_op_error_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (token_type, lexeme, expr_token_type, expr_lexeme, line) = $value;
                    let tokens = vec![
                        Token::new(
                            token_type,
                            lexeme.to_owned(),
                            None,
                            1,
                        ),
                        Token::new(
                            TokenType::Semicolon,
                            ";".to_owned(),
                            None,
                            1,
                        ),
                        Token::new(
                            TokenType::EndOfFile,
                            "".to_owned(),
                            None,
                            2,
                        ),
                    ];
                    let mut parser = Parser::new(tokens.to_vec());
                    let expr = parser.parse();
                    let token = Token::new(
                        expr_token_type,
                        expr_lexeme.to_owned(),
                        None,
                        line,
                    );
                    let expected = Err(ParseError { token, message: None });
                    assert_eq!(expr, expected);
                }
            )*
        }
    }

    repl_single_expr_tests! {
        // Number literals
        repl_number_zero: (TokenType::Number, "0.0", Some(Literal::Number(0.0)), Expr::Literal(Literal::Number(0.0))),
        repl_number_small: (TokenType::Number, "0.000000_1", Some(Literal::Number(0.000_000_1)), Expr::Literal(Literal::Number(0.000_000_1))),
        repl_number_large: (TokenType::Number, "999999.9", Some(Literal::Number(999_999.9)), Expr::Literal(Literal::Number(999_999.9))),

        // String literals
        repl_string_single: (TokenType::String, "\"a\"", Some(Literal::String("a".to_string())), Expr::Literal(Literal::String("a".to_string()))),
        repl_string_multiple: (TokenType::String, "\"abyz\"", Some(Literal::String("abyz".to_string())), Expr::Literal(Literal::String("abyz".to_string()))),
        repl_string_whitespace: (TokenType::String, "\" \"", Some(Literal::String(" ".to_string())), Expr::Literal(Literal::String(" ".to_string()))),
    }

    repl_single_op_error_tests! {
        repl_minus: (TokenType::Minus, "-", TokenType::Semicolon, ";", 1),
        repl_plus: (TokenType::Plus, "+", TokenType::Plus, "+", 1),
        repl_bang: (TokenType::Bang, "!", TokenType::Semicolon, ";", 1),
        repl_equal: (TokenType::Equal, "=", TokenType::Equal, "=", 1),
        repl_bang_equal: (TokenType::BangEqual, "!=", TokenType::BangEqual, "!=", 1),
        repl_equal_equal: (TokenType::EqualEqual, "==", TokenType::EqualEqual, "==", 1),
        repl_greater: (TokenType::Greater, ">", TokenType::Greater, ">", 1),
        repl_greater_equal: (TokenType::GreaterEqual, ">=", TokenType::GreaterEqual, ">=", 1),
        repl_less: (TokenType::Less, "<", TokenType::Less, "<", 1),
        repl_less_equal: (TokenType::LessEqual, "<=", TokenType::LessEqual, "<=", 1),
        repl_star: (TokenType::Star, "*", TokenType::Star, "*", 1),
        repl_slash: (TokenType::Slash, "/", TokenType::Slash, "/", 1),
        repl_semicolon: (TokenType::Semicolon, ";", TokenType::Semicolon, ";", 1),
        repl_comma: (TokenType::Comma, ",", TokenType::Comma, ",", 1),
        repl_dot: (TokenType::Dot, ".", TokenType::Dot, ".", 1),
        repl_left_paren: (TokenType::LeftParen, "(", TokenType::Semicolon, ";", 1),
        repl_right_paren: (TokenType::RightParen, ")", TokenType::RightParen, ")",  1),
        repl_left_brace: (TokenType::LeftBrace, "{", TokenType::LeftBrace, "{", 1),
        repl_right_brace: (TokenType::RightBrace, "}", TokenType::RightBrace, "}",  1),
    }
}
