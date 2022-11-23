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

    // program -> declaration* EOF ;
    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(parse_error) => return Err(parse_error),
            };
        }
        Ok(statements)
    }

    // declaration -> variableDeclaration | statement ;
    fn declaration(&mut self) -> Result<Statement, ParseError> {
        let value = if self.match_token_type(TokenType::Var) {
            self.variable_declaration()
        } else {
            self.statement()
        };
        match value {
            Ok(statement) => Ok(statement),
            Err(parse_error) => {
                self.synchronize();
                Err(parse_error)
            }
        }
    }

    // variableDeclaration -> "var" IDENTIFIER ( "=" expression )? ";" ;
    fn variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let name = match self.consume(TokenType::Identifier, "Expect variable name.".to_owned()) {
            Ok(token) => match &token.literal {
                Some(Literal::Identifier(name)) => Literal::Identifier(name.to_owned()),
                _ => {
                    return Err(ParseError {
                        token: token.clone(),
                        message: Some("Expect named identifier.".to_owned()),
                    })
                }
            },
            Err(parse_error) => return Err(parse_error),
        };
        let initializer = if self.match_token_type(TokenType::Equal) {
            match self.expression() {
                Ok(expr) => Some(expr),
                Err(parse_error) => return Err(parse_error),
            }
        } else {
            None
        };
        match self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.".to_owned(),
        ) {
            Ok(_) => Ok(Statement::Variable(name, initializer)),
            Err(parse_error) => Err(parse_error),
        }
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

    // primary -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
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
                    "Internal error in parser, when parsing number found literal {:?}.",
                    l
                ),
                None => panic!("Internal error in parser, when parsing number no literal found."),
            }
        }
        if self.match_token_type(TokenType::String) {
            match &self.previous().literal {
                Some(Literal::String(s)) => {
                    return Ok(Expr::Literal(Literal::String(s.clone())));
                }
                Some(l) => panic!(
                    "Internal error in parser, when parsing string found literal {:?}.",
                    l,
                ),
                None => panic!("Internal error in parser, when parsing string no literal found."),
            }
        }
        if self.match_token_type(TokenType::Identifier) {
            match &self.previous().literal {
                Some(Literal::Identifier(s)) => {
                    return Ok(Expr::Literal(Literal::Identifier(s.clone())));
                }
                Some(l) => panic!(
                    "Internal error in parser, when parsing identifier found literal {:?}.",
                    l,
                ),
                None => {
                    panic!("Internal error in parser, when parsing identifier no literal found.")
                }
            }
        }
        if self.match_token_type(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(
                TokenType::RightParen,
                "expected ')' after expression".to_owned(),
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
mod tests_parser {

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
                            lexeme.to_owned(),
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
                    let result = parser.parse();
                    let expected = Ok(vec![Statement::Expression(expr_literal)]);
                    assert_eq!(result, expected);
                }
            )*
        }
    }

    macro_rules! repl_single_token_error_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (token_type, lexeme, expr_token_type, expr_lexeme, message) = $value;
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
                    let result = parser.parse();
                    let token = Token::new(
                        expr_token_type,
                        expr_lexeme.to_owned(),
                        None,
                        1,
                    );
                    let expected = Err(ParseError { token, message });
                    assert_eq!(result, expected);
                }
            )*
        }
    }

    repl_single_expr_tests! {
        // Number literals
        test_repl_number_zero: (TokenType::Number, "0.0", Some(Literal::Number(0.0)), Expr::Literal(Literal::Number(0.0))),
        test_repl_number_small: (TokenType::Number, "0.000000_1", Some(Literal::Number(0.000_000_1)), Expr::Literal(Literal::Number(0.000_000_1))),
        test_repl_number_large: (TokenType::Number, "999999.9", Some(Literal::Number(999_999.9)), Expr::Literal(Literal::Number(999_999.9))),

        // String literals
        test_repl_string_single: (TokenType::String, "\"a\"", Some(Literal::String("a".to_owned())), Expr::Literal(Literal::String("a".to_owned()))),
        test_repl_string_multiple: (TokenType::String, "\"abyz\"", Some(Literal::String("abyz".to_owned())), Expr::Literal(Literal::String("abyz".to_owned()))),
        test_repl_string_whitespace: (TokenType::String, "\" \"", Some(Literal::String(" ".to_owned())), Expr::Literal(Literal::String(" ".to_owned()))),

        // Bool
        test_repl_true: (TokenType::True, "true", None, Expr::Literal(Literal::True)),
        test_repl_false: (TokenType::False, "false", None, Expr::Literal(Literal::False)),

        // Nil
        test_repl_nil: (TokenType::Nil, "nil", None, Expr::Literal(Literal::Nil)),
    }

    repl_single_token_error_tests! {
        test_repl_minus: (TokenType::Minus, "-", TokenType::Semicolon, ";", None),
        test_repl_plus: (TokenType::Plus, "+", TokenType::Plus, "+", None),
        test_repl_bang: (TokenType::Bang, "!", TokenType::Semicolon, ";", None),
        test_repl_equal: (TokenType::Equal, "=", TokenType::Equal, "=", None),
        test_repl_bang_equal: (TokenType::BangEqual, "!=", TokenType::BangEqual, "!=", None),
        test_repl_equal_equal: (TokenType::EqualEqual, "==", TokenType::EqualEqual, "==", None),
        test_repl_greater: (TokenType::Greater, ">", TokenType::Greater, ">", None),
        test_repl_greater_equal: (TokenType::GreaterEqual, ">=", TokenType::GreaterEqual, ">=", None),
        test_repl_less: (TokenType::Less, "<", TokenType::Less, "<", None),
        test_repl_less_equal: (TokenType::LessEqual, "<=", TokenType::LessEqual, "<=", None),
        test_repl_star: (TokenType::Star, "*", TokenType::Star, "*", None),
        test_repl_slash: (TokenType::Slash, "/", TokenType::Slash, "/", None),
        test_repl_semicolon: (TokenType::Semicolon, ";", TokenType::Semicolon, ";", None),
        test_repl_comma: (TokenType::Comma, ",", TokenType::Comma, ",", None),
        test_repl_dot: (TokenType::Dot, ".", TokenType::Dot, ".", None),
        test_repl_left_paren: (TokenType::LeftParen, "(", TokenType::Semicolon, ";", None),
        test_repl_right_paren: (TokenType::RightParen, ")", TokenType::RightParen, ")", None),
        test_repl_left_brace: (TokenType::LeftBrace, "{", TokenType::LeftBrace, "{", None),
        test_repl_right_brace: (TokenType::RightBrace, "}", TokenType::RightBrace, "}", None),
    }

    // #[test]
    // fn repl_variable_assignment_to_number() {
    //     let mut parser = Parser::new(tokens.to_vec());
    //     let expr = parser.parse();
    //     asserteq!(expr, expected)
    // }
}
