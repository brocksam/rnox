use crate::{
    error,
    literal::Literal,
    token::{Token, TokenType},
};

pub struct Scanner {
    pub source: String,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub const fn new(source: String) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.tokens.push(Token::new(
            TokenType::EndOfFile,
            String::new(),
            None,
            self.line,
        ));
        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token_without_literal(TokenType::LeftParen),
            ')' => self.add_token_without_literal(TokenType::RightParen),
            '{' => self.add_token_without_literal(TokenType::LeftBrace),
            '}' => self.add_token_without_literal(TokenType::RightBrace),
            ',' => self.add_token_without_literal(TokenType::Comma),
            '.' => self.add_token_without_literal(TokenType::Dot),
            '-' => self.add_token_without_literal(TokenType::Minus),
            '+' => self.add_token_without_literal(TokenType::Plus),
            ';' => self.add_token_without_literal(TokenType::Semicolon),
            '*' => self.add_token_without_literal(TokenType::Star),
            '!' => {
                if self.match_next('=') {
                    self.add_token_without_literal(TokenType::BangEqual);
                } else {
                    self.add_token_without_literal(TokenType::Bang);
                }
            }
            '=' => {
                if self.match_next('=') {
                    self.add_token_without_literal(TokenType::EqualEqual);
                } else {
                    self.add_token_without_literal(TokenType::Equal);
                }
            }
            '<' => {
                if self.match_next('=') {
                    self.add_token_without_literal(TokenType::LessEqual);
                } else {
                    self.add_token_without_literal(TokenType::Less);
                }
            }
            '>' => {
                if self.match_next('=') {
                    self.add_token_without_literal(TokenType::GreaterEqual);
                } else {
                    self.add_token_without_literal(TokenType::Greater);
                }
            }
            '/' => {
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token_without_literal(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            c => {
                let mut msg = "Unexpected character ".to_string();
                msg.push(c);
                msg.push('.');
                error::error(self.line, &msg);
            }
        }
    }

    const fn is_digit(c: char) -> bool {
        matches!(c, '0'..='9')
    }

    const fn is_alpha(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    const fn is_alpha_numeric(c: char) -> bool {
        Self::is_alpha(c) || Self::is_digit(c)
    }

    fn identifier(&mut self) {
        while Self::is_alpha_numeric(self.peek()) {
            self.advance();
        }
        let text = &self.source[self.start..self.current];
        let token_type = match text {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };
        if token_type == TokenType::Identifier {
            self.add_token(token_type, Some(Literal::Identifier(text.to_string())));
        } else {
            self.add_token_without_literal(token_type);
        }
    }

    fn number(&mut self) {
        while Self::is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == '.' && Self::is_digit(self.peek_next()) {
            self.advance();
            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }
        self.add_token(
            TokenType::Number,
            Some(Literal::Number(
                self.source[self.start..self.current].parse().unwrap(),
            )),
        );
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            error::error(self.line, &String::from("Unterminated string."));
            return;
        }
        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token(TokenType::String, Some(Literal::String(value.to_string())));
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        c
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current) != Some(expected) {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn add_token_without_literal(&mut self, token_type: TokenType) {
        self.add_token(token_type, None);
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let text = &self.source[self.start..self.current];
        self.tokens
            .push(Token::new(token_type, text.to_string(), literal, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[cfg(test)]
mod tests_scanner {

    use super::*;

    macro_rules! repl_single_token_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (source, token_type, lexeme, literal) = $value;
                    let mut scanner = Scanner::new(source.to_string());
                    let tokens = scanner.scan_tokens();
                    let expected = vec![
                        Token::new(
                            token_type,
                            lexeme.to_string(),
                            literal,
                            1,
                        ),
                        Token::new(
                            TokenType::EndOfFile,
                            "".to_string(),
                            None,
                            2,
                        ),
                    ];
                    assert_eq!(tokens, &expected);
                }
            )*
        }
    }

    macro_rules! repl_variable_assignment_single_token_tests {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (source, token_type, lexeme, literal) = $value;
                    let mut scanner = Scanner::new(source.to_owned());
                    let tokens = scanner.scan_tokens();
                    let expected = vec![
                        Token::new(TokenType::Var, "var".to_owned(), None, 1),
                        Token::new(TokenType::Identifier, "a".to_owned(), Some(Literal::Identifier("a".to_owned())), 1),
                        Token::new(TokenType::Equal, "=".to_owned(), None, 1),
                        Token::new(token_type, lexeme.to_owned(), literal, 1),
                        Token::new(TokenType::Semicolon, ";".to_owned(), None, 1),
                        Token::new(TokenType::EndOfFile, String::new(), None, 2),
                    ];
                    assert_eq!(tokens, &expected);
                }
            )*
        }
    }

    repl_single_token_tests! {
        // Single-character tokens
        test_repl_left_paren: ("(\n", TokenType::LeftParen, "(", None),
        test_repl_right_paren: (")\n", TokenType::RightParen, ")", None),
        test_repl_left_brace: ("{\n", TokenType::LeftBrace, "{", None),
        test_repl_right_brace: ("}\n", TokenType::RightBrace, "}", None),
        test_repl_comma: (",\n", TokenType::Comma, ",", None),
        test_repl_dot: (".\n", TokenType::Dot, ".", None),
        test_repl_minus: ("-\n", TokenType::Minus, "-", None),
        test_repl_plus: ("+\n", TokenType::Plus, "+", None),
        test_repl_semicolon: (";\n", TokenType::Semicolon, ";", None),
        test_repl_slash: ("/\n", TokenType::Slash, "/", None),
        test_repl_star: ("*\n", TokenType::Star, "*", None),

        // One or two character tokens
        test_repl_bang: ("!\n", TokenType::Bang, "!", None),
        test_repl_bang_equal: ("!=\n", TokenType::BangEqual, "!=", None),
        test_repl_equal: ("=\n", TokenType::Equal, "=", None),
        test_repl_equal_equal: ("==\n", TokenType::EqualEqual, "==", None),
        test_repl_greater: (">\n", TokenType::Greater, ">", None),
        test_repl_greater_equal: (">=\n", TokenType::GreaterEqual, ">=", None),
        test_repl_less: ("<\n", TokenType::Less, "<", None),
        test_repl_less_equal: ("<=\n", TokenType::LessEqual, "<=", None),

        // Number literals
        test_repl_zero_int: ("0\n", TokenType::Number, "0", Some(Literal::Number(0.0))),
        test_repl_zero_float: ("0.0\n", TokenType::Number, "0.0", Some(Literal::Number(0.0))),
        test_repl_one_int: ("1\n", TokenType::Number, "1", Some(Literal::Number(1.0))),
        test_repl_one_float: ("1.0\n", TokenType::Number, "1.0", Some(Literal::Number(1.0))),
        test_repl_large_int: ("999999\n", TokenType::Number, "999999", Some(Literal::Number(999_999.0))),
        test_repl_large_float: ("999999.0\n", TokenType::Number, "999999.0", Some(Literal::Number(999_999.0))),
        test_repl_small_float: ("0.0000001\n", TokenType::Number, "0.0000001", Some(Literal::Number(0.000_000_1))),

        // String literals
        test_repl_single_string: ("\"a\"\n", TokenType::String, "\"a\"", Some(Literal::String("a".to_string()))),
        test_repl_multiple_string: ("\"abyz\"\n", TokenType::String, "\"abyz\"", Some(Literal::String("abyz".to_string()))),
        test_repl_whitespace_string: ("\" \"\n", TokenType::String, "\" \"", Some(Literal::String(" ".to_string()))),

        // Identifier literals
        test_repl_lowercase_single_identifier: ("a\n", TokenType::Identifier, "a", Some(Literal::Identifier("a".to_string()))),
        test_repl_uppercase_single_identifier: ("A\n", TokenType::Identifier, "A", Some(Literal::Identifier("A".to_string()))),
        test_repl_underscore_single_identifier: ("_\n", TokenType::Identifier, "_", Some(Literal::Identifier("_".to_string()))),
        test_repl_lowercase_multiple_identifier: ("abyz\n", TokenType::Identifier, "abyz", Some(Literal::Identifier("abyz".to_string()))),
        test_repl_uppercase_multiple_identifier: ("ABYZ\n", TokenType::Identifier, "ABYZ", Some(Literal::Identifier("ABYZ".to_string()))),
        test_repl_underscore_multiple_identifier: ("_abyz_ABYZ\n", TokenType::Identifier, "_abyz_ABYZ", Some(Literal::Identifier("_abyz_ABYZ".to_string()))),

        // Keywords
        test_repl_and: ("and\n", TokenType::And, "and", None),
        test_repl_class: ("class\n", TokenType::Class, "class", None),
        test_repl_else: ("else\n", TokenType::Else, "else", None),
        test_repl_false: ("false\n", TokenType::False, "false", None),
        test_repl_for: ("for\n", TokenType::For, "for", None),
        test_repl_fun: ("fun\n", TokenType::Fun, "fun", None),
        test_repl_if: ("if\n", TokenType::If, "if", None),
        test_repl_nil: ("nil\n", TokenType::Nil, "nil", None),
        test_repl_or: ("or\n", TokenType::Or, "or", None),
        test_repl_print: ("print\n", TokenType::Print, "print", None),
        test_repl_return: ("return\n", TokenType::Return, "return", None),
        test_repl_super: ("super\n", TokenType::Super, "super", None),
        test_repl_this: ("this\n", TokenType::This, "this", None),
        test_repl_true: ("true\n", TokenType::True, "true", None),
        test_repl_var: ("var\n", TokenType::Var, "var", None),
        test_repl_while: ("while\n", TokenType::While, "while", None),
    }

    #[test]
    fn test_multiline_string() {
        let source = "\"\n \nabcd\nABCD\n \"\n";
        let lexeme = "\"\n \nabcd\nABCD\n \"";
        let literal = Some(Literal::String("\n \nabcd\nABCD\n ".to_string()));
        let mut scanner = Scanner::new(source.to_string());
        let tokens = scanner.scan_tokens();
        let expected = vec![
            Token::new(TokenType::String, lexeme.to_string(), literal, 5),
            Token::new(TokenType::EndOfFile, String::new(), None, 6),
        ];
        assert_eq!(tokens, &expected);
    }

    repl_variable_assignment_single_token_tests! {
        test_variable_assignment_string: (
            "var a = \"string\";\n",
            TokenType::String,
            "\"string\"",
            Some(Literal::String("string".to_owned())),
        ),
        test_variable_assignment_integer: (
            "var a = 1;\n",
            TokenType::Number,
            "1",
            Some(Literal::Number(1.0)),
        ),
        test_variable_assignment_float: (
            "var a = 1;\n",
            TokenType::Number,
            "1",
            Some(Literal::Number(1.0)),
        ),
        test_variable_assignment_true: (
            "var a = true;\n",
            TokenType::True,
            "true",
            None,
        ),
        test_variable_assignment_false: (
            "var a = false;\n",
            TokenType::False,
            "false",
            None,
        ),
        test_variable_assignment_nil: (
            "var a = nil;\n",
            TokenType::Nil,
            "nil",
            None,
        ),
    }

    #[test]
    fn test_variable_assignment_negative_int() {
        let source = "var a = -1;\n";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let expected = vec![
            Token::new(TokenType::Var, "var".to_owned(), None, 1),
            Token::new(
                TokenType::Identifier,
                "a".to_owned(),
                Some(Literal::Identifier("a".to_owned())),
                1,
            ),
            Token::new(TokenType::Equal, "=".to_owned(), None, 1),
            Token::new(TokenType::Minus, "-".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "1".to_owned(),
                Some(Literal::Number(1.0)),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_owned(), None, 1),
            Token::new(TokenType::EndOfFile, String::new(), None, 2),
        ];
        assert_eq!(tokens, &expected);
    }

    #[test]
    fn test_variable_assignment_negative_float() {
        let source = "var a = -1.0;\n";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let expected = vec![
            Token::new(TokenType::Var, "var".to_owned(), None, 1),
            Token::new(
                TokenType::Identifier,
                "a".to_owned(),
                Some(Literal::Identifier("a".to_owned())),
                1,
            ),
            Token::new(TokenType::Equal, "=".to_owned(), None, 1),
            Token::new(TokenType::Minus, "-".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "1.0".to_owned(),
                Some(Literal::Number(1.0)),
                1,
            ),
            Token::new(TokenType::Semicolon, ";".to_owned(), None, 1),
            Token::new(TokenType::EndOfFile, String::new(), None, 2),
        ];
        assert_eq!(tokens, &expected);
    }

    #[test]
    fn test_variable_assignment_math_expression() {
        let source = "var a = -1.0 * (2.0 / 3.0) - (4.0 + 5.0);\n";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let expected = vec![
            Token::new(TokenType::Var, "var".to_owned(), None, 1),
            Token::new(
                TokenType::Identifier,
                "a".to_owned(),
                Some(Literal::Identifier("a".to_owned())),
                1,
            ),
            Token::new(TokenType::Equal, "=".to_owned(), None, 1),
            Token::new(TokenType::Minus, "-".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "1.0".to_owned(),
                Some(Literal::Number(1.0)),
                1,
            ),
            Token::new(TokenType::Star, "*".to_owned(), None, 1),
            Token::new(TokenType::LeftParen, "(".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "2.0".to_owned(),
                Some(Literal::Number(2.0)),
                1,
            ),
            Token::new(TokenType::Slash, "/".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "3.0".to_owned(),
                Some(Literal::Number(3.0)),
                1,
            ),
            Token::new(TokenType::RightParen, ")".to_owned(), None, 1),
            Token::new(TokenType::Minus, "-".to_owned(), None, 1),
            Token::new(TokenType::LeftParen, "(".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "4.0".to_owned(),
                Some(Literal::Number(4.0)),
                1,
            ),
            Token::new(TokenType::Plus, "+".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "5.0".to_owned(),
                Some(Literal::Number(5.0)),
                1,
            ),
            Token::new(TokenType::RightParen, ")".to_owned(), None, 1),
            Token::new(TokenType::Semicolon, ";".to_owned(), None, 1),
            Token::new(TokenType::EndOfFile, String::new(), None, 2),
        ];
        assert_eq!(tokens, &expected);
    }

    #[test]
    fn test_variable_assignment_bool_expression() {
        let source = "var a = !(true == ((1.0 > 2.0) != false);\n";
        let mut scanner = Scanner::new(source.to_owned());
        let tokens = scanner.scan_tokens();
        let expected = vec![
            Token::new(TokenType::Var, "var".to_owned(), None, 1),
            Token::new(
                TokenType::Identifier,
                "a".to_owned(),
                Some(Literal::Identifier("a".to_owned())),
                1,
            ),
            Token::new(TokenType::Equal, "=".to_owned(), None, 1),
            Token::new(TokenType::Bang, "!".to_owned(), None, 1),
            Token::new(TokenType::LeftParen, "(".to_owned(), None, 1),
            Token::new(TokenType::True, "true".to_owned(), None, 1),
            Token::new(TokenType::EqualEqual, "==".to_owned(), None, 1),
            Token::new(TokenType::LeftParen, "(".to_owned(), None, 1),
            Token::new(TokenType::LeftParen, "(".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "1.0".to_owned(),
                Some(Literal::Number(1.0)),
                1,
            ),
            Token::new(TokenType::Greater, ">".to_owned(), None, 1),
            Token::new(
                TokenType::Number,
                "2.0".to_owned(),
                Some(Literal::Number(2.0)),
                1,
            ),
            Token::new(TokenType::RightParen, ")".to_owned(), None, 1),
            Token::new(TokenType::BangEqual, "!=".to_owned(), None, 1),
            Token::new(TokenType::False, "false".to_owned(), None, 1),
            Token::new(TokenType::RightParen, ")".to_owned(), None, 1),
            Token::new(TokenType::Semicolon, ";".to_owned(), None, 1),
            Token::new(TokenType::EndOfFile, String::new(), None, 2),
        ];
        assert_eq!(tokens, &expected);
    }
}
