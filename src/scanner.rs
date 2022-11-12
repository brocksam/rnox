use crate::{
    error,
    literal::Literal,
    token::{Token, TokenType}
};

pub struct Scanner {
    pub source: String,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {

    pub fn new(source: String) -> Self {
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
            String::from(""),
            None,
            self.line),
        );
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
            '!' => if self.match_next('=') {
                self.add_token_without_literal(TokenType::BangEqual)
            } else {
                self.add_token_without_literal(TokenType::Bang)
            },
            '=' => if self.match_next('=') {
                self.add_token_without_literal(TokenType::EqualEqual)
            } else {
                self.add_token_without_literal(TokenType::Equal)
            },
            '<' => if self.match_next('=') {
                self.add_token_without_literal(TokenType::LessEqual)
            } else {
                self.add_token_without_literal(TokenType::Less)
            },
            '>' => if self.match_next('=') {
                self.add_token_without_literal(TokenType::GreaterEqual)
            } else {
                self.add_token_without_literal(TokenType::Greater)
            },
            '/' => if self.match_next('/') {
                while self.peek() != '\n' && !self.is_at_end() {
                    self.advance();
                }
            } else {
                self.add_token_without_literal(TokenType::Slash)
            },
            ' ' | '\r' | '\t' => {},
            '\n' => { self.line = self.line + 1 },
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            _ => error::error(self.line, &String::from("Unexpected character.")),
        }
    }

    fn is_digit(&self, c: &char) -> bool {
        match c {
            '0'..='9' => true,
            _ => false,
        }
    }

    fn is_alpha(&self, c: &char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        }
    }

    fn is_alpha_numeric(&self, c: &char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) {
        while self.is_alpha_numeric(&self.peek()) {
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
        while self.is_digit(&self.peek()) {
            self.advance();
        }
        if self.peek() == '.' && self.is_digit(&self.peek_next()) {
            self.advance();
            while self.is_digit(&self.peek()) {
                self.advance();
            }
        }
        self.add_token(TokenType::Number, Some(Literal::Number(self.source[self.start..self.current].parse().unwrap())));

    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line = self.line + 1;
                
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
        self.current = self.current + 1;
        c
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current) != Some(expected) {
            return false;
        }
        self.current = self.current + 1;
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
        self.tokens.push(Token::new(token_type, text.to_string(), literal, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[cfg(test)]
mod tests {

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

    repl_single_token_tests! {
        // Single-character tokens
        repl_left_paren: ("(\n", TokenType::LeftParen, "(", None),
        repl_right_paren: (")\n", TokenType::RightParen, ")", None),
        repl_left_brace: ("{\n", TokenType::LeftBrace, "{", None),
        repl_right_brace: ("}\n", TokenType::RightBrace, "}", None),
        repl_comma: (",\n", TokenType::Comma, ",", None),
        repl_dot: (".\n", TokenType::Dot, ".", None),
        repl_minus: ("-\n", TokenType::Minus, "-", None),
        repl_plus: ("+\n", TokenType::Plus, "+", None),
        repl_semicolon: (";\n", TokenType::Semicolon, ";", None),
        repl_slash: ("/\n", TokenType::Slash, "/", None),
        repl_star: ("*\n", TokenType::Star, "*", None),

        // One or two character tokens
        repl_bang: ("!\n", TokenType::Bang, "!", None),
        repl_bang_equal: ("!=\n", TokenType::BangEqual, "!=", None),
        repl_equal: ("=\n", TokenType::Equal, "=", None),
        repl_equal_equal: ("==\n", TokenType::EqualEqual, "==", None),
        repl_greater: (">\n", TokenType::Greater, ">", None),
        repl_greater_equal: (">=\n", TokenType::GreaterEqual, ">=", None),
        repl_less: ("<\n", TokenType::Less, "<", None),
        repl_less_equal: ("<=\n", TokenType::LessEqual, "<=", None),

        // Number literals
        repl_zero_int: ("0\n", TokenType::Number, "0", Some(Literal::Number(0.0))),
        repl_zero_float: ("0.0\n", TokenType::Number, "0.0", Some(Literal::Number(0.0))),
        repl_one_int: ("1\n", TokenType::Number, "1", Some(Literal::Number(1.0))),
        repl_one_float: ("1.0\n", TokenType::Number, "1.0", Some(Literal::Number(1.0))),
        repl_large_int: ("999999\n", TokenType::Number, "999999", Some(Literal::Number(999999.0))),
        repl_large_float: ("999999.0\n", TokenType::Number, "999999.0", Some(Literal::Number(999999.0))),
        repl_small_float: ("0.0000001\n", TokenType::Number, "0.0000001", Some(Literal::Number(0.0000001))),

        // String literals

        // Identifier literals
        repl_lowercase_single_identifier: ("a\n", TokenType::Identifier, "a", Some(Literal::Identifier("a".to_string()))),
        repl_uppercase_single_identifier: ("A\n", TokenType::Identifier, "A", Some(Literal::Identifier("A".to_string()))),
        repl_underscore_single_identifier: ("_\n", TokenType::Identifier, "_", Some(Literal::Identifier("_".to_string()))),
        repl_lowercase_multiple_identifier: ("abyz\n", TokenType::Identifier, "abyz", Some(Literal::Identifier("abyz".to_string()))),
        repl_uppercase_multiple_identifier: ("ABYZ\n", TokenType::Identifier, "ABYZ", Some(Literal::Identifier("ABYZ".to_string()))),
        repl_underscore_multiple_identifier: ("_abyz_ABYZ\n", TokenType::Identifier, "_abyz_ABYZ", Some(Literal::Identifier("_abyz_ABYZ".to_string()))),

        // Keywords
        repl_and: ("and\n", TokenType::And, "and", None),
        repl_class: ("class\n", TokenType::Class, "class", None),
        repl_else: ("else\n", TokenType::Else, "else", None),
        repl_false: ("false\n", TokenType::False, "false", None),
        repl_for: ("for\n", TokenType::For, "for", None),
        repl_fun: ("fun\n", TokenType::Fun, "fun", None),
        repl_if: ("if\n", TokenType::If, "if", None),
        repl_nil: ("nil\n", TokenType::Nil, "nil", None),
        repl_or: ("or\n", TokenType::Or, "or", None),
        repl_print: ("print\n", TokenType::Print, "print", None),
        repl_return: ("return\n", TokenType::Return, "return", None),
        repl_super: ("super\n", TokenType::Super, "super", None),
        repl_this: ("this\n", TokenType::This, "this", None),
        repl_true: ("true\n", TokenType::True, "true", None),
        repl_var: ("var\n", TokenType::Var, "var", None),
        repl_while: ("while\n", TokenType::While, "while", None),
    }
}
