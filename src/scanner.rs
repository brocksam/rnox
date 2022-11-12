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
            Literal::String(String::from("")),
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

    fn is_digit(&self, c: char) -> bool {
        match c {
            '0'..='9' => true,
            _ => false,
        }
    }

    fn is_alpha(&self, c: char) -> bool {
        match c {
            'a'..='z' | 'A'..='Z' | '_' => true,
            _ => false,
        }
    }

    fn is_alpha_numeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn identifier(&mut self) {
        let c = self.peek();
        while self.is_alpha_numeric(c) {
            self.advance();
            let c = self.peek();
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
        self.add_token_without_literal(token_type);
    }

    fn number(&mut self) {
        let c = self.peek();
        while self.is_digit(c) {
            self.advance();
            let c = self.peek();
        }
        let next_c = self.peek_next();
        if self.peek() == '.' && self.is_digit(next_c) {
            self.advance();
            let c = self.peek();
            while self.is_digit(c) {
                self.advance();
                let c = self.peek();
            }
        }
        self.add_token(TokenType::Number, Literal::Number(self.source[self.start..self.current].parse().unwrap()));

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
        self.add_token(TokenType::String, Literal::String(value.to_string()));
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

    fn peek(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source.chars().nth(self.current).unwrap()
    }

    fn peek_next(&mut self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn add_token_without_literal(&mut self, token_type: TokenType) {
        // TODO: add Literal value to represent to literal value
        self.add_token(token_type, Literal::String(String::new()));
    }

    fn add_token(&mut self, token_type: TokenType, literal: Literal) {
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
                    let (source, token_type, lexeme) = $value;
                    let mut scanner = Scanner::new(source.to_string());
                    let tokens = scanner.scan_tokens();
                    let expected = vec![
                        Token::new(
                            token_type,
                            lexeme.to_string(),
                            Literal::String("".to_string()),
                            1,
                        ),
                        Token::new(
                            TokenType::EndOfFile,
                            "".to_string(),
                            Literal::String("".to_string()),
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
        repl_left_paren: ("(\n", TokenType::LeftParen, "("),
        repl_right_paren: (")\n", TokenType::RightParen, ")"),
        repl_left_brace: ("{\n", TokenType::LeftBrace, "{"),
        repl_right_brace: ("}\n", TokenType::RightBrace, "}"),
        repl_comma: (",\n", TokenType::Comma, ","),
        repl_dot: (".\n", TokenType::Dot, "."),
        repl_minus: ("-\n", TokenType::Minus, "-"),
        repl_plus: ("+\n", TokenType::Plus, "+"),
        repl_semicolon: (";\n", TokenType::Semicolon, ";"),
        repl_slash: ("/\n", TokenType::Slash, "/"),
        repl_star: ("*\n", TokenType::Star, "*"),

        // One or two character tokens
        repl_bang: ("!\n", TokenType::Bang, "!"),
        repl_bang_equal: ("!=\n", TokenType::BangEqual, "!="),
        repl_equal: ("=\n", TokenType::Equal, "="),
        repl_equal_equal: ("==\n", TokenType::EqualEqual, "=="),
        repl_greater: (">\n", TokenType::Greater, ">"),
        repl_greater_equal: (">=\n", TokenType::GreaterEqual, ">="),
        repl_less: ("<\n", TokenType::Less, "<"),
        repl_less_equal: ("<=\n", TokenType::LessEqual, "<="),

        // Keywords
        repl_and: ("and\n", TokenType::And, "and"),
        repl_class: ("class\n", TokenType::Class, "class"),
        repl_else: ("else\n", TokenType::Else, "else"),
        repl_false: ("false\n", TokenType::False, "false"),
        repl_for: ("for\n", TokenType::For, "for"),
        repl_fun: ("fun\n", TokenType::Fun, "fun"),
        repl_if: ("if\n", TokenType::If, "if"),
        repl_nil: ("nil\n", TokenType::Nil, "nil"),
        repl_or: ("or\n", TokenType::Or, "or"),
        repl_print: ("print\n", TokenType::Print, "print"),
        repl_return: ("return\n", TokenType::Return, "return"),
        repl_super: ("super\n", TokenType::Super, "super"),
        repl_this: ("this\n", TokenType::This, "this"),
        repl_true: ("true\n", TokenType::True, "true"),
        repl_var: ("var\n", TokenType::Var, "var"),
        repl_while: ("while\n", TokenType::While, "while"),
    }
}
