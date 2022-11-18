use std::fmt;

use crate::token::{Token, TokenType};

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub token: Token,
    pub message: Option<String>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let location = if self.token.token_type == TokenType::EndOfFile {
            "end".to_string()
        } else {
            let mut location = "'".to_string();
            location.push_str(&self.token.lexeme);
            location.push('\'');
            location
        };
        match &self.message {
            Some(message) => write!(
                f,
                "[line {}] Error{}: {:?}.",
                self.token.line, location, message,
            ),
            None => write!(f, "[line {}] Error{}", self.token.line, location),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[Rimetime Error]: {}", self.message)
    }
}

pub fn error(line: usize, message: &String) {
    report(line, &String::new(), message);
}

fn report(line: usize, location: &String, message: &String) -> bool {
    eprintln!("[line {}] Error{}: {}", line, location, message);
    true
}
