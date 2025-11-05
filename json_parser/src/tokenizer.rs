// src/tokenizer.rs
use crate::error::ParseError;
use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

// --- 4. Tokenizer ---
pub(crate) struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Tokenizer {
            input: input.chars().peekable(),
            line: 1,
            column: 1,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        let char = self.input.next();
        if let Some(c) = char {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        char
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn error(&self, message: String) -> ParseError {
        ParseError {
            message,
            line: self.line,
            column: self.column,
        }
    }

    fn lex_literal(
        &mut self,
        expected: &'static str,
        kind: TokenType,
    ) -> Result<TokenType, ParseError> {
        for expected_char in expected.chars() {
            if self.next_char() != Some(expected_char) {
                return Err(self.error(format!("Expected '{}'", expected)));
            }
        }
        Ok(kind)
    }

    fn lex_string(&mut self) -> Result<TokenType, ParseError> {
        self.next_char(); // Consume opening '"'
        let mut parsed_content = String::new();
        while let Some(c) = self.next_char() {
            match c {
                '\\' => {
                    if let Some(escaped_char) = self.next_char() {
                        match escaped_char {
                            '"' | '\\' | '/' => parsed_content.push(escaped_char),
                            'b' => parsed_content.push('\u{0008}'),
                            'f' => parsed_content.push('\u{000C}'),
                            'n' => parsed_content.push('\n'),
                            'r' => parsed_content.push('\r'),
                            't' => parsed_content.push('\t'),
                            'u' => {
                                let mut hex_code = String::with_capacity(4);
                                for _ in 0..4 {
                                    if let Some(hex_char) = self.next_char() {
                                        if hex_char.is_ascii_hexdigit() {
                                            hex_code.push(hex_char);
                                        } else {
                                            return Err(
                                                self.error("Non-hex char in Unicode".to_string())
                                            );
                                        }
                                    } else {
                                        return Err(self.error("Incomplete Unicode".to_string()));
                                    }
                                }
                                let code = u32::from_str_radix(&hex_code, 16).unwrap();
                                parsed_content.push(std::char::from_u32(code).unwrap());
                            }
                            _ => return Err(self.error("Invalid escape sequence".to_string())),
                        }
                    } else {
                        return Err(self.error("Unterminated string".to_string()));
                    }
                }
                '"' => return Ok(TokenType::String(parsed_content)),
                '\u{0000}'..='\u{001F}' => {
                    return Err(self.error("Unescaped control character in string".to_string()));
                }
                _ => parsed_content.push(c),
            }
        }
        Err(self.error("Unterminated string".to_string()))
    }

    fn lex_number(&mut self) -> Result<TokenType, ParseError> {
        let mut num_str = String::new();
        num_str.push(self.next_char().unwrap());
        while let Some(&c) = self.peek() {
            match c {
                '0'..='9' | '.' | 'e' | 'E' | '+' | '-' => {
                    num_str.push(self.next_char().unwrap());
                }
                _ => break,
            }
        }
        if num_str.starts_with('0') && num_str.len() > 1 {
            if let Some(second_char) = num_str.chars().nth(1) {
                if second_char.is_ascii_digit() {
                    return Err(self.error("Invalid number: leading zeros not allowed".to_string()));
                }
            }
        }
        if num_str.ends_with('.') {
            return Err(self.error("Invalid number: cannot end with a decimal point".to_string()));
        }
        if let Some(e_pos) = num_str.find(['e', 'E']) {
            if e_pos > 0 && num_str.chars().nth(e_pos - 1) == Some('.') {
                return Err(self.error(format!("Invalid number '{}'", num_str)));
            }
        }
        if let Some(last_char) = num_str.chars().last() {
            if last_char == 'e' || last_char == 'E' || last_char == '+' || last_char == '-' {
                return Err(self.error(format!("Invalid number '{}'", num_str)));
            }
        }
        match num_str.parse::<f64>() {
            Ok(num) => Ok(TokenType::Number(num)),
            Err(_) => Err(self.error(format!("Invalid number '{}'", num_str))),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (start_line, start_column) = (self.line, self.column);
            let next_char = match self.peek() {
                Some(c) => *c,
                None => return None,
            };
            let token_kind_result = match next_char {
                ' ' | '\t' | '\r' | '\n' => {
                    self.next_char();
                    continue;
                }
                '{' => {
                    self.next_char();
                    Ok(TokenType::LeftBrace)
                }
                '}' => {
                    self.next_char();
                    Ok(TokenType::RightBrace)
                }
                '[' => {
                    self.next_char();
                    Ok(TokenType::LeftBracket)
                }
                ']' => {
                    self.next_char();
                    Ok(TokenType::RightBracket)
                }
                ':' => {
                    self.next_char();
                    Ok(TokenType::Colon)
                }
                ',' => {
                    self.next_char();
                    Ok(TokenType::Comma)
                }
                'n' => self.lex_literal("null", TokenType::Null),
                't' => self.lex_literal("true", TokenType::Boolean(true)),
                'f' => self.lex_literal("false", TokenType::Boolean(false)),
                '"' => self.lex_string(),
                '-' | '0'..='9' => self.lex_number(),
                '/' => Err(self.error("Comments are not allowed in JSON".to_string())),
                _ => Err(self.error(format!("Unexpected character '{}'", next_char))),
            };
            let token_result = token_kind_result.map(|kind| Token {
                kind,
                line: start_line,
                column: start_column,
            });
            return Some(token_result);
        }
    }
}
