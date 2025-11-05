// src/parser.rs
use crate::error::ParseError;
use crate::token::{Token, TokenType};
use crate::tokenizer::Tokenizer;
use std::iter::Peekable;

// --- 6. "True" Streaming Parser (Stage 15) ---

#[derive(Debug, PartialEq, Clone)]
pub enum ParserEvent {
    StartObject, // {
    EndObject,   // }
    StartArray,  // [
    EndArray,    // ]
    Key(String), // "key" (in an object)
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
enum ParserState {
    ExpectValue,
    ExpectArrayFirstValueOrEnd, // After '[' - expect value or ']' (empty array)
    ExpectArrayValue,           // After ',' in array - expect value (no ']' allowed)
    ExpectArrayCommaOrEnd,      // After value in array - expect ',' or ']'
    ExpectObjectFirstKeyOrEnd,  // After '{' - expect key or '}' (empty object)
    ExpectObjectKey,            // After ',' in object - expect key (no '}' allowed)
    ExpectObjectColon,          // After key - expect ':'
    ExpectObjectValue,          // After ':' - expect value
    ExpectObjectCommaOrEnd,     // After value in object - expect ',' or '}'
}

pub struct StreamingParser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
    state_stack: Vec<ParserState>,
    max_depth: usize,
    depth: usize, // Track current nesting depth
}

impl<'a> StreamingParser<'a> {
    pub fn new(input: &'a str, max_depth: usize) -> Self {
        StreamingParser {
            tokenizer: Tokenizer::new(input).peekable(),
            state_stack: vec![ParserState::ExpectValue],
            max_depth,
            depth: 0,
        }
    }

    fn error_from_token(&self, message: String, token: &Token) -> ParseError {
        ParseError {
            message,
            line: token.line,
            column: token.column,
        }
    }
}

impl<'a> Iterator for StreamingParser<'a> {
    type Item = Result<ParserEvent, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token_result = self.tokenizer.next();

        let mut current_token = match token_result {
            Some(Ok(token)) => Some(token),
            Some(Err(e)) => return Some(Err(e)),
            None => None,
        };

        loop {
            let state_tuple = (current_token.as_ref(), self.state_stack.last());

            let (token, state) = match state_tuple {
                (Some(token), Some(state)) => (token, state.clone()),
                (None, Some(state)) => {
                    if *state == ParserState::ExpectValue && self.state_stack.len() == 1 {
                        return None;
                    }
                    return Some(Err(ParseError {
                        message: "Unexpected end of input, unclosed structure".to_string(),
                        line: 0,
                        column: 0,
                    }));
                }
                (None, None) => return None,
                (Some(token), None) => {
                    return Some(Err(
                        self.error_from_token("Unexpected trailing token".to_string(), token)
                    ));
                }
            };

            let result = match (state, &token.kind) {
                // --- Root level or nested value expected ---
                (ParserState::ExpectValue, TokenType::LeftBracket) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    self.state_stack.pop();
                    self.state_stack
                        .push(ParserState::ExpectArrayFirstValueOrEnd);
                    Ok(Some(ParserEvent::StartArray))
                }
                (ParserState::ExpectValue, TokenType::LeftBrace) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    self.state_stack.pop();
                    self.state_stack
                        .push(ParserState::ExpectObjectFirstKeyOrEnd);
                    Ok(Some(ParserEvent::StartObject))
                }
                (ParserState::ExpectValue, TokenType::String(s)) => {
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::String(s.clone())))
                }
                (ParserState::ExpectValue, TokenType::Number(n)) => {
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::Number(*n)))
                }
                (ParserState::ExpectValue, TokenType::Boolean(b)) => {
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::Boolean(*b)))
                }
                (ParserState::ExpectValue, TokenType::Null) => {
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::Null))
                }
                (ParserState::ExpectValue, _) => {
                    Err(self.error_from_token("Expected a value".to_string(), token))
                }

                // --- Inside Array: expecting first value or ']' (empty array) ---
                (ParserState::ExpectArrayFirstValueOrEnd, TokenType::RightBracket) => {
                    self.depth -= 1;
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::EndArray))
                }
                (ParserState::ExpectArrayFirstValueOrEnd, TokenType::LeftBracket) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    self.state_stack
                        .push(ParserState::ExpectArrayFirstValueOrEnd);
                    Ok(Some(ParserEvent::StartArray))
                }
                (ParserState::ExpectArrayFirstValueOrEnd, TokenType::LeftBrace) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    self.state_stack
                        .push(ParserState::ExpectObjectFirstKeyOrEnd);
                    Ok(Some(ParserEvent::StartObject))
                }
                (ParserState::ExpectArrayFirstValueOrEnd, TokenType::String(s)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::String(s.clone())))
                }
                (ParserState::ExpectArrayFirstValueOrEnd, TokenType::Number(n)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::Number(*n)))
                }
                (ParserState::ExpectArrayFirstValueOrEnd, TokenType::Boolean(b)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::Boolean(*b)))
                }
                (ParserState::ExpectArrayFirstValueOrEnd, TokenType::Null) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::Null))
                }
                (ParserState::ExpectArrayFirstValueOrEnd, _) => {
                    Err(self.error_from_token("Expected value or ']'".to_string(), token))
                }

                // --- Inside Array: after comma, expecting value (no ']' allowed) ---
                (ParserState::ExpectArrayValue, TokenType::LeftBracket) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    self.state_stack
                        .push(ParserState::ExpectArrayFirstValueOrEnd);
                    Ok(Some(ParserEvent::StartArray))
                }
                (ParserState::ExpectArrayValue, TokenType::LeftBrace) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    self.state_stack
                        .push(ParserState::ExpectObjectFirstKeyOrEnd);
                    Ok(Some(ParserEvent::StartObject))
                }
                (ParserState::ExpectArrayValue, TokenType::String(s)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::String(s.clone())))
                }
                (ParserState::ExpectArrayValue, TokenType::Number(n)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::Number(*n)))
                }
                (ParserState::ExpectArrayValue, TokenType::Boolean(b)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::Boolean(*b)))
                }
                (ParserState::ExpectArrayValue, TokenType::Null) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayCommaOrEnd;
                    Ok(Some(ParserEvent::Null))
                }
                (ParserState::ExpectArrayValue, TokenType::RightBracket) => {
                    Err(self.error_from_token("Unexpected ']'".to_string(), token))
                }
                (ParserState::ExpectArrayValue, _) => {
                    Err(self.error_from_token("Expected a value".to_string(), token))
                }

                // --- Inside Array: after a value, expecting ',' or ']' ---
                (ParserState::ExpectArrayCommaOrEnd, TokenType::Comma) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayValue;
                    Ok(None)
                }
                (ParserState::ExpectArrayCommaOrEnd, TokenType::RightBracket) => {
                    self.depth -= 1;
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::EndArray))
                }
                (ParserState::ExpectArrayCommaOrEnd, _) => {
                    Err(self.error_from_token("Expected ',' or ']'".to_string(), token))
                }

                // --- Inside Object: expecting first key or '}' (empty object) ---
                (ParserState::ExpectObjectFirstKeyOrEnd, TokenType::String(s)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectColon;
                    Ok(Some(ParserEvent::Key(s.clone())))
                }
                (ParserState::ExpectObjectFirstKeyOrEnd, TokenType::RightBrace) => {
                    self.depth -= 1;
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::EndObject))
                }
                (ParserState::ExpectObjectFirstKeyOrEnd, _) => {
                    Err(self.error_from_token("Expected '}' or a string key".to_string(), token))
                }

                // --- Inside Object: after comma, expecting key (no '}' allowed) ---
                (ParserState::ExpectObjectKey, TokenType::String(s)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectColon;
                    Ok(Some(ParserEvent::Key(s.clone())))
                }
                (ParserState::ExpectObjectKey, TokenType::RightBrace) => {
                    Err(self.error_from_token("Expected '}' or a string key".to_string(), token))
                }
                (ParserState::ExpectObjectKey, _) => {
                    Err(self.error_from_token("Expected a string key".to_string(), token))
                }

                // --- Inside Object: after key, expecting ':' ---
                (ParserState::ExpectObjectColon, TokenType::Colon) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectValue;
                    Ok(None)
                }
                (ParserState::ExpectObjectColon, _) => {
                    Err(self.error_from_token("Expected ':'".to_string(), token))
                }

                // --- Inside Object: after ':', expecting value ---
                (ParserState::ExpectObjectValue, TokenType::LeftBracket) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectCommaOrEnd;
                    self.state_stack
                        .push(ParserState::ExpectArrayFirstValueOrEnd);
                    Ok(Some(ParserEvent::StartArray))
                }
                (ParserState::ExpectObjectValue, TokenType::LeftBrace) => {
                    if self.depth >= self.max_depth {
                        return Some(Err(self.error_from_token(
                            "Maximum nesting depth exceeded".to_string(),
                            token,
                        )));
                    }
                    self.depth += 1;
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectCommaOrEnd;
                    self.state_stack
                        .push(ParserState::ExpectObjectFirstKeyOrEnd);
                    Ok(Some(ParserEvent::StartObject))
                }
                (ParserState::ExpectObjectValue, TokenType::String(s)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectCommaOrEnd;
                    Ok(Some(ParserEvent::String(s.clone())))
                }
                (ParserState::ExpectObjectValue, TokenType::Number(n)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectCommaOrEnd;
                    Ok(Some(ParserEvent::Number(*n)))
                }
                (ParserState::ExpectObjectValue, TokenType::Boolean(b)) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectCommaOrEnd;
                    Ok(Some(ParserEvent::Boolean(*b)))
                }
                (ParserState::ExpectObjectValue, TokenType::Null) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectCommaOrEnd;
                    Ok(Some(ParserEvent::Null))
                }
                (ParserState::ExpectObjectValue, _) => {
                    Err(self.error_from_token("Expected a value".to_string(), token))
                }

                // --- Inside Object: after value, expecting ',' or '}' ---
                (ParserState::ExpectObjectCommaOrEnd, TokenType::Comma) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectKey;
                    Ok(None)
                }
                (ParserState::ExpectObjectCommaOrEnd, TokenType::RightBrace) => {
                    self.depth -= 1;
                    self.state_stack.pop();
                    Ok(Some(ParserEvent::EndObject))
                }
                (ParserState::ExpectObjectCommaOrEnd, _) => {
                    Err(self.error_from_token("Expected ',' or '}'".to_string(), token))
                }
            };

            match result {
                Ok(Some(event)) => {
                    return Some(Ok(event));
                }
                Ok(None) => {
                    current_token = match self.tokenizer.next() {
                        Some(Ok(token)) => Some(token),
                        Some(Err(e)) => return Some(Err(e)),
                        None => None,
                    };
                    continue;
                }
                Err(e) => return Some(Err(e)),
            }
        }
    }
}
