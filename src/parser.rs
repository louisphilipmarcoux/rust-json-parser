//! Contains the `StreamingParser` and its state machine.
//!
//! This module defines the core logic of the parser, which is implemented
//! as a state machine that consumes `Token`s from the `Tokenizer` and
//! emits `ParserEvent`s.

use crate::error::ParseError;
use crate::token::{Token, TokenType};
use crate::tokenizer::Tokenizer;
use crate::value::JsonNumber;
use std::borrow::Cow;
use std::iter::Peekable;

/// A single event emitted by the `StreamingParser`.
///
/// The parser is an `Iterator` that yields these events, allowing you
/// to react to JSON data as it's being parsed without loading the
/// entire structure into memory.
#[derive(Debug, PartialEq, Clone)]
pub enum ParserEvent<'a> {
    /// The start of a JSON object (`{`).
    StartObject,
    /// The end of a JSON object (`}`).
    EndObject,
    /// The start of a JSON array (`[`).
    StartArray,
    /// The end of a JSON array (`]`).
    EndArray,
    /// A JSON object key (e.g., `"name": ...`).
    Key(Cow<'a, str>),
    /// A JSON string value (e.g., `... : "value"`).
    String(Cow<'a, str>),
    /// A JSON number value (e.g., `123`, `-0.5`, `1e10`).
    Number(JsonNumber),
    /// A JSON boolean value (`true` or `false`).
    Boolean(bool),
    /// A JSON `null` value.
    Null,
}

/// Internal state machine for the parser.
///
/// This enum tracks what the parser *expects* to see next,
/// allowing it to enforce JSON grammar rules (e.g., "a comma or
/// closing bracket must follow a value in an array").
#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::enum_variant_names)]
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

/// The main streaming JSON parser.
///
/// This struct is an `Iterator` that yields `Result<ParserEvent, ParseError>`.
/// It is created by the `parse_streaming` function.
pub struct StreamingParser<'a> {
    /// The internal tokenizer (lexer) that breaks the input string into `Token`s.
    tokenizer: Peekable<Tokenizer<'a>>,
    /// A stack of states, used for tracking nested objects and arrays.
    state_stack: Vec<ParserState>,
    /// The maximum allowed nesting depth to prevent DoS attacks.
    max_depth: usize,
    /// The *current* nesting depth of the parser.
    depth: usize,
}

impl<'a> StreamingParser<'a> {
    /// Creates a new `StreamingParser` for a given input string.
    ///
    /// This is called by the `parse_streaming` function in `lib.rs`.
    pub fn new(input: &'a str, max_depth: usize) -> Self {
        StreamingParser {
            tokenizer: Tokenizer::new(input).peekable(),
            state_stack: vec![ParserState::ExpectValue],
            max_depth,
            depth: 0,
        }
    }

    /// A helper function to create a `ParseError` from a token's location.
    fn error_from_token(&self, message: String, token: &Token<'a>) -> ParseError {
        ParseError {
            message,
            line: token.line,
            column: token.column,
        }
    }
}

/// The main implementation of the parser's `Iterator` trait.
/// This is where the state machine logic lives.
impl<'a> Iterator for StreamingParser<'a> {
    type Item = Result<ParserEvent<'a>, ParseError>;

    /// Consumes the next token and advances the parser's state.
    fn next(&mut self) -> Option<Self::Item> {
        // Get the next token from the tokenizer.
        let token_result = self.tokenizer.next();

        let mut current_token = match token_result {
            Some(Ok(token)) => Some(token),
            Some(Err(e)) => return Some(Err(e)), // Tokenizer error
            None => None,                        // End of input
        };

        // Loop handles "non-event" tokens (like `,` or `:`)
        // that advance the state but don't emit a `ParserEvent`.
        loop {
            let state_tuple = (current_token.as_ref(), self.state_stack.last());

            // Determine the current token and state.
            let (token, state) = match state_tuple {
                (Some(token), Some(state)) => (token, state.clone()),
                // End of input, but we're still in a nested state.
                (None, Some(state)) => {
                    if *state == ParserState::ExpectValue && self.state_stack.len() == 1 {
                        // We expected a value, but got clean EOF. Valid for empty input.
                        // The tokenizer.next() would have returned None, so we'd be in (None, None).
                        // This path is for cases like `[1,` and then EOF.
                        // A clean EOF on ExpectValue (e.g. empty string) is handled by (None, None)
                    }
                    // Handle clean EOF on empty input
                    if *state == ParserState::ExpectValue && self.state_stack.len() == 1 {
                        if let Some(_tok) = &current_token {
                            // This case should not be hit if current_token is None
                        } else {
                            // This means tokenizer.next() returned None *and* state is ExpectValue
                            return None;
                        }
                    }

                    // Return a more specific error message based on the parser's state
                    let msg = match state {
                        ParserState::ExpectObjectCommaOrEnd
                        | ParserState::ExpectObjectFirstKeyOrEnd
                        | ParserState::ExpectObjectKey
                        | ParserState::ExpectObjectColon
                        | ParserState::ExpectObjectValue => "Unclosed object",
                        ParserState::ExpectArrayCommaOrEnd
                        | ParserState::ExpectArrayFirstValueOrEnd
                        | ParserState::ExpectArrayValue => "Unclosed array",
                        _ => "Unexpected end of input, unclosed structure",
                    };

                    return Some(Err(ParseError {
                        message: msg.to_string(),
                        line: 0, // We don't have a token for location info
                        column: 0,
                    }));
                }
                (None, None) => return None, // Clean end
                // We have a token, but the state stack is empty (parser finished).
                (Some(token), None) => {
                    return Some(Err(
                        self.error_from_token("Unexpected trailing token".to_string(), token)
                    ));
                }
            };

            // This is the main state machine logic.
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
                // Check for invalid trailing comma `[1,,2]`
                (ParserState::ExpectArrayValue, TokenType::RightBracket) => {
                    Err(self
                        .error_from_token("Unexpected ']', expected a value".to_string(), token))
                }
                (ParserState::ExpectArrayValue, _) => {
                    Err(self.error_from_token("Expected a value".to_string(), token))
                }

                // --- Inside Array: after a value, expecting ',' or ']' ---
                (ParserState::ExpectArrayCommaOrEnd, TokenType::Comma) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectArrayValue;
                    Ok(None) // Comma is consumed, state changes, but no event emitted.
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
                // Check for invalid trailing comma `{"key":1,}`
                (ParserState::ExpectObjectKey, TokenType::RightBrace) => Err(self
                    .error_from_token("Unexpected '}', expected a string key".to_string(), token)),
                (ParserState::ExpectObjectKey, _) => {
                    Err(self.error_from_token("Expected a string key".to_string(), token))
                }

                // --- Inside Object: after key, expecting ':' ---
                (ParserState::ExpectObjectColon, TokenType::Colon) => {
                    *self.state_stack.last_mut().unwrap() = ParserState::ExpectObjectValue;
                    Ok(None) // Colon is consumed, state changes, no event emitted.
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
                    Ok(None) // Comma consumed, state changes, no event.
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

            // Handle the result of the `match` expression
            match result {
                Ok(Some(event)) => {
                    // We have an event to emit. Return it.
                    return Some(Ok(event));
                }
                Ok(None) => {
                    // This was a non-event token (like `,` or `:`).
                    // We loop again to get the *next* token.
                    current_token = match self.tokenizer.next() {
                        Some(Ok(token)) => Some(token),
                        Some(Err(e)) => return Some(Err(e)),
                        None => None,
                    };
                    continue;
                }
                Err(e) => {
                    // A parsing error occurred.
                    return Some(Err(e));
                }
            }
        }
    }
}
