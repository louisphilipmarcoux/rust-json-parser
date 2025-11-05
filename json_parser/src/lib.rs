use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::iter::Peekable;
use std::str::Chars;

// --- Constants ---
const DEFAULT_MAX_DEPTH: usize = 100;
const MAX_JSON_SIZE_BYTES: usize = 10 * 1024 * 1024;

// --- 2. Token Structs ---
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    Comma,        // ,
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    kind: TokenType,
    line: usize,
    column: usize,
}

// --- 3. Error Type ---
#[derive(Debug, PartialEq)]
pub struct ParseError {
    message: String,
    line: usize,
    column: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error: {} at line {}, column {}.",
            self.message, self.line, self.column
        )
    }
}

// --- 4. Tokenizer ---
struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a str) -> Self {
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

// --- 5. JSON Value Enum (for Stage 16) ---
// This enum represents the "native data structures" (like maps and lists)
// that Stage 16 asks us to serialize.
#[derive(Debug, PartialEq, Clone)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

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

// --- 7. Stringify (Serialization - Stage 16) ---

impl JsonValue {
    /// Serializes the JsonValue into a compact JSON string,
    /// as required by Stage 16.
    pub fn stringify(&self) -> String {
        let mut output = String::new();
        // We use a helper that writes to a String.
        // This unwrap is safe because writing to a String never fails.
        Self::write_value(self, &mut output).unwrap();
        output
    }

    /// Recursive helper function to write a value.
    /// Uses fmt::Write for efficient string building.
    fn write_value<W: fmt::Write>(value: &JsonValue, w: &mut W) -> fmt::Result {
        match value {
            JsonValue::Null => w.write_str("null"),
            JsonValue::Boolean(b) => w.write_str(if *b { "true" } else { "false" }),
            JsonValue::Number(n) => write!(w, "{}", n),
            JsonValue::String(s) => Self::write_string(s, w),
            JsonValue::Array(a) => Self::write_array(a, w),
            JsonValue::Object(o) => Self::write_object(o, w),
        }
    }

    /// Helper to write a JSON array.
    fn write_array<W: fmt::Write>(arr: &Vec<JsonValue>, w: &mut W) -> fmt::Result {
        w.write_char('[')?;
        let mut first = true;
        for val in arr {
            if !first {
                w.write_char(',')?;
            }
            Self::write_value(val, w)?;
            first = false;
        }
        w.write_char(']')
    }

    /// Helper to write a JSON object.
    fn write_object<W: fmt::Write>(obj: &HashMap<String, JsonValue>, w: &mut W) -> fmt::Result {
        w.write_char('{')?;
        let mut first = true;
        // Note: HashMap iteration order is not guaranteed,
        // but this is fine according to the JSON specification.
        for (key, val) in obj {
            if !first {
                w.write_char(',')?;
            }
            Self::write_string(key, w)?; // Write the key (which must be a string)
            w.write_char(':')?;
            Self::write_value(val, w)?; // Write the value
            first = false;
        }
        w.write_char('}')
    }

    /// Helper to write an escaped JSON string.
    /// This handles escapes required by Stage 8 and Stage 16.
    fn write_string<W: fmt::Write>(s: &str, w: &mut W) -> fmt::Result {
        w.write_char('"')?;
        for c in s.chars() {
            match c {
                // Standard escapes
                '"' => w.write_str("\\\""),
                '\\' => w.write_str("\\\\"),
                '/' => w.write_str("\\/"), // Optional, but good practice
                '\u{0008}' => w.write_str("\\b"),
                '\u{000C}' => w.write_str("\\f"),
                '\n' => w.write_str("\\n"),
                '\r' => w.write_str("\\r"),
                '\t' => w.write_str("\\t"),
                // Control characters must be escaped
                '\u{0000}'..='\u{001F}' => {
                    write!(w, "\\u{:04x}", c as u32)
                }
                _ => w.write_char(c),
            }?;
        }
        w.write_char('"')
    }

    // --- Pretty Print Bonus ---

    /// The indentation string to use for pretty-printing.
    const INDENT: &'static str = "  ";

    /// Serializes the JsonValue into a human-readable,
    /// indented JSON string.
    pub fn stringify_pretty(&self) -> String {
        let mut output = String::new();
        // This unwrap is safe because writing to a String never fails.
        Self::write_value_pretty(self, &mut output, 0).unwrap();
        output
    }

    /// Recursive helper for pretty-printing a value.
    fn write_value_pretty<W: fmt::Write>(
        value: &JsonValue,
        w: &mut W,
        depth: usize,
    ) -> fmt::Result {
        match value {
            // Primitives are written the same as compact
            JsonValue::Null => w.write_str("null"),
            JsonValue::Boolean(b) => w.write_str(if *b { "true" } else { "false" }),
            JsonValue::Number(n) => write!(w, "{}", n),
            JsonValue::String(s) => Self::write_string(s, w),
            // Composites get new logic
            JsonValue::Array(a) => Self::write_array_pretty(a, w, depth),
            JsonValue::Object(o) => Self::write_object_pretty(o, w, depth),
        }
    }

    /// Helper to pretty-print a JSON array.
    fn write_array_pretty<W: fmt::Write>(
        arr: &Vec<JsonValue>,
        w: &mut W,
        depth: usize,
    ) -> fmt::Result {
        // Empty array is just "[]"
        if arr.is_empty() {
            return w.write_str("[]");
        }

        let new_depth = depth + 1;
        let indent = Self::INDENT.repeat(new_depth);
        let closing_indent = Self::INDENT.repeat(depth);

        w.write_str("[\n")?; // Opening bracket and newline

        let mut first = true;
        for val in arr {
            if !first {
                w.write_str(",\n")?; // Comma and newline before next item
            }
            w.write_str(&indent)?; // Indent
            Self::write_value_pretty(val, w, new_depth)?; // Write the value
            first = false;
        }

        write!(w, "\n{}", closing_indent)?; // Newline and closing indent
        w.write_char(']') // Closing bracket
    }

    /// Helper to pretty-print a JSON object.
    fn write_object_pretty<W: fmt::Write>(
        obj: &HashMap<String, JsonValue>,
        w: &mut W,
        depth: usize,
    ) -> fmt::Result {
        // Empty object is just "{}"
        if obj.is_empty() {
            return w.write_str("{}");
        }

        let new_depth = depth + 1;
        let indent = Self::INDENT.repeat(new_depth);
        let closing_indent = Self::INDENT.repeat(depth);

        w.write_str("{\n")?; // Opening brace and newline

        let mut first = true;
        for (key, val) in obj {
            if !first {
                w.write_str(",\n")?; // Comma and newline before next item
            }
            w.write_str(&indent)?; // Indent
            Self::write_string(key, w)?; // Write the key
            w.write_str(": ")?; // Colon and space
            Self::write_value_pretty(val, w, new_depth)?; // Write the value
            first = false;
        }

        write!(w, "\n{}", closing_indent)?; // Newline and closing indent
        w.write_char('}') // Closing brace
    }
}

// --- 8. Main Function ---
fn main() {
    let input = "{ \"key\": [1, true, null] }";
    println!("--- Running Streaming Parser ---");

    match parse_streaming(input) {
        Ok(mut parser) => {
            while let Some(event) = parser.next() {
                match event {
                    Ok(event) => println!("Event: {:?}", event),
                    Err(e) => {
                        println!("{}", e);
                        break;
                    }
                }
            }
        }
        Err(e) => println!("{}", e),
    }

    println!("\n--- Running Stringify Demo ---");
    let mut items = HashMap::new();
    items.insert("key".to_string(), JsonValue::String("value".to_string()));
    items.insert(
        "items".to_string(),
        JsonValue::Array(vec![JsonValue::Number(1.0), JsonValue::Null]),
    );
    let obj = JsonValue::Object(items);

    println!("Compact: {}", obj.stringify());
    println!("Pretty:\n{}", obj.stringify_pretty());
}

// --- 9. Public-facing helper function ---
pub fn parse_streaming(input: &'_ str) -> Result<StreamingParser<'_>, ParseError> {
    if input.len() > MAX_JSON_SIZE_BYTES {
        return Err(ParseError {
            message: "Input exceeds maximum size limit".to_string(),
            line: 1,
            column: 1,
        });
    }
    Ok(StreamingParser::new(input, DEFAULT_MAX_DEPTH))
}

// --- 10. Test Module ---
#[cfg(test)]
mod tests {
    use super::JsonValue; // Make sure to import JsonValue
    use super::*;
    use std::collections::HashMap; // And HashMap

    fn collect_events(input: &str) -> Result<Vec<ParserEvent>, ParseError> {
        parse_streaming(input)?.collect()
    }

    fn collect_events_with_depth(
        input: &str,
        depth: usize,
    ) -> Result<Vec<ParserEvent>, ParseError> {
        StreamingParser::new(input, depth).collect()
    }

    #[test]
    fn test_streaming_parser_simple() {
        let input = "{ \"key\": [1, null, true, \"hello\"] }";
        let events = collect_events(input).unwrap();

        assert_eq!(
            events,
            vec![
                ParserEvent::StartObject,
                ParserEvent::Key("key".to_string()),
                ParserEvent::StartArray,
                ParserEvent::Number(1.0),
                ParserEvent::Null,
                ParserEvent::Boolean(true),
                ParserEvent::String("hello".to_string()),
                ParserEvent::EndArray,
                ParserEvent::EndObject
            ]
        );
    }

    #[test]
    fn test_streaming_empty_array_object() {
        let input = "[ { } ]";
        let events = collect_events(input).unwrap();

        assert_eq!(
            events,
            vec![
                ParserEvent::StartArray,
                ParserEvent::StartObject,
                ParserEvent::EndObject,
                ParserEvent::EndArray
            ]
        );
    }

    #[test]
    fn test_streaming_errors() {
        let input = "[1 true]";
        let err = collect_events(input).unwrap_err();
        assert_eq!(err.message, "Expected ',' or ']'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 4);
    }

    #[test]
    fn test_streaming_object_errors() {
        let input = "{ : 1 }";
        let err = collect_events(input).unwrap_err();
        assert_eq!(err.message, "Expected '}' or a string key");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 3);

        let input = "{\"key\" 1}";
        let err = collect_events(input).unwrap_err();
        assert_eq!(err.message, "Expected ':'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 8);
    }

    #[test]
    fn test_streaming_tokenizer_errors() {
        let input = "[1, ?]";
        let err = collect_events(input).unwrap_err();
        assert_eq!(err.message, "Unexpected character '?'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 5);

        let input = "[1] [2]";
        let err = collect_events(input).unwrap_err();
        assert_eq!(err.message, "Unexpected trailing token");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 5);
    }

    #[test]
    fn test_streaming_rfc_8259_compliance() {
        // Trailing Commas
        let err = collect_events("[1, 2,]").unwrap_err();
        assert_eq!(err.message, "Unexpected ']'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 7);

        let err = collect_events("{\"key\": 1,}").unwrap_err();
        assert_eq!(err.message, "Expected '}' or a string key");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 11);

        let err = collect_events("// a comment\n[1, 2]").unwrap_err();
        assert_eq!(err.message, "Comments are not allowed in JSON");

        let err = collect_events("0123").unwrap_err();
        assert_eq!(err.message, "Invalid number: leading zeros not allowed");

        let err = collect_events("1.").unwrap_err();
        assert_eq!(
            err.message,
            "Invalid number: cannot end with a decimal point"
        );

        let err = collect_events(".5").unwrap_err();
        assert_eq!(err.message, "Unexpected character '.'");

        let err = collect_events("\"\n\"").unwrap_err();
        assert_eq!(err.message, "Unescaped control character in string");
        assert_eq!(err.line, 2);
        assert_eq!(err.column, 1);
    }

    #[test]
    fn test_streaming_security_limits() {
        let evil_input = "[".repeat(101) + &"]".repeat(101);
        let err = collect_events_with_depth(&evil_input, 100).unwrap_err();
        assert_eq!(err.message, "Maximum nesting depth exceeded");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 101);

        let ok_input = "[".repeat(100) + &"]".repeat(100);
        assert!(collect_events_with_depth(&ok_input, 100).is_ok());

        // Test size limit
        let small_input = "[1]";
        let err = parse_streaming(small_input);
        assert!(err.is_ok());
    }

    // --- Stage 16 Tests ---

    #[test]
    fn test_stringify_stage_16_examples() {
        // Test case from challenge:
        // Input: A native map {"key": "value", "items": [1, None]}
        // Output: The string {"key":"value","items":[1,null]}
        let mut items = HashMap::new();
        items.insert("key".to_string(), JsonValue::String("value".to_string()));
        items.insert(
            "items".to_string(),
            JsonValue::Array(vec![JsonValue::Number(1.0), JsonValue::Null]),
        );
        let obj = JsonValue::Object(items);

        // We must check both key orders since HashMap order is not guaranteed
        let output = obj.stringify();
        let expected1 = r#"{"key":"value","items":[1,null]}"#;
        let expected2 = r#"{"items":[1,null],"key":"value"}"#;

        assert!(
            output == expected1 || output == expected2,
            "Stringify output was: {}",
            output
        );

        // Test case from challenge:
        // Input: A native string a "quoted" \ string
        // Output: The string "a \"quoted\" \\ string"
        let s = JsonValue::String("a \"quoted\" \\ string".to_string());
        assert_eq!(s.stringify(), r#""a \"quoted\" \\ string""#);
    }

    #[test]
    fn test_stringify_all_types() {
        // Primitives
        assert_eq!(JsonValue::Null.stringify(), "null");
        assert_eq!(JsonValue::Boolean(true).stringify(), "true");
        assert_eq!(JsonValue::Boolean(false).stringify(), "false");
        assert_eq!(JsonValue::Number(123.45).stringify(), "123.45");
        assert_eq!(JsonValue::Number(-0.5).stringify(), "-0.5");
        assert_eq!(JsonValue::Number(1e+3).stringify(), "1000");

        // Empty Structures
        assert_eq!(JsonValue::Array(vec![]).stringify(), "[]");
        assert_eq!(JsonValue::Object(HashMap::new()).stringify(), "{}");

        // Complex Array
        let arr = JsonValue::Array(vec![
            JsonValue::Number(1.0),
            JsonValue::String("test".to_string()),
            JsonValue::Boolean(true),
            JsonValue::Null,
            JsonValue::Object(HashMap::new()),
        ]);
        assert_eq!(arr.stringify(), r#"[1,"test",true,null,{}]"#);
    }

    #[test]
    fn test_stringify_string_escapes() {
        // Test all escapes from Stage 8
        let s = JsonValue::String("\" \\ / \u{0008} \u{000C} \n \r \t".to_string());
        assert_eq!(s.stringify(), r#""\" \\ \/ \b \f \n \r \t""#);

        // Test control character escape
        let s_control = JsonValue::String("hello\u{0001}world".to_string());
        assert_eq!(s_control.stringify(), r#""hello\u0001world""#);
    }

    #[test]
    fn test_stringify_pretty_print() {
        let mut sub_obj = HashMap::new();
        sub_obj.insert("sub_key".to_string(), JsonValue::Number(2.0));

        let mut items = HashMap::new();
        items.insert("key".to_string(), JsonValue::String("value".to_string()));
        items.insert(
            "items".to_string(),
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::Null,
                JsonValue::Object(sub_obj),
            ]),
        );
        items.insert("admin".to_string(), JsonValue::Boolean(true));
        let obj = JsonValue::Object(items);

        let pretty_string = obj.stringify_pretty();

        // We can't test for an exact string match because HashMap
        // iteration order is not guaranteed.

        assert!(pretty_string.starts_with("{\n"));
        assert!(pretty_string.ends_with("\n}"));

        // --- FIXED LINES ---
        // Check for the content of the lines, but NOT the trailing comma,
        // because any of them could be the last item.
        assert!(pretty_string.contains("\n  \"key\": \"value\""));
        assert!(pretty_string.contains("\n  \"admin\": true"));
        // --- END FIX ---

        // This assertion is still correct because the value itself contains newlines
        assert!(pretty_string.contains("\n  \"items\": [\n"));

        // These assertions are also fine
        assert!(pretty_string.contains("\n    1,"));
        assert!(pretty_string.contains("\n    null,"));
        assert!(pretty_string.contains("\n    {\n"));
        assert!(pretty_string.contains("\n      \"sub_key\": 2\n"));
        assert!(pretty_string.contains("\n    }\n"));
        assert!(pretty_string.contains("\n  ]\n"));

        // Test empty object and array
        assert_eq!(JsonValue::Object(HashMap::new()).stringify_pretty(), "{}");
        assert_eq!(JsonValue::Array(vec![]).stringify_pretty(), "[]");
    }
}
