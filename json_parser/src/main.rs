use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

// --- 1. The final JSON Value Enum ---
#[derive(Debug, PartialEq, Clone)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

// --- 2. The new Token Structs ---
/// The *type* of token. This is what our old `Token` enum was.
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

/// A token, now with its location in the source.
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    kind: TokenType,
    line: usize,
    column: usize,
}

// --- 3. The new Error Type ---
/// A precise error, with a message and location.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    message: String,
    line: usize,
    column: usize,
}

/// Implement the Display trait to print nice error messages.
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error: {} at line {}, column {}.",
            self.message, self.line, self.column
        )
    }
}

// --- 4. The Tokenizer (Lexer) ---
/// A stateful tokenizer that tracks line and column.
struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    /// Creates a new tokenizer.
    fn new(input: &'a str) -> Self {
        Tokenizer {
            input: input.chars().peekable(),
            line: 1,
            column: 1,
        }
    }

    /// Consumes the next char and updates line/column.
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

    /// Peeks at the next char.
    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    /// Helper to create a ParseError at the current location.
    fn error(&self, message: String) -> ParseError {
        ParseError {
            message,
            line: self.line,
            column: self.column,
        }
    }

    /// Scans the input and produces a Vec of Tokens.
    fn tokenize(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut tokens = Vec::new();

        // Use a loop instead of `while let` to fix the borrow-checker issue
        loop {
            let (start_line, start_column) = (self.line, self.column);

            // Peek for the next character.
            let next_char = match self.peek() {
                Some(c) => *c,
                None => break, // End of input, break the loop
            };

            let token_kind = match next_char {
                // 1. Whitespace
                ' ' | '\t' | '\r' | '\n' => {
                    self.next_char(); // Consume whitespace
                    continue; // Skip to the next loop iteration
                }
                // 2. Single-char tokens
                '{' => {
                    self.next_char();
                    TokenType::LeftBrace
                }
                '}' => {
                    self.next_char();
                    TokenType::RightBrace
                }
                '[' => {
                    self.next_char();
                    TokenType::LeftBracket
                }
                ']' => {
                    self.next_char();
                    TokenType::RightBracket
                }
                ':' => {
                    self.next_char();
                    TokenType::Colon
                }
                ',' => {
                    self.next_char();
                    TokenType::Comma
                }

                // 3. Literals
                'n' => self.lex_literal("null", TokenType::Null)?,
                't' => self.lex_literal("true", TokenType::Boolean(true))?,
                'f' => self.lex_literal("false", TokenType::Boolean(false))?,

                // 4. Strings
                '"' => self.lex_string()?,

                // 5. Numbers
                '-' | '0'..='9' => self.lex_number()?,

                // 6. Unexpected
                _ => {
                    // This is now safe because the borrow from `peek()` is gone
                    return Err(self.error(format!("Unexpected character '{}'", next_char)));
                }
            };

            tokens.push(Token {
                kind: token_kind,
                line: start_line,
                column: start_column,
            });
        }
        Ok(tokens)
    }

    /// Helper for parsing 'null', 'true', 'false'
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

    /// Helper for parsing strings
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
                                // (Simplified Unicode, add surrogate logic back if needed)
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
                '"' => return Ok(TokenType::String(parsed_content)), // End of string
                _ => parsed_content.push(c),
            }
        }
        Err(self.error("Unterminated string".to_string())) // Unclosed string
    }

    /// Helper for parsing numbers (with Stage 10 validation)
    fn lex_number(&mut self) -> Result<TokenType, ParseError> {
        let mut num_str = String::new();

        // Consume the first char (which we know is '-' or a digit)
        num_str.push(self.next_char().unwrap());

        while let Some(&c) = self.peek() {
            match c {
                '0'..='9' | '.' | 'e' | 'E' | '+' | '-' => {
                    num_str.push(self.next_char().unwrap());
                }
                _ => break,
            }
        }

        // --- Stage 10 Validation ---
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
        // --- End Validation ---

        match num_str.parse::<f64>() {
            Ok(num) => Ok(TokenType::Number(num)),
            Err(_) => Err(self.error(format!("Invalid number '{}'", num_str))),
        }
    }
}

// --- 5. The Parser ---
/// Consumes `Token`s and produces a `JsonValue`.
pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Result<JsonValue, ParseError> {
        let value = self.parse_value()?;
        if self.position == self.tokens.len() {
            Ok(value)
        } else {
            // Error: Trailing tokens
            let token = self.peek()?;
            Err(self.error(format!("Unexpected token '{:?}'", token.kind), token))
        }
    }

    /// Helper to create a ParseError from a Token's location.
    fn error(&self, message: String, token: &Token) -> ParseError {
        ParseError {
            message,
            line: token.line,
            column: token.column,
        }
    }

    /// Helper to create an EOI error
    fn error_eoi(&self, message: String) -> ParseError {
        let (line, column) = if let Some(last_token) = self.tokens.last() {
            // Error position is *after* the last token
            (last_token.line, last_token.column + 1)
        } else {
            (1, 1) // Empty input
        };
        ParseError {
            message,
            line,
            column,
        }
    }

    fn parse_value(&mut self) -> Result<JsonValue, ParseError> {
        let token = self.consume()?.clone();
        match token.kind {
            TokenType::Null => Ok(JsonValue::Null),
            TokenType::Boolean(b) => Ok(JsonValue::Boolean(b)),
            TokenType::Number(n) => Ok(JsonValue::Number(n)),
            TokenType::String(s) => Ok(JsonValue::String(s)),
            TokenType::LeftBracket => self.parse_array(),
            TokenType::LeftBrace => self.parse_object(),
            _ => Err(self.error(
                format!("Expected a value, found '{:?}'", token.kind),
                &token,
            )),
        }
    }

    fn parse_array(&mut self) -> Result<JsonValue, ParseError> {
        let mut elements = Vec::new();

        let next_token = self.peek()?;
        if next_token.kind == TokenType::RightBracket {
            self.consume()?; // Consume ']'
            return Ok(JsonValue::Array(elements));
        }

        loop {
            elements.push(self.parse_value()?);
            let token = self.consume()?.clone();
            match token.kind {
                TokenType::Comma => continue,
                TokenType::RightBracket => break,
                _ => {
                    // --- FIX 1 ---
                    // The error message was wrong. We expected a comma or bracket.
                    return Err(self.error("Expected ',' or ']'".to_string(), &token));
                }
            }
        }
        Ok(JsonValue::Array(elements))
    }

    fn parse_object(&mut self) -> Result<JsonValue, ParseError> {
        let mut map = HashMap::new();

        let next_token = self.peek()?;
        if next_token.kind == TokenType::RightBrace {
            self.consume()?; // Consume '}'
            return Ok(JsonValue::Object(map));
        }

        loop {
            // 1. Parse Key
            let key_token = self.consume()?.clone();
            let key = match &key_token.kind {
                TokenType::String(s) => s.clone(),
                _ => {
                    // --- FIX 2 ---
                    // The error message was wrong. We expected a string key.
                    return Err(self.error("Object key must be a string".to_string(), &key_token));
                }
            };

            // 2. Expect Colon
            self.expect(TokenType::Colon)?;

            // 3. Parse Value
            let value = self.parse_value()?;
            map.insert(key, value);

            // 4. Expect Comma or Brace
            let token = self.consume()?.clone();
            match token.kind {
                TokenType::Comma => continue,
                TokenType::RightBrace => break,
                _ => {
                    // --- FIX 3 ---
                    // The error message was wrong. We expected a comma or brace.
                    return Err(self.error("Expected ',' or '}'".to_string(), &token));
                }
            }
        }
        Ok(JsonValue::Object(map))
    }

    // --- Token consumption helpers ---
    fn peek(&self) -> Result<&Token, ParseError> {
        self.tokens
            .get(self.position)
            .ok_or_else(|| self.error_eoi("Unexpected end of input".to_string()))
    }

    fn consume(&mut self) -> Result<&Token, ParseError> {
        let token = self
            .tokens
            .get(self.position)
            .ok_or_else(|| self.error_eoi("Unexpected end of input".to_string()))?;
        self.position += 1;
        Ok(token)
    }

    fn expect(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        let token = self.consume()?.clone();
        if token.kind == expected {
            Ok(token)
        } else {
            Err(self.error(
                format!("Expected '{:?}' but found '{:?}'", expected, token.kind),
                &token,
            ))
        }
    }
}

// --- 6. Main Function ---
fn main() {
    let input = "{ \"key\": [1, true 3] }";
    println!("Parsing: {}", input);

    match public_parse(input) {
        Ok(json) => println!("Parsed: {:#?}", json),
        Err(e) => println!("{}", e), // This will now use our Display impl!
    }
}

/// Public-facing helper function to tie it all together.
pub fn public_parse(input: &str) -> Result<JsonValue, ParseError> {
    let mut tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;
    let mut parser = Parser::new(&tokens);
    parser.parse()
}

// --- 7. Test Module (Refactored) ---
#[cfg(test)]
mod tests {
    use super::*;

    // Helper macro
    macro_rules! hashmap {
        ($($key:expr => $value:expr),* $(,)?) => {
            {
                let mut map = HashMap::new();
                $(
                    map.insert($key.to_string(), $value);
                )*
                map
            }
        };
    }

    #[test]
    fn test_parse_valid() {
        let input = "{ \"key\": [1, null, true, \"hello\"] }";
        let result = public_parse(input).unwrap();
        assert_eq!(
            result,
            JsonValue::Object(hashmap! {
                "key" => JsonValue::Array(vec![
                    JsonValue::Number(1.0),
                    JsonValue::Null,
                    JsonValue::Boolean(true),
                    JsonValue::String("hello".to_string())
                ])
            })
        );
    }

    // --- NEW TESTS for Stage 12 ---
    #[test]
    fn test_error_unexpected_eoi() {
        let input = "[1, 2, 3"; // Missing ']'
        let err = public_parse(input).unwrap_err();

        // The parser tried to consume a token after '3' but found EOI
        assert_eq!(err.message, "Unexpected end of input");
        assert_eq!(err.line, 1);
        // --- FIX 4 ---
        // The last token '3' starts at col 8. The error is *after* it, at col 9.
        assert_eq!(err.column, 9);
    }

    #[test]
    fn test_error_unexpected_token() {
        let input = "{\"key\":: \"value\"}"; // Double colon
        let err = public_parse(input).unwrap_err();

        // The parser expected a value after the first ':', but found another ':'
        assert_eq!(err.message, "Expected a value, found 'Colon'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 8);
    }

    #[test]
    fn test_error_missing_comma() {
        let input = "[1, true 3]"; // Missing comma between 'true' and '3'
        let err = public_parse(input).unwrap_err();

        // The array parser expected a ',' or ']' after 'true', but found 'Number(3.0)'
        // This test will now pass because parse_array returns the correct message.
        assert_eq!(err.message, "Expected ',' or ']'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 10); // Location of the '3'
    }

    #[test]
    fn test_error_tokenizer_invalid_char() {
        let input = "[1, ?]";
        let err = public_parse(input).unwrap_err();

        // The tokenizer found an invalid character
        assert_eq!(err.message, "Unexpected character '?'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 5);
    }

    #[test]
    fn test_error_trailing_tokens() {
        let input = "[1, 2] {"; // Valid JSON followed by junk
        let err = public_parse(input).unwrap_err();

        // The parser finished, but there were tokens left
        assert_eq!(err.message, "Unexpected token 'LeftBrace'");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 8);
    }
}
