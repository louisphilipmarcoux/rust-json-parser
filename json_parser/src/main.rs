use std::collections::HashMap;

// --- 1. The final JSON Value Enum ---
#[derive(Debug, PartialEq, Clone)] // Clone is useful for testing
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

// --- 2. The new Token Enum ---
/// Represents a single "word" or symbol in the JSON language.
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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

// --- 3. The new Error Type ---
/// A simple error type for both tokenizing and parsing.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedCharacter(char),
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
    InvalidString(String),
    InvalidNumber(String),
}

// --- 4. The Tokenizer (Lexer) ---
/// Scans the raw text and produces a `Vec<Token>`.
pub fn tokenize(input: &str) -> Result<Vec<Token>, ParseError> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            // --- 1. Whitespace ---
            ' ' | '\n' | '\t' | '\r' => {
                // Skip whitespace
                continue;
            }

            // --- 2. Single-char tokens ---
            '{' => tokens.push(Token::LeftBrace),
            '}' => tokens.push(Token::RightBrace),
            '[' => tokens.push(Token::LeftBracket),
            ']' => tokens.push(Token::RightBracket),
            ':' => tokens.push(Token::Colon),
            ',' => tokens.push(Token::Comma),

            // --- 3. Literals (null, true, false) ---
            'n' => {
                // Expect "ull"
                if chars.next() == Some('u')
                    && chars.next() == Some('l')
                    && chars.next() == Some('l')
                {
                    tokens.push(Token::Null);
                } else {
                    return Err(ParseError::InvalidString("Expected 'null'".to_string()));
                }
            }
            't' => {
                // Expect "rue"
                if chars.next() == Some('r')
                    && chars.next() == Some('u')
                    && chars.next() == Some('e')
                {
                    tokens.push(Token::Boolean(true));
                } else {
                    return Err(ParseError::InvalidString("Expected 'true'".to_string()));
                }
            }
            'f' => {
                // Expect "alse"
                if chars.next() == Some('a')
                    && chars.next() == Some('l')
                    && chars.next() == Some('s')
                    && chars.next() == Some('e')
                {
                    tokens.push(Token::Boolean(false));
                } else {
                    return Err(ParseError::InvalidString("Expected 'false'".to_string()));
                }
            }

            // --- 4. Strings ---
            '"' => {
                tokens.push(lex_string(&mut chars)?);
            }

            // --- 5. Numbers ---
            '-' | '0'..='9' => {
                tokens.push(lex_number(c, &mut chars)?);
            }

            // --- 6. Unexpected Character ---
            _ => {
                return Err(ParseError::UnexpectedCharacter(c));
            }
        }
    }

    Ok(tokens)
}

/// Helper for tokenizing a string (consumes the closing quote)
fn lex_string(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Result<Token, ParseError> {
    let mut parsed_content = String::new();

    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                // Handle escapes
                if let Some(escaped_char) = chars.next() {
                    match escaped_char {
                        '"' | '\\' | '/' => parsed_content.push(escaped_char),
                        'b' => parsed_content.push('\u{0008}'),
                        'f' => parsed_content.push('\u{000C}'),
                        'n' => parsed_content.push('\n'),
                        'r' => parsed_content.push('\r'),
                        't' => parsed_content.push('\t'),
                        'u' => {
                            let parse_hex_4 = |chars: &mut std::iter::Peekable<std::str::Chars<'_>>| -> Result<u32, ParseError> {
                                let mut hex_code = String::with_capacity(4);
                                for _ in 0..4 {
                                    if let Some(hex_char) = chars.next() {
                                        if hex_char.is_ascii_hexdigit() {
                                            hex_code.push(hex_char);
                                        } else {
                                            return Err(ParseError::InvalidString("Non-hex char in Unicode".to_string()));
                                        }
                                    } else {
                                        return Err(ParseError::InvalidString("Incomplete Unicode".to_string()));
                                    }
                                }
                                u32::from_str_radix(&hex_code, 16).map_err(|_| ParseError::InvalidString("Unicode parse error".to_string()))
                            };

                            let code1 = parse_hex_4(chars)?;
                            let c = std::char::from_u32(code1).ok_or(ParseError::InvalidString(
                                "Invalid Unicode code point".to_string(),
                            ))?;

                            // (We're simplifying by not handling surrogate pairs for now,
                            // as our old parser did. We can add this back if needed.)
                            parsed_content.push(c);
                        }
                        _ => return Err(ParseError::InvalidString("Invalid escape".to_string())),
                    }
                } else {
                    return Err(ParseError::UnexpectedEndOfInput);
                }
            }
            '"' => {
                // End of string
                return Ok(Token::String(parsed_content));
            }
            _ => parsed_content.push(c),
        }
    }
    Err(ParseError::UnexpectedEndOfInput) // Unclosed string
}

/// Helper for tokenizing a number
fn lex_number(
    first_char: char,
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
) -> Result<Token, ParseError> {
    let mut num_str = String::new();
    num_str.push(first_char);

    // Greedily consume all valid number characters
    while let Some(c) = chars.peek() {
        match c {
            '0'..='9' | '.' | 'e' | 'E' | '+' | '-' => {
                num_str.push(chars.next().unwrap()); // Consume the char
            }
            _ => {
                // Not a number char, so the number token is done
                break;
            }
        }
    }

    // --- Re-introduce Stage 10 Validation ---

    // Fail cases like "1.e1" or "1.E1"
    if let Some(e_pos) = num_str.find(['e', 'E']) {
        if e_pos > 0 {
            if let Some(char_before_e) = num_str.chars().nth(e_pos - 1) {
                if char_before_e == '.' {
                    // This is an invalid format
                    return Err(ParseError::InvalidNumber(num_str));
                }
            }
        }
    }

    // Fail cases like "1e", "1e+", "1e-"
    if let Some(last_char) = num_str.chars().last() {
        if last_char == 'e' || last_char == 'E' || last_char == '+' || last_char == '-' {
            return Err(ParseError::InvalidNumber(num_str));
        }
    }
    // --- End Validation ---

    match num_str.parse::<f64>() {
        Ok(num) => Ok(Token::Number(num)),
        Err(_) => Err(ParseError::InvalidNumber(num_str)),
    }
}

// --- 5. The Parser ---
/// Consumes a stream of `Token`s and produces a `JsonValue`.
pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> Parser<'a> {
    /// Creates a new parser for a slice of tokens.
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    /// The main public-facing parse method.
    pub fn parse(&mut self) -> Result<JsonValue, ParseError> {
        let value = self.parse_value()?;

        // After parsing, we should be at the end of the tokens.
        // If not, it means there was extra junk (e.g., `[1] [2]`).
        if self.position == self.tokens.len() {
            Ok(value)
        } else {
            Err(ParseError::UnexpectedToken(self.peek().clone()))
        }
    }

    // --- Private Helper Methods ---

    /// Parses any single JSON value.
    fn parse_value(&mut self) -> Result<JsonValue, ParseError> {
        let token = self.consume()?;
        match token {
            Token::Null => Ok(JsonValue::Null),
            Token::Boolean(b) => Ok(JsonValue::Boolean(*b)),
            Token::Number(n) => Ok(JsonValue::Number(*n)),
            Token::String(s) => Ok(JsonValue::String(s.clone())),
            Token::LeftBracket => self.parse_array(),
            Token::LeftBrace => self.parse_object(),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    /// Parses an array: [ ... ]
    fn parse_array(&mut self) -> Result<JsonValue, ParseError> {
        let mut elements = Vec::new();

        // Check for empty array
        if self.peek() == &Token::RightBracket {
            self.consume()?; // Consume ']'
            return Ok(JsonValue::Array(elements));
        }

        // Parse elements
        loop {
            elements.push(self.parse_value()?);

            match self.consume()? {
                Token::Comma => continue,     // Expect another value
                Token::RightBracket => break, // End of array
                t => return Err(ParseError::UnexpectedToken(t.clone())),
            }
        }

        Ok(JsonValue::Array(elements))
    }

    /// Parses an object: { ... }
    fn parse_object(&mut self) -> Result<JsonValue, ParseError> {
        let mut map = HashMap::new();

        // Check for empty object
        if self.peek() == &Token::RightBrace {
            self.consume()?; // Consume '}'
            return Ok(JsonValue::Object(map));
        }

        // Parse key-value pairs
        loop {
            // 1. Parse Key (must be a string)
            let key = match self.parse_value()? {
                JsonValue::String(s) => s,
                _ => {
                    return Err(ParseError::InvalidString(
                        "Object key must be a string".to_string(),
                    ));
                }
            };

            // 2. Expect Colon
            self.expect(Token::Colon)?;

            // 3. Parse Value
            let value = self.parse_value()?;
            map.insert(key, value);

            // 4. Expect Comma or Brace
            match self.consume()? {
                Token::Comma => continue,   // Expect another pair
                Token::RightBrace => break, // End of object
                t => return Err(ParseError::UnexpectedToken(t.clone())),
            }
        }

        Ok(JsonValue::Object(map))
    }

    // --- Token consumption helpers ---

    /// Returns the next token without consuming it.
    fn peek(&self) -> &Token {
        &self.tokens[self.position]
    }

    /// Consumes and returns the next token.
    fn consume(&mut self) -> Result<&Token, ParseError> {
        if self.position >= self.tokens.len() {
            return Err(ParseError::UnexpectedEndOfInput);
        }
        let token = &self.tokens[self.position];
        self.position += 1;
        Ok(token)
    }

    /// Expects a specific token next, consuming it if found.
    fn expect(&mut self, expected: Token) -> Result<&Token, ParseError> {
        let token = self.consume()?;
        if token == &expected {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedToken(token.clone()))
        }
    }
}

// --- 6. Main Function (Unchanged) ---
fn main() {
    println!("JSON Parser. Run 'cargo test' to execute tests.");
}

// --- 7. Test Module (Refactored) ---
#[cfg(test)]
mod tests {
    use super::*;

    // Helper macro for object tests
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

    /// Helper to run a full parse
    fn parse_test_helper(input: &str) -> Result<JsonValue, ParseError> {
        let tokens = tokenize(input)?;
        Parser::new(&tokens).parse()
    }

    #[test]
    fn test_tokenizer() {
        let input = "{\n \"key\": [1, null, true]\n}";
        let tokens = tokenize(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LeftBrace,
                Token::String("key".to_string()),
                Token::Colon,
                Token::LeftBracket,
                Token::Number(1.0),
                Token::Comma,
                Token::Null,
                Token::Comma,
                Token::Boolean(true),
                Token::RightBracket,
                Token::RightBrace
            ]
        );
    }

    #[test]
    fn test_parse_null_bool_num() {
        assert_eq!(parse_test_helper("null").unwrap(), JsonValue::Null);
        assert_eq!(parse_test_helper("true").unwrap(), JsonValue::Boolean(true));
        assert_eq!(
            parse_test_helper("123.45").unwrap(),
            JsonValue::Number(123.45)
        );
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse_test_helper("\"hello\\nworld\"").unwrap(),
            JsonValue::String("hello\nworld".to_string())
        );
    }

    #[test]
    fn test_parse_array() {
        let result = parse_test_helper("[ 1, \"hello\", [ false ] ]").unwrap();
        assert_eq!(
            result,
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::String("hello".to_string()),
                JsonValue::Array(vec![JsonValue::Boolean(false)])
            ])
        );
    }

    #[test]
    fn test_parse_object() {
        let result = parse_test_helper(
            "{ \"name\": \"Babbage\", \"nested\": { \"admin\": true, \"age\": 30 } }",
        )
        .unwrap();
        assert_eq!(
            result,
            JsonValue::Object(hashmap! {
                "name" => JsonValue::String("Babbage".to_string()),
                "nested" => JsonValue::Object(hashmap! {
                    "admin" => JsonValue::Boolean(true),
                    "age" => JsonValue::Number(30.0)
                })
            })
        );
    }

    #[test]
    fn test_all_previous_stages_pass() {
        // This confirms all our old tests (which are now covered by the
        // tests above) still behave as expected.
        assert!(parse_test_helper("nul").is_err()); // Stage 1
        assert!(parse_test_helper("True").is_err()); // Stage 2
        assert!(parse_test_helper("1.e1").is_err()); // Stage 10 (fails at lex_number)
        assert!(parse_test_helper("\"hello").is_err()); // Stage 4 (fails at lex_string)
        assert!(parse_test_helper("[1, 2").is_err()); // Stage 5 (fails at parser)
        assert!(parse_test_helper("{key: 1}").is_err()); // Stage 6 (fails at tokenizer)
        assert!(parse_test_helper("[1,,2]").is_err()); // Stage 5 (fails at parser)
    }
}
