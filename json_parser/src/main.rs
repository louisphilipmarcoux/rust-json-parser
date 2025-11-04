use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

// --- Constants for Stage 14 ---
/// Default maximum nesting depth (e.g., 100)
const DEFAULT_MAX_DEPTH: usize = 100;
/// Maximum file size to parse (e.g., 10MB)
const MAX_JSON_SIZE_BYTES: usize = 10 * 1024 * 1024;

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

// --- 3. The new Error Type ---
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

// --- 4. The Tokenizer (Lexer) ---
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

    fn tokenize(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut tokens = Vec::new();
        loop {
            let (start_line, start_column) = (self.line, self.column);
            let next_char = match self.peek() {
                Some(c) => *c,
                None => break,
            };

            let token_kind = match next_char {
                // ... (whitespace, single-char tokens) ...
                ' ' | '\t' | '\r' | '\n' => {
                    self.next_char();
                    continue;
                }
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
                'n' => self.lex_literal("null", TokenType::Null)?,
                't' => self.lex_literal("true", TokenType::Boolean(true))?,
                'f' => self.lex_literal("false", TokenType::Boolean(false))?,
                '"' => self.lex_string()?,
                '-' | '0'..='9' => self.lex_number()?,
                '/' => {
                    return Err(self.error("Comments are not allowed in JSON".to_string()));
                }
                _ => {
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
                    // ... (escape logic) ...
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

        // ... (Stage 13 validation checks) ...
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

// --- 5. The Parser (UPDATED for Stage 14) ---
pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
    // --- NEW Fields ---
    current_depth: usize,
    max_depth: usize,
}

impl<'a> Parser<'a> {
    /// Updated `new` function
    pub fn new(tokens: &'a [Token], max_depth: usize) -> Self {
        Parser {
            tokens,
            position: 0,
            current_depth: 0,
            max_depth,
        }
    }

    pub fn parse(&mut self) -> Result<JsonValue, ParseError> {
        let value = self.parse_value()?;
        if self.position == self.tokens.len() {
            Ok(value)
        } else {
            let token = self.peek()?;
            Err(self.error(format!("Unexpected token '{:?}'", token.kind), token))
        }
    }

    fn error(&self, message: String, token: &Token) -> ParseError {
        ParseError {
            message,
            line: token.line,
            column: token.column,
        }
    }

    fn error_eoi(&self, message: String) -> ParseError {
        let (line, column) = if let Some(last_token) = self.tokens.last() {
            (last_token.line, last_token.column + 1)
        } else {
            (1, 1)
        };
        ParseError {
            message,
            line,
            column,
        }
    }

    // --- UPDATED for Stage 14: Check depth ---
    fn parse_value(&mut self) -> Result<JsonValue, ParseError> {
        let token = self.consume()?.clone();
        match token.kind {
            TokenType::Null => Ok(JsonValue::Null),
            TokenType::Boolean(b) => Ok(JsonValue::Boolean(b)),
            TokenType::Number(n) => Ok(JsonValue::Number(n)),
            TokenType::String(s) => Ok(JsonValue::String(s)),

            TokenType::LeftBracket => {
                // Check depth BEFORE parsing the array
                if self.current_depth >= self.max_depth {
                    return Err(self.error("Maximum nesting depth exceeded".to_string(), &token));
                }
                self.current_depth += 1;
                let result = self.parse_array();
                self.current_depth -= 1; // Decrement after, whether it succeeded or failed
                result
            }
            TokenType::LeftBrace => {
                // Check depth BEFORE parsing the object
                if self.current_depth >= self.max_depth {
                    return Err(self.error("Maximum nesting depth exceeded".to_string(), &token));
                }
                self.current_depth += 1;
                let result = self.parse_object();
                self.current_depth -= 1; // Decrement after, whether it succeeded or failed
                result
            }
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
            self.consume()?;
            return Ok(JsonValue::Array(elements));
        }

        loop {
            elements.push(self.parse_value()?); // This will recursively call parse_value
            let token = self.peek()?.clone();
            match token.kind {
                TokenType::Comma => {
                    self.consume()?;
                    if self.peek()?.kind == TokenType::RightBracket {
                        return Err(
                            self.error("Trailing comma not allowed in array".to_string(), &token)
                        );
                    }
                }
                TokenType::RightBracket => {
                    self.consume()?;
                    break;
                }
                _ => {
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
            self.consume()?;
            return Ok(JsonValue::Object(map));
        }

        loop {
            // 1. Parse Key
            let key_token = self.consume()?.clone();
            let key = match &key_token.kind {
                TokenType::String(s) => s.clone(),
                _ => return Err(self.error("Object key must be a string".to_string(), &key_token)),
            };

            // 2. Expect Colon
            self.expect(TokenType::Colon)?;

            // 3. Parse Value
            let value = self.parse_value()?; // This will recursively call parse_value
            map.insert(key, value);

            // 4. Expect Comma or Brace
            let token = self.peek()?.clone();
            match token.kind {
                TokenType::Comma => {
                    self.consume()?;
                    if self.peek()?.kind == TokenType::RightBrace {
                        return Err(
                            self.error("Trailing comma not allowed in object".to_string(), &token)
                        );
                    }
                }
                TokenType::RightBrace => {
                    self.consume()?;
                    break;
                }
                _ => {
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
    let input = "[".repeat(101) + &"]".repeat(101); // 101 levels deep
    println!("Parsing: {}... (truncated)", &input[..20]);

    match public_parse(&input) {
        Ok(json) => println!("Parsed: {:#?}", json),
        Err(e) => println!("{}", e),
    }
}

/// Public-facing helper function (UPDATED for Stage 14)
pub fn public_parse(input: &str) -> Result<JsonValue, ParseError> {
    // 1. Check max size
    if input.len() > MAX_JSON_SIZE_BYTES {
        return Err(ParseError {
            message: "Input exceeds maximum size limit".to_string(),
            line: 1,
            column: 1,
        });
    }

    // 2. Tokenize
    let mut tokenizer = Tokenizer::new(input);
    let tokens = tokenizer.tokenize()?;

    // 3. Parse (pass in the max depth)
    let mut parser = Parser::new(&tokens, DEFAULT_MAX_DEPTH);
    parser.parse()
}

// --- 7. Test Module ---
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

    // --- NEW TESTS for Stage 13 ---
    #[test]
    fn test_rfc_8259_compliance() {
        // 1. Reject Trailing Commas
        let err = public_parse("[1, 2,]").unwrap_err();
        assert_eq!(err.message, "Trailing comma not allowed in array");

        let err = public_parse("{\"key\": 1,}").unwrap_err();
        assert_eq!(err.message, "Trailing comma not allowed in object");

        // 2. Reject Comments
        let err = public_parse("// a comment\n[1, 2]").unwrap_err();
        assert_eq!(err.message, "Comments are not allowed in JSON");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 1);

        let err = public_parse("[1, 2] /* a comment */").unwrap_err();
        assert_eq!(err.message, "Comments are not allowed in JSON");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 8);

        // 3. Reject Leading Zeros
        let err = public_parse("0123").unwrap_err();
        assert_eq!(err.message, "Invalid number: leading zeros not allowed");

        let err = public_parse("[01]").unwrap_err();
        assert_eq!(err.message, "Invalid number: leading zeros not allowed");

        // (Make sure valid '0's still work)
        assert_eq!(public_parse("0").unwrap(), JsonValue::Number(0.0));
        assert_eq!(public_parse("0.123").unwrap(), JsonValue::Number(0.123));

        // 4. Reject numbers starting/ending with '.'
        let err = public_parse(".5").unwrap_err();
        assert_eq!(err.message, "Unexpected character '.'"); // Fails in tokenizer

        let err = public_parse("1.").unwrap_err();
        assert_eq!(
            err.message,
            "Invalid number: cannot end with a decimal point"
        );

        // 5. Reject unescaped control chars
        let err = public_parse("\"\n\"").unwrap_err();
        assert_eq!(err.message, "Unescaped control character in string");
        assert_eq!(err.line, 2);
        assert_eq!(err.column, 1);
    }

    // --- NEW TESTS for Stage 14 ---
    #[test]
    fn test_security_limits() {
        // 1. Test Nesting Depth

        // Generate a string with 101 levels of nesting (1 over our limit of 100)
        let evil_input = "[".repeat(101) + &"]".repeat(101);

        let err = public_parse(&evil_input).unwrap_err();
        assert_eq!(err.message, "Maximum nesting depth exceeded");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 101); // The 101st '['

        // Generate a string with 100 levels (exactly our limit)
        // This should pass
        let ok_input = "[".repeat(100) + &"]".repeat(100);
        let result = public_parse(&ok_input);
        assert!(result.is_ok());

        // Test with objects
        let evil_obj_input = "{ \"a\": ".repeat(101) + "null" + &"}".repeat(101);
        let err = public_parse(&evil_obj_input).unwrap_err();
        assert_eq!(err.message, "Maximum nesting depth exceeded");
    }

    // We don't add a test for MAX_JSON_SIZE_BYTES because
    // it's not practical to create a 10MB string in a unit test.
    // We trust the simple `input.len()` check.
}
