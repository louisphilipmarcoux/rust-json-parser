//! The internal, high-performance, byte-based tokenizer (lexer).
//!
//! This module is the performance-critical part of the parser. It consumes
//! the raw input `&str` (as `&[u8]`) and produces a stream of `Token`s.
//! It is *not* part of the public API.

use crate::error::ParseError;
use crate::token::{Token, TokenType};
use memchr::memchr;
use std::str;

// --- The Lookup Table (LUT) ---
// This is the core of our "safe" speedup.
// A 256-entry array. We can check properties of any
// byte with a single, branchless lookup.
pub(crate) const W: u8 = 1; // Whitespace
pub(crate) const S: u8 = 2; // Structural
pub(crate) const L: u8 = 3; // Literal
pub(crate) const D: u8 = 4; // Digit
pub(crate) const Q: u8 = 5; // Quote

static BYTE_PROPERTIES: [u8; 256] = {
    let mut table = [0; 256];
    // 1: Whitespace
    table[b' ' as usize] = W;
    table[b'\n' as usize] = W;
    table[b'\r' as usize] = W;
    table[b'\t' as usize] = W;

    // 2: Structural
    table[b'{' as usize] = S;
    table[b'}' as usize] = S;
    table[b'[' as usize] = S;
    table[b']' as usize] = S;
    table[b':' as usize] = S;
    table[b',' as usize] = S;

    // 3: Literal
    table[b't' as usize] = L;
    table[b'f' as usize] = L;
    table[b'n' as usize] = L;

    // 5: Quote
    table[b'"' as usize] = Q;

    // 4: Digit (and '-')
    table[b'-' as usize] = D;
    table[b'0' as usize] = D;
    table[b'1' as usize] = D;
    table[b'2' as usize] = D;
    table[b'3' as usize] = D;
    table[b'4' as usize] = D;
    table[b'5' as usize] = D;
    table[b'6' as usize] = D;
    table[b'7' as usize] = D;
    table[b'8' as usize] = D;
    table[b'9' as usize] = D;

    // 0: All other bytes are "invalid" in a top-level context
    table
};

const IS_WHITESPACE: u8 = 1;

// --- 4. Tokenizer ---

/// The internal tokenizer (lexer).
///
/// It operates on raw bytes (`&[u8]`) for performance, using a lookup
/// table (`BYTE_PROPERTIES`) to classify bytes and `memchr` for fast
/// string scanning.
pub(crate) struct Tokenizer<'a> {
    /// The raw byte slice of the input JSON.
    bytes: &'a [u8],
    /// The current position (index) in the `bytes` slice.
    cursor: usize,
    /// The current line number (1-indexed) for error reporting.
    line: usize,
    /// The current column number (1-indexed) for error reporting.
    column: usize,
}

impl<'a> Tokenizer<'a> {
    /// Creates a new `Tokenizer` from an input string.
    pub(crate) fn new(input: &'a str) -> Self {
        Tokenizer {
            bytes: input.as_bytes(),
            cursor: 0,
            line: 1,
            column: 1,
        }
    }

    /// Creates a `ParseError` with the current line and column.
    fn error(&self, message: String) -> ParseError {
        ParseError {
            message,
            line: self.line,
            column: self.column,
        }
    }

    /// The performance-critical whitespace skipping function.
    /// Uses the LUT to check byte properties in a branchless way.
    #[inline]
    fn skip_whitespace(&mut self) {
        while let Some(&byte) = self.bytes.get(self.cursor) {
            // This is the hot loop: a single array lookup and comparison.
            if BYTE_PROPERTIES[byte as usize] != IS_WHITESPACE {
                break; // Not whitespace, stop.
            }

            // It *is* whitespace, advance cursor and update position.
            if byte == b'\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.cursor += 1;
        }
    }

    /// Advances the cursor by one byte, updating line/col, and returns the byte.
    /// Assumes the cursor is not at the end.
    #[inline]
    fn advance_byte(&mut self) -> u8 {
        let byte = self.bytes[self.cursor];
        if byte == b'\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.cursor += 1;
        byte
    }

    /// Advances the cursor by `n` bytes, updating line/col for each.
    #[inline]
    fn advance_by(&mut self, n: usize) {
        // This is safe to call on ASCII (literals, numbers),
        // but we use the slower `advance_byte` for strings
        // to correctly track newlines.
        for _ in 0..n {
            self.advance_byte();
        }
    }

    /// Helper to safely get a slice from the input bytes.
    #[inline]
    fn get_slice(&self, range: std::ops::Range<usize>) -> Option<&'a [u8]> {
        self.bytes.get(range)
    }

    /// Parses a JSON literal (`true`, `false`, `null`).
    fn lex_literal(
        &mut self,
        expected: &'static [u8],
        kind: TokenType,
    ) -> Result<TokenType, ParseError> {
        let end = self.cursor + expected.len();
        if self.get_slice(self.cursor..end) == Some(expected) {
            self.advance_by(expected.len());
            Ok(kind)
        } else {
            // This unwrap is safe because "true", "false", and "null" are valid UTF-8
            Err(self.error(format!("Expected '{}'", str::from_utf8(expected).unwrap())))
        }
    }

    /// Parses a JSON string, handling escapes.
    /// Uses `memchr` for "safe SIMD" acceleration.
    fn lex_string(&mut self) -> Result<TokenType, ParseError> {
        self.advance_byte(); // Consume opening '"'

        let string_start_cursor = self.cursor;
        let mut s: String; // Will hold our final string if it has escapes.

        // --- "Hot" path ---
        // 1. Scan for the *closing quote*. This is the common case.
        let mut current_slice = &self.bytes[self.cursor..];
        let mut total_offset = 0; // Offset from self.cursor

        let quote_index = loop {
            match memchr(b'"', current_slice) {
                Some(i) => {
                    // We found a quote. Check if it's escaped.
                    // Count the number of preceding backslashes.
                    let mut backslashes = 0;
                    let mut pos = i;
                    while pos > 0 {
                        if current_slice.get(pos - 1) == Some(&b'\\') {
                            backslashes += 1;
                            pos -= 1;
                        } else {
                            break;
                        }
                    }

                    if backslashes % 2 == 0 {
                        // Even number of backslashes (or zero).
                        // This is the real closing quote.
                        break total_offset + i;
                    } else {
                        // Odd number of backslashes.
                        // This quote is escaped. Continue searching *after* it.
                        total_offset += i + 1;
                        current_slice = &current_slice[i + 1..];
                    }
                }
                None => return Err(self.error("Unterminated string".to_string())),
            }
        };

        // 2. Get the slice of *just* the string's content.
        let content_slice = &self.bytes[self.cursor..self.cursor + quote_index];

        // 3. Scan *that slice* for an escape (`\`).
        if memchr(b'\\', content_slice).is_some() {
            // --- "Cold" path (contains escapes) ---
            // We must build the string byte-by-byte.
            s = String::with_capacity(content_slice.len());

            // Re-scan from the start, but this time building the string.
            self.cursor = string_start_cursor;

            while self.cursor < string_start_cursor + quote_index {
                let byte = self.advance_byte();
                // Check for unescaped control characters.
                if byte < 0x20 {
                    return Err(self.error("Unescaped control character in string".to_string()));
                }

                if byte == b'\\' {
                    // Handle escape sequence
                    let escaped_char = match self.advance_byte() {
                        b'"' | b'\\' | b'/' => self.bytes[self.cursor - 1],
                        b'b' => b'\x08', // Backspace
                        b'f' => b'\x0C', // Form feed
                        b'n' => b'\n',   // Newline
                        b'r' => b'\r',   // Carriage return
                        b't' => b'\t',   // Tab
                        b'u' => {
                            // Handle \uXXXX Unicode escape
                            let s_slice =
                                self.get_slice(self.cursor..self.cursor + 4)
                                    .ok_or_else(|| {
                                        self.error("Incomplete Unicode escape".to_string())
                                    })?;

                            let hex_str = str::from_utf8(s_slice).map_err(|_| {
                                self.error("Non-UTF8 in Unicode escape".to_string())
                            })?;

                            let code = u32::from_str_radix(hex_str, 16).map_err(|_| {
                                self.error("Non-hex char in Unicode escape".to_string())
                            })?;

                            self.advance_by(4); // Advance past the 4 hex digits

                            let c = std::char::from_u32(code).ok_or_else(|| {
                                self.error("Invalid Unicode code point".to_string())
                            })?;
                            s.push(c);
                            continue; // Skip the char push at the end
                        }
                        _ => return Err(self.error("Invalid escape sequence".to_string())),
                    };
                    s.push(escaped_char as char);
                } else {
                    // Not an escape, just a regular char
                    s.push(byte as char);
                }
            }
            // After the loop, we're at the closing quote.
            self.advance_byte(); // Consume the quote
            Ok(TokenType::String(s))
        } else {
            // --- "Hot" path (no escapes) ---
            // This is the fastest path.
            // We *must* still check for unescaped control chars
            // and update our line/column counters.
            for _ in 0..content_slice.len() {
                let byte = self.advance_byte(); // Consume *first*
                if byte < 0x20 {
                    // This reports the error at the correct line/col.
                    return Err(self.error("Unescaped control character in string".to_string()));
                }
            }

            // Now we know the slice is valid.
            let s_str = str::from_utf8(content_slice)
                .map_err(|_| self.error("Invalid UTF-8 in string".to_string()))?;

            // We're at the closing quote. Consume it.
            self.advance_byte();
            Ok(TokenType::String(s_str.to_string()))
        }
    }

    /// Parses a JSON number.
    fn lex_number(&mut self) -> Result<TokenType, ParseError> {
        let start = self.cursor;

        // Greedily consume all valid number characters.
        while let Some(&byte) = self.bytes.get(self.cursor) {
            match byte {
                // Use the LUT for the most common case
                b if BYTE_PROPERTIES[b as usize] == D => {
                    self.advance_byte();
                }
                // Handle other valid number parts
                b'.' | b'e' | b'E' | b'+' => {
                    self.advance_byte();
                }
                _ => break, // Not a number char, stop
            }
        }

        let end = self.cursor;
        if start == end {
            // This should be unreachable if called from `next`
            return Err(self.error("Expected a number".to_string()));
        }

        // This is safe because we only advanced over ASCII bytes
        let num_str = unsafe { str::from_utf8_unchecked(&self.bytes[start..end]) };

        // --- RFC 8259 Validation ---
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
            if (last_char == 'e' || last_char == 'E' || last_char == '+' || last_char == '-')
                && num_str.len() > start - self.cursor
            {
                return Err(self.error(format!("Invalid number '{}'", num_str)));
            }
        }
        // --- End validation ---

        match num_str.parse::<f64>() {
            Ok(num) => Ok(TokenType::Number(num)),
            Err(_) => Err(self.error(format!("Invalid number '{}'", num_str))),
        }
    }
}

// --- NEW Iterator implementation ---
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, ParseError>;

    /// Gets the next `Token` from the input stream.
    fn next(&mut self) -> Option<Self::Item> {
        // 1. Skip all insignificant whitespace. This is the first hot path.
        self.skip_whitespace();

        // 2. Peek at the next byte to see what to do.
        let byte = match self.bytes.get(self.cursor) {
            Some(&b) => b,
            None => return None, // End of file
        };

        // 3. Record position *after* skipping whitespace.
        let (start_line, start_column) = (self.line, self.column);

        // 4. Use our blazing-fast LUT to decide which lexer to call.
        let token_kind_result = match BYTE_PROPERTIES[byte as usize] {
            S => {
                // Structural character (e.g., `{`, `[`, `,`)
                self.advance_byte();
                Ok(match byte {
                    b'{' => TokenType::LeftBrace,
                    b'}' => TokenType::RightBrace,
                    b'[' => TokenType::LeftBracket,
                    b']' => TokenType::RightBracket,
                    b':' => TokenType::Colon,
                    b',' => TokenType::Comma,
                    _ => unreachable!(), // LUT guarantees this
                })
            }
            L => {
                // Literal (true, false, null)
                match byte {
                    b't' => self.lex_literal(b"true", TokenType::Boolean(true)),
                    b'f' => self.lex_literal(b"false", TokenType::Boolean(false)),
                    b'n' => self.lex_literal(b"null", TokenType::Null),
                    _ => unreachable!(), // LUT guarantees this
                }
            }
            D => self.lex_number(), // Digit or '-' (start of a number)
            Q => self.lex_string(), // Quote (start of a string)
            _ => {
                // All other bytes (0 or 1) are invalid in this context.
                // Whitespace (1) should have been skipped.
                // 0 means an invalid character.
                Err(self.error(format!("Unexpected character '{}'", byte as char)))
            }
        };

        // 5. Wrap the result in a `Token` struct.
        let token_result = token_kind_result.map(|kind| Token {
            kind,
            line: start_line,
            column: start_column,
        });

        Some(token_result)
    }
}

// --- Unit Tests for Tokenizer ---
#[cfg(test)]
mod tests {
    use super::*;

    // Helper to collect tokens into just their types for easy comparison
    fn collect_token_types(input: &str) -> Result<Vec<TokenType>, ParseError> {
        let tokenizer = Tokenizer::new(input);
        tokenizer.map(|res| res.map(|token| token.kind)).collect()
    }

    #[test]
    fn test_tokenizer_structurals() {
        let input = "{}[]:,";
        let expected = vec![
            TokenType::LeftBrace,
            TokenType::RightBrace,
            TokenType::LeftBracket,
            TokenType::RightBracket,
            TokenType::Colon,
            TokenType::Comma,
        ];
        assert_eq!(collect_token_types(input).unwrap(), expected);
    }

    #[test]
    fn test_tokenizer_literals() {
        let input = "true false null";
        let expected = vec![
            TokenType::Boolean(true),
            TokenType::Boolean(false),
            TokenType::Null,
        ];
        assert_eq!(collect_token_types(input).unwrap(), expected);
    }

    #[test]
    fn test_tokenizer_numbers() {
        let input = "123 -0.5 1e10";
        let expected = vec![
            TokenType::Number(123.0),
            TokenType::Number(-0.5),
            TokenType::Number(10000000000.0),
        ];
        assert_eq!(collect_token_types(input).unwrap(), expected);
    }

    #[test]
    fn test_tokenizer_strings() {
        let input = r#" "hello" "a\nb" "\u1234" "\"" "#;
        let expected = vec![
            TokenType::String("hello".to_string()),
            TokenType::String("a\nb".to_string()),
            TokenType::String("\u{1234}".to_string()),
            TokenType::String("\"".to_string()),
        ];
        assert_eq!(collect_token_types(input).unwrap(), expected);
    }

    #[test]
    fn test_tokenizer_all_escapes() {
        let input = r#""\" \\ \/ \b \f \n \r \t""#;
        let expected = vec![TokenType::String(
            "\" \\ / \u{0008} \u{000C} \n \r \t".to_string(),
        )];
        assert_eq!(collect_token_types(input).unwrap(), expected);
    }

    #[test]
    fn test_tokenizer_whitespace_skipping() {
        let input = "  { \n \t \"key\" \r\n : \n 123 \n } \n ";
        let expected = vec![
            TokenType::LeftBrace,
            TokenType::String("key".to_string()),
            TokenType::Colon,
            TokenType::Number(123.0),
            TokenType::RightBrace,
        ];
        assert_eq!(collect_token_types(input).unwrap(), expected);
    }

    #[test]
    fn test_tokenizer_string_errors() {
        // Unterminated string
        let input = r#" "hello "#;
        let err = collect_token_types(input).unwrap_err();
        assert_eq!(err.message, "Unterminated string");

        // Unescaped control char
        let input = "\"\n\"";
        let err = collect_token_types(input).unwrap_err();
        assert_eq!(err.message, "Unescaped control character in string");

        // Invalid escape
        let input = r#" "\z" "#;
        let err = collect_token_types(input).unwrap_err();
        assert_eq!(err.message, "Invalid escape sequence");
    }

    #[test]
    fn test_tokenizer_number_errors() {
        // Leading zero
        let input = "0123";
        let err = collect_token_types(input).unwrap_err();
        assert_eq!(err.message, "Invalid number: leading zeros not allowed");

        // Trailing decimal
        let input = "123.";
        let err = collect_token_types(input).unwrap_err();
        assert_eq!(
            err.message,
            "Invalid number: cannot end with a decimal point"
        );
    }

    #[test]
    fn test_tokenizer_invalid_char() {
        let input = "?";
        let err = collect_token_types(input).unwrap_err();
        assert_eq!(err.message, "Unexpected character '?'");

        let input = "[1, 2, &]";
        let err = collect_token_types(input).unwrap_err();
        assert_eq!(err.message, "Unexpected character '&'");
    }
}
