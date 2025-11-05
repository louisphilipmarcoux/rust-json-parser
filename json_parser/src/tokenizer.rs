// src/tokenizer.rs
use crate::error::ParseError;
use crate::token::{Token, TokenType};
use memchr::memchr; // We only need memchr and memchr2
use std::str;

// --- The Lookup Table (LUT) ---
pub(crate) const W: u8 = 1; // Whitespace
pub(crate) const S: u8 = 2; // Structural
pub(crate) const L: u8 = 3; // Literal
pub(crate) const D: u8 = 4; // Digit
pub(crate) const Q: u8 = 5; // Quote

static BYTE_PROPERTIES: [u8; 256] = {
    let mut table = [0; 256];
    table[b' ' as usize] = W;
    table[b'\n' as usize] = W;
    table[b'\r' as usize] = W;
    table[b'\t' as usize] = W;

    table[b'{' as usize] = S;
    table[b'}' as usize] = S;
    table[b'[' as usize] = S;
    table[b']' as usize] = S;
    table[b':' as usize] = S;
    table[b',' as usize] = S;

    table[b't' as usize] = L;
    table[b'f' as usize] = L;
    table[b'n' as usize] = L;

    table[b'"' as usize] = Q;

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

    table
};

const IS_WHITESPACE: u8 = 1;

// --- 4. Tokenizer ---
pub(crate) struct Tokenizer<'a> {
    bytes: &'a [u8],
    cursor: usize,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Tokenizer {
            bytes: input.as_bytes(),
            cursor: 0,
            line: 1,
            column: 1,
        }
    }

    fn error(&self, message: String) -> ParseError {
        ParseError {
            message,
            line: self.line,
            column: self.column,
        }
    }

    #[inline]
    fn skip_whitespace(&mut self) {
        while let Some(&byte) = self.bytes.get(self.cursor) {
            if BYTE_PROPERTIES[byte as usize] != IS_WHITESPACE {
                break;
            }
            if byte == b'\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.cursor += 1;
        }
    }

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

    #[inline]
    fn advance_by(&mut self, n: usize) {
        // This is now safe because we only call it on ASCII slices
        for _ in 0..n {
            self.advance_byte();
        }
    }

    #[inline]
    fn get_slice(&self, range: std::ops::Range<usize>) -> Option<&'a [u8]> {
        self.bytes.get(range)
    }

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
            Err(self.error(format!("Expected '{}'", str::from_utf8(expected).unwrap())))
        }
    }

    // --- NEW, CORRECTED lex_string ---
    fn lex_string(&mut self) -> Result<TokenType, ParseError> {
        self.advance_byte(); // Consume opening '"'

        let string_start_cursor = self.cursor;
        let mut s: String; // Will hold our final string

        // "Hot" path: Scan for the closing quote.
        let slice = &self.bytes[self.cursor..];
        let quote_index = match memchr(b'"', slice) {
            Some(i) => i,
            None => return Err(self.error("Unterminated string".to_string())),
        };

        // Get the slice for the potential string content
        let content_slice = &self.bytes[self.cursor..self.cursor + quote_index];

        // Now, scan *that slice* for a backslash.
        if let Some(_) = memchr(b'\\', content_slice) {
            // "Cold" path: Contains escapes. We must build the string.
            s = String::with_capacity(content_slice.len());

            // Re-scan from the start, but this time building the string
            // We use `string_start_cursor` to restart
            self.cursor = string_start_cursor;

            while self.cursor < string_start_cursor + quote_index {
                let byte = self.advance_byte();
                if byte < 0x20 {
                    return Err(self.error("Unescaped control character in string".to_string()));
                }
                if byte == b'\\' {
                    // Handle escape
                    let escaped_char = match self.advance_byte() {
                        b'"' | b'\\' | b'/' => self.bytes[self.cursor - 1],
                        b'b' => b'\x08',
                        b'f' => b'\x0C',
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'u' => {
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
                            continue;
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
            return Ok(TokenType::String(s));
        } else {
            // "Hot" path: No escapes!
            for _ in 0..content_slice.len() {
                let byte = self.advance_byte(); // Consume *first*
                if byte < 0x20 {
                    // self.line/col are now pointing *after* the
                    // control char, which matches the old parser's logic.
                    return Err(self.error("Unescaped control character in string".to_string()));
                }
            }
            let s_str = str::from_utf8(content_slice)
                .map_err(|_| self.error("Invalid UTF-8 in string".to_string()))?;

            // We must *still* advance our column/line counters
            self.advance_byte(); // Advance past content + closing quote
            return Ok(TokenType::String(s_str.to_string()));
        }
    }

    fn lex_number(&mut self) -> Result<TokenType, ParseError> {
        let start = self.cursor;

        while let Some(&byte) = self.bytes.get(self.cursor) {
            match byte {
                b'0'..=b'9' | b'-' => {
                    self.advance_byte();
                }
                b'.' | b'e' | b'E' | b'+' => {
                    self.advance_byte();
                }
                _ => break,
            }
        }

        let end = self.cursor;
        if start == end {
            return Err(self.error("Expected a number".to_string()));
        }

        let num_str = unsafe {
            // This is safe because we only advanced over ASCII bytes
            str::from_utf8_unchecked(&self.bytes[start..end])
        };

        // --- All your existing validation logic ---
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

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        let byte = match self.bytes.get(self.cursor) {
            Some(&b) => b,
            None => return None,
        };

        let (start_line, start_column) = (self.line, self.column);

        let token_kind_result = match BYTE_PROPERTIES[byte as usize] {
            S => {
                self.advance_byte();
                Ok(match byte {
                    b'{' => TokenType::LeftBrace,
                    b'}' => TokenType::RightBrace,
                    b'[' => TokenType::LeftBracket,
                    b']' => TokenType::RightBracket,
                    b':' => TokenType::Colon,
                    b',' => TokenType::Comma,
                    _ => unreachable!(),
                })
            }
            L => match byte {
                b't' => self.lex_literal(b"true", TokenType::Boolean(true)),
                b'f' => self.lex_literal(b"false", TokenType::Boolean(false)),
                b'n' => self.lex_literal(b"null", TokenType::Null),
                _ => unreachable!(),
            },
            D => self.lex_number(),
            Q => self.lex_string(),
            _ => Err(self.error(format!("Unexpected character '{}'", byte as char))),
        };

        let token_result = token_kind_result.map(|kind| Token {
            kind,
            line: start_line,
            column: start_column,
        });

        Some(token_result)
    }
}
