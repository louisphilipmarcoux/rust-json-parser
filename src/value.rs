//! Contains the `JsonValue` enum, a native Rust representation of any
//! valid JSON value.
//!
//! This module also includes the "stringify" (serialization) logic
//! for converting a `JsonValue` back into a JSON string.
use crate::{parse_streaming, ParseError, ParserEvent, StreamingParser};
use std::collections::BTreeMap;
use std::fmt;

/// A native Rust representation of any valid JSON number.
///
/// This enum is used to store numbers without precision loss,
/// supporting `i64`, `u64`, and `f64`.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum JsonNumber {
    /// Represents a signed 64-bit integer.
    I64(i64),
    /// Represents an unsigned 64-bit integer.
    U64(u64),
    /// Represents a 64-bit floating-point number.
    F64(f64),
}

/// Implement Display to allow `write!(w, "{}", ...)`
impl fmt::Display for JsonNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsonNumber::I64(n) => write!(f, "{}", n),
            JsonNumber::U64(n) => write!(f, "{}", n),
            JsonNumber::F64(n) => write!(f, "{}", n),
        }
    }
}

/// A native Rust representation of any valid JSON value.
///
/// This enum is used by the `stringify` functions to serialize
/// Rust data *into* a JSON string.
#[derive(Debug, PartialEq, Clone)]
pub enum JsonValue {
    /// Represents a JSON `null`.
    Null,
    /// Represents a JSON `true` or `false`.
    Boolean(bool),
    /// Represents a JSON number.
    Number(JsonNumber),
    /// Represents a JSON string.
    String(String),
    /// Represents a JSON array (list).
    Array(Vec<JsonValue>),
    /// Represents a JSON object (map). (Using BTreeMap for Fix 4)
    Object(BTreeMap<String, JsonValue>),
}

impl JsonValue {
    /// Parses a JSON string into a `JsonValue`.
    ///
    /// This function builds an in-memory `JsonValue` from the input string.
    /// For large inputs, using the `parse_streaming` iterator is more memory-efficient.
    ///
    /// # Errors
    /// Returns a `ParseError` if the JSON is invalid, empty, or has trailing tokens.
    pub fn parse(input: &str) -> Result<JsonValue, ParseError> {
        let mut parser = parse_streaming(input)?.peekable();

        // Check for empty input
        if parser.peek().is_none() {
            return Err(ParseError {
                message: "Empty input".to_string(),
                line: 1,
                column: 1,
            });
        }

        // Recursive helper
        fn parse_one(
            parser: &mut std::iter::Peekable<StreamingParser<'_>>,
        ) -> Result<JsonValue, ParseError> {
            // Consume next event
            let event = match parser.next() {
                Some(Ok(event)) => event,
                Some(Err(e)) => return Err(e),
                None => {
                    return Err(ParseError {
                        message: "Unexpected end of input".to_string(),
                        line: 1, // This is a best-effort location
                        column: 1,
                    });
                }
            };

            match event {
                // Base cases
                ParserEvent::String(s) => Ok(JsonValue::String(s.into_owned())),
                ParserEvent::Number(n) => Ok(JsonValue::Number(n)),
                ParserEvent::Boolean(b) => Ok(JsonValue::Boolean(b)),
                ParserEvent::Null => Ok(JsonValue::Null),

                // Recursive cases
                ParserEvent::StartArray => {
                    let mut arr = Vec::new();
                    // Loop until we see `EndArray`
                    loop {
                        match parser.peek() {
                            Some(Ok(ParserEvent::EndArray)) => {
                                parser.next(); // Consume the EndArray
                                break Ok(JsonValue::Array(arr));
                            }
                            Some(Ok(_)) => {
                                // It's a value, recurse
                                arr.push(parse_one(parser)?);
                            }
                            Some(Err(_)) => {
                                // Propagate the error
                                return Err(parser.next().unwrap().unwrap_err());
                            }
                            None => {
                                // This branch should be unreachable.
                                // The StreamingParser's `next()` will return
                                // `Some(Err("Unclosed array"))` when it hits EOF
                                // in this state, which `peek()` will cache.
                                // The `Some(Err(_))` branch will be taken instead.
                                unreachable!("Unclosed array branch hit None");
                            }
                        }
                    }
                }

                ParserEvent::StartObject => {
                    let mut obj = BTreeMap::new();
                    // Loop until we see `EndObject`
                    loop {
                        match parser.peek() {
                            Some(Ok(ParserEvent::EndObject)) => {
                                parser.next(); // Consume the EndObject
                                break Ok(JsonValue::Object(obj));
                            }
                            Some(Ok(ParserEvent::Key(_))) => {
                                // Get the key
                                let key = match parser.next() {
                                    Some(Ok(ParserEvent::Key(key))) => key.into_owned(),
                                    _ => unreachable!(), // We just peeked
                                };
                                // Get the value
                                let val = parse_one(parser)?;
                                obj.insert(key, val);
                            }
                            Some(Ok(_)) => {
                                // This branch should be unreachable.
                                // The StreamingParser would see a non-string token
                                // (like a number) and return
                                // `Err("Expected '}' or a string key")`.
                                // This `Err` would be caught by `Some(Err(_))`.
                                unreachable!("Invalid event in object");
                            }
                            Some(Err(_)) => {
                                // Propagate the error
                                return Err(parser.next().unwrap().unwrap_err());
                            }
                            None => {
                                // This branch should be unreachable.
                                // The StreamingParser will return `Some(Err("Unclosed object"))`
                                // which `peek()` will cache, causing the
                                // `Some(Err(_))` branch to be taken.
                                unreachable!("Unclosed object branch hit None");
                            }
                        }
                    }
                }

                // Invalid start
                ParserEvent::Key(_) | ParserEvent::EndArray | ParserEvent::EndObject => {
                    // This should be unreachable. The StreamingParser's
                    // state machine should never emit these events when
                    // `parse_one` is expecting a value. It would
                    // return `Err("Expected a value")` instead.
                    unreachable!("Invalid start event: {:?}", event)
                }
            }
        } // End of `parse_one`

        // Parse the root value
        let root = parse_one(&mut parser)?;

        // Check for trailing tokens
        match parser.next() {
            None => Ok(root),
            Some(Err(e)) => Err(e),
            // This branch is unreachable. The StreamingParser's `next()`
            // will return `Some(Err("Unexpected trailing token"))` if it
            // finds a token when its state stack is empty.
            Some(Ok(event)) => unreachable!("Trailing token was not an error: {:?}", event),
        }
    }
}

impl JsonValue {
    /// Serializes the `JsonValue` into a compact, minified JSON string.
    ///
    /// # Errors
    /// Returns `fmt::Error` if the value contains `f64::NAN` or `f64::INFINITY`.
    pub fn stringify(&self) -> Result<String, fmt::Error> {
        let mut output = String::new();
        Self::write_value(self, &mut output)?;
        Ok(output)
    }

    /// Recursive helper function to write any `JsonValue` to a string buffer.
    fn write_value<W: fmt::Write>(value: &JsonValue, w: &mut W) -> fmt::Result {
        match value {
            JsonValue::Null => w.write_str("null"),
            JsonValue::Boolean(b) => w.write_str(if *b { "true" } else { "false" }),
            // Check for NaN/inf (which are invalid JSON) and use JsonNumber.
            JsonValue::Number(n) => match n {
                JsonNumber::F64(f) if f.is_nan() || f.is_infinite() => {
                    Err(fmt::Error) // Hard error
                }
                _ => write!(w, "{}", n), // Use JsonNumber's Display impl
            },
            JsonValue::String(s) => Self::write_string(s, w),
            JsonValue::Array(a) => Self::write_array(a, w),
            JsonValue::Object(o) => Self::write_object(o, w),
        }
    }

    /// Helper to write a JSON array (compact).
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

    /// Helper to write a JSON object (compact).
    fn write_object<W: fmt::Write>(obj: &BTreeMap<String, JsonValue>, w: &mut W) -> fmt::Result {
        w.write_char('{')?;
        let mut first = true;
        // Note: BTreeMap iteration order IS guaranteed (alphabetical).
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
    /// This handles all required JSON escape sequences (e.g., `\"`, `\\`, `\n`).
    fn write_string<W: fmt::Write>(s: &str, w: &mut W) -> fmt::Result {
        w.write_char('"')?;
        for c in s.chars() {
            match c {
                // Standard escapes
                '"' => w.write_str("\\\""),
                '\\' => w.write_str("\\\\"),
                '/' => w.write_str("\\/"), // Optional, but good practice
                '\u{0008}' => w.write_str("\\b"), // Backspace
                '\u{000C}' => w.write_str("\\f"), // Form feed
                '\n' => w.write_str("\\n"), // Newline
                '\r' => w.write_str("\\r"), // Carriage return
                '\t' => w.write_str("\\t"), // Tab
                // Control characters must be escaped as \uXXXX
                '\u{0000}'..='\u{001F}' => {
                    write!(w, "\\u{:04x}", c as u32)
                }
                _ => w.write_char(c),
            }?;
        }
        w.write_char('"')
    }

    // --- Pretty-Printing Logic ---
    /// The indentation string to use for pretty-printing (two spaces).
    const INDENT: &'static str = "  ";

    /// Serializes the `JsonValue` into a human-readable,
    /// indented JSON string ("pretty-print").
    ///
    /// # Errors
    /// Returns `fmt::Error` if the value contains `f64::NAN` or `f64::INFINITY`.
    pub fn stringify_pretty(&self) -> Result<String, fmt::Error> {
        let mut output = String::new();
        Self::write_value_pretty(self, &mut output, 0)?;
        Ok(output)
    }

    /// Recursive helper for pretty-printing a value.
    fn write_value_pretty<W: fmt::Write>(
        value: &JsonValue,
        w: &mut W,
        depth: usize,
    ) -> fmt::Result {
        match value {
            // Primitives
            JsonValue::Null => w.write_str("null"),
            JsonValue::Boolean(b) => w.write_str(if *b { "true" } else { "false" }),
            JsonValue::Number(n) => match n {
                JsonNumber::F64(f) if f.is_nan() || f.is_infinite() => {
                    Err(fmt::Error) // Hard error
                }
                _ => write!(w, "{}", n), // Use JsonNumber's Display impl
            },
            JsonValue::String(s) => Self::write_string(s, w),
            // Composites
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
        obj: &BTreeMap<String, JsonValue>,
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
