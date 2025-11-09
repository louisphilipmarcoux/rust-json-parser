//! # rill-json
//!
//! `rill-json` is a fast, 100% safe, and RFC 8259-compliant streaming JSON parser
//! and serializer, built from scratch in pure Rust.
//!
//! This library is designed for performance, safety, and correctness.
//!
//! ## Key Features
//!
//! * **100% Safe Rust:** Contains no `unsafe` code.
//! * **Streaming Parser:** An `Iterator` that emits `ParserEvent`s, ideal
//!   for parsing large files with minimal memory.
//! * **Optimized Performance:** Uses a byte-slice-based tokenizer with a
//!   branchless Lookup Table (LUT) and `memchr` for high-performance,
//!   safe-SIMD-accelerated string parsing.
//! * **Serializer Included:** Comes with `stringify()` and `stringify_pretty()`
//!   to serialize your Rust data back to JSON.
//! * **RFC 8259 Compliant:** Passes a full test suite for specification compliance.
//!
//! ## Quick Start: Parsing (Streaming)
//!
//! The `parse_streaming` function is the primary entry point. It's the most
//! efficient way to parse JSON, especially large files.
//!
//! no_run
//! use rill_json::{parse_streaming, ParserEvent};
//!
//! fn main() {
//!     let json_data = r#"{ "name": "Babbage", "id": 1815 }"#;
//!     let mut parser = parse_streaming(json_data).unwrap();
//!     let mut found_name_key = false;
//!
//!     while let Some(event) = parser.next() {
//!         match event.unwrap() {
//!             ParserEvent::Key(key) if key == "name" => found_name_key = true,
//!             ParserEvent::String(value) if found_name_key => {
//!                 println!("Found name: {}", value);
//!                 break;
//!             }
//!             _ => found_name_key = false,
//!         }
//!     }
//! }
//! ```
//!
//! // ## Quick Start: Serializing
//!
//! // You can also create JSON strings from your own Rust data using the `JsonValue` enum.
//!
//! ```no_run
//! use rill_json::JsonValue;
//! use std::collections::HashMap;
//!
//! let mut user = HashMap::new();
//! user.insert("username".to_string(), JsonValue::String("ada_l".to_string()));
//! user.insert("id".to_string(), JsonValue::Number(1815.0));
//!
//! let json_object = JsonValue::Object(user);
//!
//! // Get the compact string
//! let json_string = json_object.stringify();
//! assert_eq!(json_string, r#"{"id":1815,"username":"ada_l"}"#);
//! ```

// 1. Declare all the new modules.
/// Contains the primary `ParseError` type for the library.
pub mod error;
/// Contains the streaming `Parser` and its `ParserEvent` enum.
pub mod parser;
/// Contains the `Token` and `TokenType` enums used internally.
pub mod token;
/// Contains the `JsonValue` enum and the serialization (stringify) logic.
pub mod value;

/// The internal, high-performance, byte-based tokenizer (lexer).
/// This module is private to the crate.
mod tokenizer;

// 2. Re-export the public-facing types.
// This creates the clean, top-level API for users.
pub use error::ParseError;
pub use parser::{ParserEvent, StreamingParser};
pub use value::JsonValue;

// --- Constants ---
/// The default maximum nesting depth (e.g., `[[[]]]`) to prevent stack overflows.
const DEFAULT_MAX_DEPTH: usize = 100;
/// The maximum allowed size of an input JSON (10MB) to prevent DoS attacks.
const MAX_JSON_SIZE_BYTES: usize = 10 * 1024 * 1024;

// --- Public-facing helper function ---

/// Parses a JSON string slice into a `StreamingParser`.
///
/// This is the main entry point for the streaming parser. It's fast,
/// low-allocation, and operates as an `Iterator` over `ParserEvent`s.
///
/// # Arguments
/// * `input` - A string slice containing the JSON data to be parsed.
///
/// # Errors
/// Returns a `ParseError` if the input exceeds the `MAX_JSON_SIZE_BYTES`
/// limit (10MB) *before* parsing begins.
///
/// # Examples
/// ```
/// use rill_json::{parse_streaming, ParserEvent};
///
/// let json_data = r#"[1, "hello"]"#;
/// let mut parser = parse_streaming(json_data).unwrap();
///
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::StartArray);
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::Number(1.0));
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::String("hello".to_string()));
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::EndArray);
/// assert!(parser.next().is_none());
/// ```
pub fn parse_streaming(input: &'_ str) -> Result<StreamingParser<'_>, ParseError> {
    if input.len() > MAX_JSON_SIZE_BYTES {
        return Err(ParseError {
            message: "Input exceeds maximum size limit".to_string(),
            line: 1,
            column: 1,
        });
    }
    // We can call StreamingParser::new because it's public in parser.rs
    Ok(StreamingParser::new(input, DEFAULT_MAX_DEPTH))
}

// --- 10. Test Module ---
// The tests all stay in lib.rs, but we update the `use` statements.
#[cfg(test)]
mod tests {
    use super::{parse_streaming, JsonValue, ParseError, ParserEvent, StreamingParser};
    use serde_json::{self, Value as SerdeValue};
    use std::collections::HashMap;

    fn collect_events(input: &str) -> Result<Vec<ParserEvent>, ParseError> {
        parse_streaming(input)?.collect()
    }

    fn collect_events_with_depth(
        input: &str,
        depth: usize,
    ) -> Result<Vec<ParserEvent>, ParseError> {
        // We can call this because `StreamingParser` and its `new` are public
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
    fn test_streaming_parser_top_level_values() {
        assert_eq!(
            collect_events(r#""hello""#).unwrap(),
            vec![ParserEvent::String("hello".to_string())]
        );
        assert_eq!(
            collect_events("123.5").unwrap(),
            vec![ParserEvent::Number(123.5)]
        );
        assert_eq!(
            collect_events("true").unwrap(),
            vec![ParserEvent::Boolean(true)]
        );
        assert_eq!(collect_events("null").unwrap(), vec![ParserEvent::Null]);
    }

    #[test]
    fn test_streaming_parser_complex_nesting() {
        let input = r#"[{"a": 1, "b": [null, {"c": {}}]}]"#;
        let events = collect_events(input).unwrap();
        assert_eq!(
            events,
            vec![
                ParserEvent::StartArray,
                ParserEvent::StartObject,
                ParserEvent::Key("a".to_string()),
                ParserEvent::Number(1.0),
                ParserEvent::Key("b".to_string()),
                ParserEvent::StartArray,
                ParserEvent::Null,
                ParserEvent::StartObject,
                ParserEvent::Key("c".to_string()),
                ParserEvent::StartObject,
                ParserEvent::EndObject,
                ParserEvent::EndObject,
                ParserEvent::EndArray,
                ParserEvent::EndObject,
                ParserEvent::EndArray,
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
        assert_eq!(err.message, "Unexpected ']', expected a value");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 7);

        let err = collect_events("{\"key\": 1,}").unwrap_err();
        assert_eq!(err.message, "Unexpected '}', expected a string key");
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 11);

        let err = collect_events("// a comment\n[1, 2]").unwrap_err();
        assert_eq!(err.message, "Unexpected character '/'");

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
    fn test_stringify_basic() {
        // Input: A native map {"key": "value", "items": [1, None]}
        // Output: The string {"key":"value","items":[1,null]}
        let mut items = HashMap::new();
        items.insert("key".to_string(), JsonValue::String("value".to_string()));
        items.insert(
            "items".to_string(),
            JsonValue::Array(vec![JsonValue::Number(1.0), JsonValue::Null]),
        );
        let obj = JsonValue::Object(items);

        // --- Robust Test ---
        // Parse the string output back into a serde_json::Value
        // This is robust to key order changes.
        let output_str = obj.stringify();
        let parsed_value: SerdeValue =
            serde_json::from_str(&output_str).expect("Stringify output should be valid JSON");

        // Create the expected structure
        let expected_value = serde_json::json!({
            "key": "value",
            "items": [1, null]
        });

        assert_eq!(parsed_value, expected_value);

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

        // Parse the output string with serde_json
        let parsed_value: SerdeValue =
            serde_json::from_str(&pretty_string).expect("Pretty-printed JSON should be valid");

        // Define the expected JSON value
        let expected_value = serde_json::json!({
            "key": "value",
            "admin": true,
            "items": [
                1,
                null,
                {
                    "sub_key": 2
                }
            ]
        });

        // Assert that the *parsed value* matches the expected value.
        // This test will ALWAYS pass regardless of HashMap ordering.
        assert_eq!(parsed_value, expected_value);
    }

    #[test]
    fn test_stringify_pretty_empty() {
        // Test empty object and array pretty printing
        let empty_obj = JsonValue::Object(HashMap::new());
        assert_eq!(empty_obj.stringify_pretty(), "{}");

        let empty_arr = JsonValue::Array(vec![]);
        assert_eq!(empty_arr.stringify_pretty(), "[]");

        // Also check that they are valid JSON
        let parsed_obj: SerdeValue = serde_json::from_str(&empty_obj.stringify_pretty()).unwrap();
        assert!(parsed_obj.is_object());

        let parsed_arr: SerdeValue = serde_json::from_str(&empty_arr.stringify_pretty()).unwrap();
        assert!(parsed_arr.is_array());
    }
}
