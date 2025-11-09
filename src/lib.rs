//! # rill-json
//!
//! `rill-json` is a fast, 100% safe, and RFC 8259-compliant streaming JSON parser
//! and serializer, built from scratch in pure Rust.
//!
//! This library is designed for performance, safety, and correctness.
//!
//! ## Key Features
//!
//! * **100% Safe Rust:** Contains no `#![forbid(unsafe_code)]`.
//! * **Streaming Parser:** An `Iterator` that emits `ParserEvent`s, ideal
//!   for parsing large files with minimal memory.
//! * **Optimized Performance:** Uses a byte-slice-based tokenizer with a
//!   branchless Lookup Table (LUT) and `memchr` for high-performance,
//!   safe-SIMD-accelerated string parsing.
//! * **Zero-Allocation String Parsing:** Returns borrowed string slices (`&str`)
//!   when no JSON escapes are present, avoiding allocations.
//! * **Serializer Included:** Comes with `stringify()` and `stringify_pretty()`
//!   to serialize your Rust data back to JSON.
//! * **RFC 8259 Compliant:** Passes a full test suite for specification compliance.
//!
//! ## Quick Start: Parsing (Streaming)
//!
//! The `parse_streaming` function is the primary entry point. It's the most
//! efficient way to parse JSON, especially large files.
//!
//! ```no_run
//! #[allow(clippy::needless_doctest_main)]
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
//! ## Quick Start: Parsing (In-Memory)
//!
//! For convenience, you can also parse directly to an in-memory `JsonValue`.
//!
//! ```no_run
//! use rill_json::{JsonValue, JsonNumber};
//! use std::collections::BTreeMap;
//!
//! let json_data = r#"{ "id": 1815 }"#;
//! let parsed = JsonValue::parse(json_data).unwrap();
//!
//! let mut expected_map = BTreeMap::new();
//! expected_map.insert("id".to_string(), JsonValue::Number(JsonNumber::I64(1815)));
//! let expected_val = JsonValue::Object(expected_map);
//!
//! assert_eq!(parsed, expected_val);
//! ```
//!
//! ## Quick Start: Serializing
//!
//! You can also create JSON strings from your own Rust data using the `JsonValue` enum.
//!
//! ```no_run
//! use rill_json::{JsonValue, JsonNumber};
//! use std::collections::BTreeMap;
//!
//! let mut user = BTreeMap::new();
//! user.insert("username".to_string(), JsonValue::String("ada_l".to_string()));
//! user.insert("id".to_string(), JsonValue::Number(JsonNumber::I64(1815)));
//!
//! let json_object = JsonValue::Object(user);
//!
//! // Get the compact string
//! let json_string = json_object.stringify().unwrap();
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
pub use value::{JsonNumber, JsonValue}; // <-- Added JsonNumber

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
/// use rill_json::{parse_streaming, ParserEvent, JsonNumber};
///
/// let json_data = r#"[1, "hello"]"#;
/// let mut parser = parse_streaming(json_data).unwrap();
///
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::StartArray);
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::Number(JsonNumber::I64(1)));
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::String("hello".into()));
/// assert_eq!(parser.next().unwrap().unwrap(), ParserEvent::EndArray);
/// assert!(parser.next().is_none());
/// ```
pub fn parse_streaming<'a>(input: &'a str) -> Result<StreamingParser<'a>, ParseError> {
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

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    // Use the public API we just defined
    use super::{parse_streaming, JsonNumber, JsonValue, ParseError, ParserEvent, StreamingParser};
    use serde_json::{self, Value as SerdeValue};
    use std::borrow::Cow;

    fn collect_events<'a>(input: &'a str) -> Result<Vec<ParserEvent<'a>>, ParseError> {
        parse_streaming(input)?.collect()
    }

    fn collect_events_with_depth<'a>(
        input: &'a str,
        depth: usize,
    ) -> Result<Vec<ParserEvent<'a>>, ParseError> {
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
                ParserEvent::Key(Cow::Borrowed("key")),
                ParserEvent::StartArray,
                ParserEvent::Number(JsonNumber::I64(1)),
                ParserEvent::Null,
                ParserEvent::Boolean(true),
                ParserEvent::String(Cow::Borrowed("hello")),
                ParserEvent::EndArray,
                ParserEvent::EndObject
            ]
        );
    }

    #[test]
    fn test_streaming_parser_escaped_string() {
        let input = r#"["a\n\"b"]"#;
        let events = collect_events(input).unwrap();
        assert_eq!(
            events,
            vec![
                ParserEvent::StartArray,
                ParserEvent::String(Cow::Owned("a\n\"b".to_string())),
                ParserEvent::EndArray
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
            vec![ParserEvent::String("hello".into())]
        );
        assert_eq!(
            collect_events("123.5").unwrap(),
            vec![ParserEvent::Number(JsonNumber::F64(123.5))]
        );
        assert_eq!(
            collect_events("9007199254740993").unwrap(),
            vec![ParserEvent::Number(JsonNumber::I64(9007199254740993))]
        );
        assert_eq!(
            collect_events("9223372036854775808").unwrap(),
            vec![ParserEvent::Number(JsonNumber::U64(9223372036854775808))]
        );
        assert_eq!(
            collect_events("-1234567890").unwrap(),
            vec![ParserEvent::Number(JsonNumber::I64(-1234567890))]
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
                ParserEvent::Key("a".into()),
                ParserEvent::Number(JsonNumber::I64(1)),
                ParserEvent::Key("b".into()),
                ParserEvent::StartArray,
                ParserEvent::Null,
                ParserEvent::StartObject,
                ParserEvent::Key("c".into()),
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

    #[test]
    fn test_stringify_basic() {
        // Input: A native map {"key": "value", "items": [1, None]}
        // Output: The string {"items":[1,null],"key":"value"}
        let mut items = BTreeMap::new();
        items.insert("key".to_string(), JsonValue::String("value".to_string()));
        items.insert(
            "items".to_string(),
            JsonValue::Array(vec![JsonValue::Number(JsonNumber::I64(1)), JsonValue::Null]),
        );
        let obj = JsonValue::Object(items);

        // Parse the string output back into a serde_json::Value
        let output_str = obj.stringify().unwrap();
        let parsed_value: SerdeValue =
            serde_json::from_str(&output_str).expect("Stringify output should be valid JSON");

        // Create the expected structure
        let expected_value = serde_json::json!({
            "key": "value",
            "items": [1, null]
        });

        assert_eq!(parsed_value, expected_value);
        // BTreeMap guarantees key order, so we can also test the string directly
        assert_eq!(output_str, r#"{"items":[1,null],"key":"value"}"#);

        // Test case from challenge:
        // Input: A native string a "quoted" \ string
        // Output: The string "a \"quoted\" \\ string"
        let s = JsonValue::String("a \"quoted\" \\ string".to_string());
        assert_eq!(s.stringify().unwrap(), r#""a \"quoted\" \\ string""#);
    }

    #[test]
    fn test_stringify_all_types() {
        assert_eq!(JsonValue::Null.stringify().unwrap(), "null");
        assert_eq!(JsonValue::Boolean(true).stringify().unwrap(), "true");
        assert_eq!(JsonValue::Boolean(false).stringify().unwrap(), "false");
        assert_eq!(
            JsonValue::Number(JsonNumber::F64(123.45))
                .stringify()
                .unwrap(),
            "123.45"
        );
        assert_eq!(
            JsonValue::Number(JsonNumber::F64(-0.5))
                .stringify()
                .unwrap(),
            "-0.5"
        );
        assert_eq!(
            JsonValue::Number(JsonNumber::I64(1000))
                .stringify()
                .unwrap(),
            "1000"
        );
        assert_eq!(
            JsonValue::Number(JsonNumber::U64(123456789012345))
                .stringify()
                .unwrap(),
            "123456789012345"
        );

        // Empty Structures
        assert_eq!(JsonValue::Array(vec![]).stringify().unwrap(), "[]");
        assert_eq!(
            JsonValue::Object(BTreeMap::new()).stringify().unwrap(),
            "{}"
        );

        // Complex Array
        let arr = JsonValue::Array(vec![
            JsonValue::Number(JsonNumber::I64(1)),
            JsonValue::String("test".to_string()),
            JsonValue::Boolean(true),
            JsonValue::Null,
            JsonValue::Object(BTreeMap::new()),
        ]);
        assert_eq!(arr.stringify().unwrap(), r#"[1,"test",true,null,{}]"#);
    }

    #[test]
    fn test_stringify_string_escapes() {
        // Test all escapes from Stage 8
        let s = JsonValue::String("\" \\ / \u{0008} \u{000C} \n \r \t".to_string());
        assert_eq!(s.stringify().unwrap(), r#""\" \\ \/ \b \f \n \r \t""#);

        // Test control character escape
        let s_control = JsonValue::String("hello\u{0001}world".to_string());
        assert_eq!(s_control.stringify().unwrap(), r#""hello\u0001world""#);
    }

    #[test]
    fn test_stringify_pretty_print() {
        let mut sub_obj = BTreeMap::new();
        sub_obj.insert("sub_key".to_string(), JsonValue::Number(JsonNumber::I64(2)));

        let mut items = BTreeMap::new();
        items.insert("key".to_string(), JsonValue::String("value".to_string()));
        items.insert(
            "items".to_string(),
            JsonValue::Array(vec![
                JsonValue::Number(JsonNumber::I64(1)),
                JsonValue::Null,
                JsonValue::Object(sub_obj),
            ]),
        );
        items.insert("admin".to_string(), JsonValue::Boolean(true));
        let obj = JsonValue::Object(items);

        let pretty_string = obj.stringify_pretty().unwrap();

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
        assert_eq!(parsed_value, expected_value);

        // With BTreeMap, we can also test the exact string output
        let expected_string = r#"{
  "admin": true,
  "items": [
    1,
    null,
    {
      "sub_key": 2
    }
  ],
  "key": "value"
}"#;
        assert_eq!(pretty_string, expected_string);
    }

    #[test]
    fn test_stringify_pretty_empty() {
        // Test empty object and array
        assert_eq!(
            JsonValue::Object(BTreeMap::new())
                .stringify_pretty()
                .unwrap(),
            "{}"
        );
        assert_eq!(JsonValue::Array(vec![]).stringify_pretty().unwrap(), "[]");
    }

    #[test]
    fn test_stringify_nan_inf() {
        let val_nan = JsonValue::Number(JsonNumber::F64(f64::NAN));
        let val_inf = JsonValue::Number(JsonNumber::F64(f64::INFINITY));
        let val_neg_inf = JsonValue::Number(JsonNumber::F64(f64::NEG_INFINITY));

        // We defined this as a hard error
        assert!(val_nan.stringify().is_err());
        assert!(val_inf.stringify().is_err());
        assert!(val_neg_inf.stringify().is_err());

        // Test pretty print
        assert!(val_nan.stringify_pretty().is_err());
        assert!(val_inf.stringify_pretty().is_err());
    }

    #[test]
    fn test_value_parse_simple() {
        let input = r#"{ "key": [1, null, true, "hello"] }"#;
        let value = JsonValue::parse(input).unwrap();

        let mut obj = BTreeMap::new();
        obj.insert(
            "key".to_string(),
            JsonValue::Array(vec![
                JsonValue::Number(JsonNumber::I64(1)),
                JsonValue::Null,
                JsonValue::Boolean(true),
                JsonValue::String("hello".to_string()),
            ]),
        );
        let expected = JsonValue::Object(obj);

        assert_eq!(value, expected);
    }

    #[test]
    fn test_value_parse_primitives() {
        assert_eq!(JsonValue::parse("null").unwrap(), JsonValue::Null);
        assert_eq!(
            JsonValue::parse("123").unwrap(),
            JsonValue::Number(JsonNumber::I64(123))
        );
        assert_eq!(
            JsonValue::parse("-456").unwrap(),
            JsonValue::Number(JsonNumber::I64(-456))
        );
        assert_eq!(
            JsonValue::parse("123.5").unwrap(),
            JsonValue::Number(JsonNumber::F64(123.5))
        );
        assert_eq!(
            JsonValue::parse("1e5").unwrap(),
            JsonValue::Number(JsonNumber::F64(100000.0))
        );
        assert_eq!(
            JsonValue::parse("9007199254740993").unwrap(),
            JsonValue::Number(JsonNumber::I64(9007199254740993))
        );
        assert_eq!(
            JsonValue::parse("9223372036854775808").unwrap(),
            JsonValue::Number(JsonNumber::U64(9223372036854775808))
        );
        assert_eq!(
            JsonValue::parse(r#""hi""#).unwrap(),
            JsonValue::String("hi".to_string())
        );
        assert_eq!(
            JsonValue::parse(r#""esc\n""#).unwrap(),
            JsonValue::String("esc\n".to_string())
        );
    }

    #[test]
    fn test_value_parse_errors() {
        assert_eq!(JsonValue::parse("").unwrap_err().message, "Empty input");
        assert_eq!(
            JsonValue::parse("{").unwrap_err().message,
            "Unclosed object"
        );
        assert_eq!(
            JsonValue::parse("[1, 2").unwrap_err().message,
            "Unclosed array"
        );
        assert!(JsonValue::parse("[1, 2]").is_ok());
        assert_eq!(
            JsonValue::parse("[1, 2] extra").unwrap_err().message,
            "Unexpected character 'e'"
        );
        assert_eq!(
            JsonValue::parse(r#"{ "key": 1, }"#).unwrap_err().message,
            "Unexpected '}', expected a string key"
        );
    }
}
