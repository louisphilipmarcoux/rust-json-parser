// --- 1. JsonValue Enum ---
// We've added the String variant.
#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String), // <-- ADDED
                    // Array(Vec<JsonValue>),
                    // Object(std::collections::HashMap<String, JsonValue>),
}

// --- 2. Parser Functions ---

/// Tries to parse a 'null' literal.
fn parse_null(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if input.starts_with("null") {
        Ok((JsonValue::Null, &input[4..]))
    } else {
        Err("Expected 'null'")
    }
}

/// Tries to parse a 'true' or 'false' literal.
fn parse_boolean(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if input.starts_with("true") {
        Ok((JsonValue::Boolean(true), &input[4..]))
    } else if input.starts_with("false") {
        Ok((JsonValue::Boolean(false), &input[5..]))
    } else {
        Err("Expected 'true' or 'false'")
    }
}

/// Tries to parse a JSON number (integer or float).
fn parse_number(input: &str) -> Result<(JsonValue, &str), &'static str> {
    let end_index = input
        .find(|c: char| c.is_whitespace() || c == ',' || c == ']' || c == '}')
        .unwrap_or(input.len());

    let num_str = &input[..end_index];

    match num_str.parse::<f64>() {
        Ok(num) => Ok((JsonValue::Number(num), &input[end_index..])),
        Err(_) => Err("Invalid number format"),
    }
}

// --- NEW FUNCTION for Stage 4 ---
/// Tries to parse a JSON string.
/// This implementation is simple and does not yet handle escape sequences.
fn parse_string(input: &str) -> Result<(JsonValue, &str), &'static str> {
    // Check if it starts with a double quote
    if !input.starts_with('"') {
        return Err("Expected '\"' at start of string");
    }

    // Find the closing double quote.
    // We skip the first character (the opening quote).
    // `find` will locate the *next* quote.
    // Note: This simple version doesn't handle escaped quotes (\") yet.
    match input[1..].find('"') {
        Some(end_index) => {
            // The `end_index` is relative to `&input[1..]`.
            // The actual content of the string is from index 1 up to `end_index + 1`.
            let string_content = &input[1..end_index + 1];

            // The rest of the input starts *after* the closing quote.
            let rest = &input[end_index + 2..];

            Ok((JsonValue::String(string_content.to_string()), rest))
        }
        None => {
            // We didn't find a closing quote
            Err("Unmatched '\"' at end of string") // [cite: 68]
        }
    }
}
// --- END NEW FUNCTION ---

/// Tries to parse any valid JSON value from the beginning of the input.
// --- UPDATED to include strings ---
fn parse_value(input: &str) -> Result<(JsonValue, &str), &'static str> {
    match input.chars().next() {
        Some('n') => parse_null(input),
        Some('t') | Some('f') => parse_boolean(input),
        Some('-') | Some('0'..='9') => parse_number(input),
        Some('"') => parse_string(input), // <-- ADDED
        // We'll add more cases here for arrays, objects, etc.
        _ => Err("Expected a valid JSON value"),
    }
}

// --- 3. Main Function ---
fn main() {
    println!("JSON Parser. Run 'cargo test' to execute tests.");
}

// --- 4. Test Module ---
#[cfg(test)]
mod tests {
    use super::*;

    // --- Stage 1 Tests ---
    #[test]
    fn test_parse_null() {
        let (value, rest) = parse_value("null").unwrap();
        assert_eq!(value, JsonValue::Null);
        assert_eq!(rest, "");

        let (value, rest) = parse_value("null, 123").unwrap();
        assert_eq!(value, JsonValue::Null);
        assert_eq!(rest, ", 123");

        assert!(parse_value("nul").is_err());
        assert!(parse_value("NULL").is_err());
    }

    // --- Stage 2 Tests ---
    #[test]
    fn test_parse_booleans() {
        let (value, rest) = parse_value("true").unwrap();
        assert_eq!(value, JsonValue::Boolean(true));
        assert_eq!(rest, "");

        let (value, rest) = parse_value("false").unwrap();
        assert_eq!(value, JsonValue::Boolean(false));
        assert_eq!(rest, "");

        let (value, rest) = parse_value("true}").unwrap();
        assert_eq!(value, JsonValue::Boolean(true));
        assert_eq!(rest, "}");

        assert!(parse_value("True").is_err());
        assert!(parse_value("fals").is_err());
    }

    // --- Stage 3 Tests ---
    #[test]
    fn test_parse_numbers() {
        assert_eq!(parse_value("0").unwrap(), (JsonValue::Number(0.0), ""));
        assert_eq!(parse_value("123").unwrap(), (JsonValue::Number(123.0), ""));
        assert_eq!(
            parse_value("-123").unwrap(),
            (JsonValue::Number(-123.0), "")
        );
        assert_eq!(
            parse_value("45.67").unwrap(),
            (JsonValue::Number(45.67), "")
        );
        assert_eq!(parse_value("-0.5").unwrap(), (JsonValue::Number(-0.5), ""));

        let (value, rest) = parse_value("123, 456").unwrap();
        assert_eq!(value, JsonValue::Number(123.0));
        assert_eq!(rest, ", 456");

        assert!(parse_value("1.2.3").is_err());
        assert!(parse_value("--1").is_err());
    }

    // --- NEW TESTS for Stage 4 ---
    #[test]
    fn test_parse_strings() {
        let (value, rest) = parse_value("\"\"").unwrap();
        assert_eq!(value, JsonValue::String("".to_string()));
        assert_eq!(rest, "");

        let (value, rest) = parse_value("\"hello\"").unwrap();
        assert_eq!(value, JsonValue::String("hello".to_string()));
        assert_eq!(rest, "");

        let (value, rest) = parse_value("\"hello\", 123").unwrap();
        assert_eq!(value, JsonValue::String("hello".to_string()));
        assert_eq!(rest, ", 123");

        assert!(parse_value("\"hello").is_err());

        assert!(parse_value("hello").is_err());

        assert!(parse_value("'hello'").is_err());
    }
}
