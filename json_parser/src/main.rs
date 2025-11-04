use std::collections::HashMap; // <-- ADDED (for Stage 6, but good to add now)

// --- 1. JsonValue Enum ---
// We've added the Array variant.
#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),              // <-- ADDED
    Object(HashMap<String, JsonValue>), // <-- ADDED (for Stage 6)
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

/// Tries to parse a JSON string.
fn parse_string(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if !input.starts_with('"') {
        return Err("Expected '\"' at start of string");
    }

    match input[1..].find('"') {
        Some(end_index) => {
            let string_content = &input[1..end_index + 1];
            let rest = &input[end_index + 2..];
            Ok((JsonValue::String(string_content.to_string()), rest))
        }
        None => Err("Unmatched '\"' at end of string"),
    }
}

// --- NEW FUNCTION for Stage 5 ---
/// Tries to parse a JSON array.
fn parse_array(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if !input.starts_with('[') {
        return Err("Expected '[' at start of array");
    }

    // Get rid of the opening '['
    let mut current_input = &input[1..];
    let mut elements = Vec::new();

    // Handle empty array: []
    if current_input.starts_with(']') {
        return Ok((JsonValue::Array(elements), &current_input[1..]));
    }

    // Loop to parse elements
    loop {
        // 1. Parse a value
        let (value, rest) = parse_value(current_input)?;
        elements.push(value);
        current_input = rest;

        // 2. See what's next: a comma or a closing bracket
        if current_input.starts_with(',') {
            // Consume the comma and loop again
            current_input = &current_input[1..];
        } else if current_input.starts_with(']') {
            // Consume the bracket and finish
            current_input = &current_input[1..];
            break;
        } else {
            // Anything else is an error
            return Err("Expected ',' or ']' after array element");
        }
    }

    Ok((JsonValue::Array(elements), current_input))
}
// --- END NEW FUNCTION ---

/// Tries to parse any valid JSON value from the beginning of the input.
// --- UPDATED to include arrays ---
fn parse_value(input: &str) -> Result<(JsonValue, &str), &'static str> {
    match input.chars().next() {
        Some('n') => parse_null(input),
        Some('t') | Some('f') => parse_boolean(input),
        Some('-') | Some('0'..='9') => parse_number(input),
        Some('"') => parse_string(input),
        Some('[') => parse_array(input), // <-- ADDED
        // We'll add objects next
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
        assert_eq!(parse_value("true").unwrap(), (JsonValue::Boolean(true), ""));
        assert_eq!(
            parse_value("false").unwrap(),
            (JsonValue::Boolean(false), "")
        );
        assert_eq!(
            parse_value("true}").unwrap(),
            (JsonValue::Boolean(true), "}")
        );
        assert!(parse_value("True").is_err());
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
        assert_eq!(
            parse_value("123, 456").unwrap(),
            (JsonValue::Number(123.0), ", 456")
        );
        assert!(parse_value("1.2.3").is_err());
    }

    // --- Stage 4 Tests ---
    #[test]
    fn test_parse_strings() {
        assert_eq!(
            parse_value("\"\"").unwrap(),
            (JsonValue::String("".to_string()), "")
        );
        assert_eq!(
            parse_value("\"hello\"").unwrap(),
            (JsonValue::String("hello".to_string()), "")
        );
        assert_eq!(
            parse_value("\"hello\", 123").unwrap(),
            (JsonValue::String("hello".to_string()), ", 123")
        );
        assert!(parse_value("\"hello").is_err());
        assert!(parse_value("hello").is_err());
    }

    // --- NEW TESTS for Stage 5 ---
    #[test]
    fn test_parse_arrays() {
        // Valid empty array
        let (value, rest) = parse_value("[]").unwrap();
        assert_eq!(value, JsonValue::Array(vec![]));
        assert_eq!(rest, "");

        // Valid array of numbers
        let (value, rest) = parse_value("[1,2,3]").unwrap();
        assert_eq!(
            value,
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::Number(2.0),
                JsonValue::Number(3.0)
            ])
        );
        assert_eq!(rest, "");

        // Valid array of mixed types
        let (value, rest) = parse_value("[1,\"hello\",null,true]").unwrap();
        assert_eq!(
            value,
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::String("hello".to_string()),
                JsonValue::Null,
                JsonValue::Boolean(true)
            ])
        );
        assert_eq!(rest, "");

        // Valid nested array (this proves recursion!)
        let (value, rest) = parse_value("[[1,2],[3,4]]").unwrap();
        assert_eq!(
            value,
            JsonValue::Array(vec![
                JsonValue::Array(vec![JsonValue::Number(1.0), JsonValue::Number(2.0)]),
                JsonValue::Array(vec![JsonValue::Number(3.0), JsonValue::Number(4.0)])
            ])
        );
        assert_eq!(rest, "");

        // Invalid: Missing closing bracket
        assert!(parse_value("[1, 2").is_err());

        // Invalid: Missing comma
        // Our parser will fail this: it parses `1`, then `parse_number` fails on `2`
        // because `parse_value` was expecting `]` or `,`.
        assert!(parse_value("[1 2 3]").is_err());

        // Invalid: Empty element
        // `parse_value` will be called on `, 2` which will fail.
        assert!(parse_value("[1,,2]").is_err());
    }
}
