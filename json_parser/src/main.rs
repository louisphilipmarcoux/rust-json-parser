use std::collections::HashMap;

// --- 1. JsonValue Enum ---
#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

// --- 2. Parser Functions ---

// --- NEW FUNCTION for Stage 7 ---
/// Skips any insignificant whitespace (space, newline, tab, carriage return).
/// Returns a string slice of the input starting *after* the whitespace.
fn skip_whitespace(input: &str) -> &str {
    input.trim_start()
}
// --- END NEW FUNCTION ---

fn parse_null(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if input.starts_with("null") {
        Ok((JsonValue::Null, &input[4..]))
    } else {
        Err("Expected 'null'")
    }
}

fn parse_boolean(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if input.starts_with("true") {
        Ok((JsonValue::Boolean(true), &input[4..]))
    } else if input.starts_with("false") {
        Ok((JsonValue::Boolean(false), &input[5..]))
    } else {
        Err("Expected 'true' or 'false'")
    }
}

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

// --- UPDATED for Stage 7 ---
fn parse_array(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if !input.starts_with('[') {
        return Err("Expected '[' at start of array");
    }

    // Skip opening '[' and initial whitespace
    let mut current_input = skip_whitespace(&input[1..]);
    let mut elements = Vec::new();

    // Handle empty array: []
    if current_input.starts_with(']') {
        return Ok((JsonValue::Array(elements), &current_input[1..]));
    }

    // Loop to parse elements
    loop {
        // 1. Parse a value (which now also skips whitespace)
        let (value, rest) = parse_value(current_input)?;
        elements.push(value);
        current_input = skip_whitespace(rest); // Skip whitespace *after* value

        // 2. See what's next: a comma or a closing bracket
        if current_input.starts_with(',') {
            // Consume the comma and skip whitespace after it
            current_input = skip_whitespace(&current_input[1..]);
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
// --- END UPDATE ---

// --- UPDATED for Stage 7 ---
fn parse_object(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if !input.starts_with('{') {
        return Err("Expected '{' at start of object");
    }

    // Skip opening '{' and initial whitespace
    let mut current_input = skip_whitespace(&input[1..]);
    let mut map = HashMap::new();

    // Handle empty object: {}
    if current_input.starts_with('}') {
        return Ok((JsonValue::Object(map), &current_input[1..]));
    }

    // Loop to parse key-value pairs
    loop {
        // 1. Parse the key (must be a string)
        let (key_value, rest) = parse_string(current_input)?;
        let key = match key_value {
            JsonValue::String(s) => s,
            _ => return Err("Object key is not a string"),
        };
        current_input = skip_whitespace(rest); // Skip whitespace *after* key

        // 2. Expect and consume the colon
        if !current_input.starts_with(':') {
            return Err("Expected ':' after object key");
        }
        current_input = skip_whitespace(&current_input[1..]); // Skip whitespace *after* colon

        // 3. Parse the value
        let (value, rest) = parse_value(current_input)?; // parse_value handles its own whitespace
        map.insert(key, value);
        current_input = skip_whitespace(rest); // Skip whitespace *after* value

        // 4. See what's next: a comma or a closing brace
        if current_input.starts_with(',') {
            // Consume the comma and skip whitespace
            current_input = skip_whitespace(&current_input[1..]);
        } else if current_input.starts_with('}') {
            // Consume the brace and finish
            current_input = &current_input[1..];
            break;
        } else {
            return Err("Expected ',' or '}' after object value");
        }
    }
    Ok((JsonValue::Object(map), current_input))
}
// --- END UPDATE ---

/// Tries to parse any valid JSON value from the beginning of the input.
// --- UPDATED for Stage 7 ---
fn parse_value(input: &str) -> Result<(JsonValue, &str), &'static str> {
    // 1. Skip any preceding whitespace!
    let input = skip_whitespace(input);

    // 2. Look at the first *meaningful* character
    let parse_result = match input.chars().next() {
        Some('n') => parse_null(input),
        Some('t') | Some('f') => parse_boolean(input),
        Some('-') | Some('0'..='9') => parse_number(input),
        Some('"') => parse_string(input),
        Some('[') => parse_array(input),
        Some('{') => parse_object(input),
        Some(_) => Err("Invalid character at start of value"), // More specific error
        None => Err("Unexpected end of input"),
    };

    parse_result.map(|(value, rest)| (value, skip_whitespace(rest)))
}
// --- END UPDATE ---

// --- 3. Main Function ---
fn main() {
    println!("JSON Parser. Run 'cargo test' to execute tests.");
}

// --- 4. Test Module ---
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

    #[test]
    fn test_parse_null() { /* ... (keep your old tests) ... */
    }
    #[test]
    fn test_parse_booleans() { /* ... (keep your old tests) ... */
    }
    #[test]
    fn test_parse_numbers() { /* ... (keep your old tests) ... */
    }
    #[test]
    fn test_parse_strings() { /* ... (keep your old tests) ... */
    }
    #[test]
    fn test_parse_arrays() { /* ... (keep your old tests) ... */
    }

    // --- This test should now PASS! ---
    #[test]
    fn test_parse_objects() {
        // Valid empty object
        assert_eq!(
            parse_value("{}").unwrap().0,
            JsonValue::Object(HashMap::new())
        );

        // Valid simple object
        assert_eq!(
            parse_value("{\"key\": \"value\"}").unwrap().0,
            JsonValue::Object(hashmap! { "key" => JsonValue::String("value".to_string()) })
        );

        // Valid object with mixed values (THIS WAS THE FAILING TEST)
        let (value, rest) =
            parse_value("{\"name\": \"Babbage\", \"age\": 30, \"admin\": true}").unwrap();
        assert_eq!(
            value,
            JsonValue::Object(hashmap! {
                "name" => JsonValue::String("Babbage".to_string()),
                "age" => JsonValue::Number(30.0),
                "admin" => JsonValue::Boolean(true)
            })
        );
        assert_eq!(rest, ""); // Ensure it parses the whole thing

        // Valid nested object
        assert_eq!(
            parse_value("{\"nested\": {\"key\": [null, 1]}}").unwrap().0,
            JsonValue::Object(hashmap! {
                "nested" => JsonValue::Object(hashmap! {
                    "key" => JsonValue::Array(vec![
                        JsonValue::Null,
                        JsonValue::Number(1.0)
                    ])
                })
            })
        );

        // Invalid cases
        assert!(parse_value("{key: \"value\"}").is_err());
        assert!(parse_value("{\"key\" \"value\"}").is_err());
        assert!(parse_value("{\"key\": value}").is_err());
    }

    // --- NEW TESTS for Stage 7 ---
    #[test]
    fn test_parse_with_whitespace() {
        // Test whitespace around all token types
        assert_eq!(parse_value(" null ").unwrap().0, JsonValue::Null);
        assert_eq!(
            parse_value(" \n true \t ").unwrap().0,
            JsonValue::Boolean(true)
        );
        assert_eq!(
            parse_value(" \t 123 \n ").unwrap().0,
            JsonValue::Number(123.0)
        );
        assert_eq!(
            parse_value(" \n \"hello\" \t ").unwrap().0,
            JsonValue::String("hello".to_string())
        );

        // Test whitespace inside arrays
        let (value, rest) = parse_value(" [ 1 , 2 , 3 ] ").unwrap();
        assert_eq!(
            value,
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::Number(2.0),
                JsonValue::Number(3.0)
            ])
        );
        assert_eq!(rest, "");

        // Test whitespace inside objects
        let (value, rest) = parse_value(" { \n \"key\" \t : \n \"value\" \n } ").unwrap();
        assert_eq!(
            value,
            JsonValue::Object(hashmap! { "key" => JsonValue::String("value".to_string()) })
        );
        assert_eq!(rest, "");

        // Test complex nested structure with whitespace
        let input = "
            [
                1,
                \"hello\",
                [
                    null
                ]
            ]
        ";
        assert_eq!(
            parse_value(input).unwrap().0,
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::String("hello".to_string()),
                JsonValue::Array(vec![JsonValue::Null])
            ])
        );

        // Invalid: Whitespace inside a token [cite: 128]
        assert!(parse_value("n ull").is_err());
        assert!(parse_value("t rue").is_err());
        let (value, rest) = parse_value("1 23").unwrap();
        assert_eq!(value, JsonValue::Number(1.0));
        assert_eq!(rest, "23");
    }
}
