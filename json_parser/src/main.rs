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

fn skip_whitespace(input: &str) -> &str {
    input.trim_start()
}

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

    // --- NEW VALIDATION for Stage 10 ---
    if num_str.is_empty() {
        return Err("Invalid number format: empty string");
    }

    // Fail cases like "1.e1" or "1.E1"
    if let Some(e_pos) = num_str.find(['e', 'E']) {
        if e_pos > 0 {
            // Check the character right before the 'e' or 'E'
            if let Some(char_before_e) = num_str.chars().nth(e_pos - 1) {
                if char_before_e == '.' {
                    return Err("Invalid number format: decimal point must be followed by digits");
                }
            }
        }
    }

    // Fail cases like "1e", "1e+", "1e-" [cite: 174]
    if let Some(last_char) = num_str.chars().last() {
        if last_char == 'e' || last_char == 'E' || last_char == '+' || last_char == '-' {
            return Err("Invalid number format: incomplete exponent");
        }
    }
    // --- END NEW VALIDATION ---

    // Now, try to parse the (mostly) validated string
    match num_str.parse::<f64>() {
        Ok(num) => Ok((JsonValue::Number(num), &input[end_index..])),
        Err(_) => Err("Invalid number format"), // Catches other errors like "--1" [cite: 58]
    }
}

/// Tries to parse a JSON string, handling escape sequences and Unicode.
fn parse_string(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if !input.starts_with('"') {
        return Err("Expected '\"' at start of string");
    }

    let mut parsed_content = String::new();
    let mut chars = input[1..].chars().enumerate();

    while let Some((i, c)) = chars.next() {
        match c {
            '\\' => {
                if let Some((_, escaped_char)) = chars.next() {
                    match escaped_char {
                        '"' | '\\' | '/' => parsed_content.push(escaped_char),
                        'b' => parsed_content.push('\u{0008}'),
                        'f' => parsed_content.push('\u{000C}'),
                        'n' => parsed_content.push('\n'),
                        'r' => parsed_content.push('\r'),
                        't' => parsed_content.push('\t'),
                        'u' => {
                            // --- NEW Surrogate-Aware Logic ---

                            // Helper closure to parse 4 hex digits from the main iterator
                            let parse_hex_4 = |chars: &mut std::iter::Enumerate<std::str::Chars<'_>>| -> Result<u32, &'static str> {
                                let mut hex_code = String::with_capacity(4);
                                for _ in 0..4 {
                                    if let Some((_, hex_char)) = chars.next() {
                                        if hex_char.is_ascii_hexdigit() {
                                            hex_code.push(hex_char);
                                        } else {
                                            return Err("Invalid Unicode: Non-hex char");
                                        }
                                    } else {
                                        return Err("Invalid Unicode: Incomplete sequence");
                                    }
                                }
                                u32::from_str_radix(&hex_code, 16).map_err(|_| "Invalid Unicode: Parse error")
                            };

                            let code1 = parse_hex_4(&mut chars)?;

                            if (0xD800..=0xDBFF).contains(&code1) {
                                // High surrogate. Must be followed by a low surrogate.
                                if !(chars.next().map(|(_, c)| c) == Some('\\')
                                    && chars.next().map(|(_, c)| c) == Some('u'))
                                {
                                    return Err("Invalid Unicode: Unpaired high surrogate");
                                }

                                let code2 = parse_hex_4(&mut chars)?;

                                if !(0xDC00..=0xDFFF).contains(&code2) {
                                    return Err("Invalid Unicode: Expected low surrogate");
                                }

                                // Combine them
                                let high = code1 - 0xD800;
                                let low = code2 - 0xDC00;
                                let combined = 0x10000 + (high << 10) + low;

                                match std::char::from_u32(combined) {
                                    Some(c) => parsed_content.push(c),
                                    None => {
                                        return Err(
                                            "Invalid Unicode: Combined code point out of range",
                                        );
                                    }
                                }
                            } else if (0xDC00..=0xDFFF).contains(&code1) {
                                // Unpaired low surrogate
                                return Err("Invalid Unicode: Unpaired low surrogate");
                            } else {
                                // Normal Basic Multilingual Plane (BMP) char
                                match std::char::from_u32(code1) {
                                    Some(c) => parsed_content.push(c),
                                    None => return Err("Invalid Unicode code point"),
                                }
                            }
                            // --- END NEW LOGIC ---
                        }
                        _ => return Err("Invalid escape sequence"),
                    }
                } else {
                    return Err("Unmatched '\"' at end of string");
                }
            }
            '"' => {
                let rest = &input[i + 2..];
                return Ok((JsonValue::String(parsed_content), rest));
            }
            _ => parsed_content.push(c),
        }
    }
    Err("Unmatched '\"' at end of string")
}

fn parse_array(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if !input.starts_with('[') {
        return Err("Expected '[' at start of array");
    }
    let mut current_input = skip_whitespace(&input[1..]);
    let mut elements = Vec::new();
    if current_input.starts_with(']') {
        return Ok((JsonValue::Array(elements), &current_input[1..]));
    }
    loop {
        let (value, rest) = parse_value(current_input)?;
        elements.push(value);
        current_input = skip_whitespace(rest);
        if current_input.starts_with(',') {
            current_input = skip_whitespace(&current_input[1..]);
        } else if current_input.starts_with(']') {
            current_input = &current_input[1..];
            break;
        } else {
            return Err("Expected ',' or ']' after array element");
        }
    }
    Ok((JsonValue::Array(elements), current_input))
}

fn parse_object(input: &str) -> Result<(JsonValue, &str), &'static str> {
    if !input.starts_with('{') {
        return Err("Expected '{' at start of object");
    }
    let mut current_input = skip_whitespace(&input[1..]);
    let mut map = HashMap::new();
    if current_input.starts_with('}') {
        return Ok((JsonValue::Object(map), &current_input[1..]));
    }
    loop {
        let (key_value, rest) = parse_string(current_input)?;
        let key = match key_value {
            JsonValue::String(s) => s,
            _ => return Err("Object key is not a string"),
        };
        current_input = skip_whitespace(rest);
        if !current_input.starts_with(':') {
            return Err("Expected ':' after object key");
        }
        current_input = skip_whitespace(&current_input[1..]);
        let (value, rest) = parse_value(current_input)?;
        map.insert(key, value);
        current_input = skip_whitespace(rest);
        if current_input.starts_with(',') {
            current_input = skip_whitespace(&current_input[1..]);
        } else if current_input.starts_with('}') {
            current_input = &current_input[1..];
            break;
        } else {
            return Err("Expected ',' or '}' after object value");
        }
    }
    Ok((JsonValue::Object(map), current_input))
}

/// Tries to parse any valid JSON value from the beginning of the input.
fn parse_value(input: &str) -> Result<(JsonValue, &str), &'static str> {
    let input = skip_whitespace(input);
    let parse_result = match input.chars().next() {
        Some('n') => parse_null(input),
        Some('t') | Some('f') => parse_boolean(input),
        Some('-') | Some('0'..='9') => parse_number(input),
        Some('"') => parse_string(input),
        Some('[') => parse_array(input),
        Some('{') => parse_object(input),
        Some(_) => Err("Invalid character at start of value"),
        None => Err("Unexpected end of input"),
    };
    parse_result.map(|(value, rest)| (value, skip_whitespace(rest)))
}

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
    fn test_parse_null() {
        assert_eq!(parse_value("null").unwrap(), (JsonValue::Null, ""));
        let (value, rest) = parse_value("null, 123").unwrap();
        assert_eq!(value, JsonValue::Null);
        assert_eq!(rest, ", 123");
        assert!(parse_value("nul").is_err());
    }

    #[test]
    fn test_parse_booleans() {
        assert_eq!(parse_value("true").unwrap(), (JsonValue::Boolean(true), ""));
        assert_eq!(
            parse_value("false").unwrap(),
            (JsonValue::Boolean(false), "")
        );
        assert!(parse_value("True").is_err());
    }

    #[test]
    fn test_parse_numbers() {
        assert_eq!(parse_value("123").unwrap(), (JsonValue::Number(123.0), ""));
        assert_eq!(parse_value("-0.5").unwrap(), (JsonValue::Number(-0.5), ""));
        let (value, rest) = parse_value("123, 456").unwrap();
        assert_eq!(value, JsonValue::Number(123.0));
        assert_eq!(rest, ", 456");
        assert!(parse_value("1.2.3").is_err());
    }

    #[test]
    fn test_parse_strings_basic() {
        assert_eq!(
            parse_value("\"hello\"").unwrap(),
            (JsonValue::String("hello".to_string()), "")
        );
        let (value, rest) = parse_value("\"hello\", 123").unwrap();
        assert_eq!(value, JsonValue::String("hello".to_string()));
        assert_eq!(rest, ", 123");
        assert!(parse_value("\"hello").is_err());
    }

    #[test]
    fn test_parse_string_escapes() {
        // Test escaped quote
        let (value, _) = parse_value("\"hello \\\"quoted\\\" world\"").unwrap();
        assert_eq!(
            value,
            JsonValue::String("hello \"quoted\" world".to_string())
        );
        // Test common escapes
        let (value, _) = parse_value("\"line1\\nline2\\t-tabbed\"").unwrap();
        assert_eq!(
            value,
            JsonValue::String("line1\nline2\t-tabbed".to_string())
        );
        // Invalid: Invalid escape sequence
        assert!(parse_value("\"invalid \\a escape\"").is_err());
    }

    // --- THIS TEST IS NOW CORRECTED ---
    #[test]
    fn test_parse_string_unicode() {
        // Valid: "Hello"
        let (value, _) = parse_value("\"\\u0048\\u0065\\u006C\\u006C\\u006F\"").unwrap();
        assert_eq!(value, JsonValue::String("Hello".to_string()));

        // Valid: Smiling emoji 😊 (U+1F60A)
        // This is the correct JSON surrogate pair: \uD83D\uDE0A
        let (value, _) = parse_value("\"\\uD83D\\uDE0A\"").unwrap();
        assert_eq!(value, JsonValue::String("😊".to_string()));

        // Invalid: Incomplete sequence
        assert!(parse_value("\"\\u123\"").is_err());
        // Invalid: Non-hex characters
        assert!(parse_value("\"\\uGHIJ\"").is_err());

        // --- New tests for unpaired surrogates ---

        // Invalid: Unpaired high surrogate
        let res = parse_value("\"\\uD83D\"").unwrap_err();
        assert_eq!(res, "Invalid Unicode: Unpaired high surrogate");

        // Invalid: Unpaired high surrogate followed by non-surrogate
        let res = parse_value("\"\\uD83D\\u0048\"").unwrap_err();
        assert_eq!(res, "Invalid Unicode: Expected low surrogate");

        // Invalid: Unpaired low surrogate
        let res = parse_value("\"\\uDE0A\"").unwrap_err();
        assert_eq!(res, "Invalid Unicode: Unpaired low surrogate");
    }

    #[test]
    fn test_parse_arrays() {
        assert_eq!(parse_value("[]").unwrap(), (JsonValue::Array(vec![]), ""));
        let (value, _) = parse_value("[1, \"hello\", null]").unwrap();
        assert_eq!(
            value,
            JsonValue::Array(vec![
                JsonValue::Number(1.0),
                JsonValue::String("hello".to_string()),
                JsonValue::Null
            ])
        );
        assert!(parse_value("[1, 2").is_err());
    }

    // --- THIS TEST IS NOW INCLUDED (FIXES WARNING) ---
    #[test]
    fn test_parse_objects() {
        assert_eq!(
            parse_value("{}").unwrap(),
            (JsonValue::Object(HashMap::new()), "")
        );
        let (value, _) = parse_value("{\"key\": \"value\"}").unwrap();
        assert_eq!(
            value,
            JsonValue::Object(hashmap! { "key" => JsonValue::String("value".to_string()) })
        );
        assert!(parse_value("{key: \"value\"}").is_err());
    }

    #[test]
    fn test_parse_with_whitespace() {
        assert_eq!(parse_value(" null ").unwrap().0, JsonValue::Null);
        assert_eq!(
            parse_value(" \n true \t ").unwrap().0,
            JsonValue::Boolean(true)
        );
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

        let (value, rest) = parse_value("1 23").unwrap();
        assert_eq!(value, JsonValue::Number(1.0));
        assert_eq!(rest, "23");
    }

    // --- NEW TESTS for Stage 10 ---
    #[test]
    fn test_parse_numbers_advanced() {
        // Valid: Scientific notation (lowercase 'e')
        let (value, _) = parse_value("1.23e4").unwrap();
        assert_eq!(value, JsonValue::Number(12300.0));

        // Valid: Scientific notation (uppercase 'E')
        let (value, _) = parse_value("1.23E4").unwrap();
        assert_eq!(value, JsonValue::Number(12300.0));

        // Valid: Negative exponent
        let (value, _) = parse_value("-5.0E-2").unwrap();
        assert_eq!(value, JsonValue::Number(-0.05));

        // Valid: Exponent with positive sign
        let (value, _) = parse_value("1e+3").unwrap();
        assert_eq!(value, JsonValue::Number(1000.0));

        // Valid: Integer with exponent
        let (value, _) = parse_value("1e3").unwrap();
        assert_eq!(value, JsonValue::Number(1000.0));

        // Invalid: Incomplete exponent
        assert!(parse_value("1e").is_err());
        assert!(parse_value("1e-").is_err());
        assert!(parse_value("1e+").is_err());

        // Invalid: Invalid format
        assert!(parse_value("1.e1").is_err());
    }
}
