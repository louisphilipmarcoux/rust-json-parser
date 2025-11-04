#[derive(Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Boolean(bool),
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

fn parse_value(input: &str) -> Result<(JsonValue, &str), &'static str> {
    match input.chars().next() {
        Some('n') => parse_null(input),
        Some('t') | Some('f') => parse_boolean(input),
        _ => Err("Unexpected 'null', 'true', or 'false'"),
    }
}

fn main() {
    println!("--- Stage 1 Tests ---");
    run_test("null", true);
    run_test("nul", false);
    run_test("null, \"more stuff\"", true);

    println!("\n--- Stage 2 Tests ---");
    run_test("true", true);
    run_test("false", true);
    run_test("True", false); // Invalid case
    run_test("fals", false); // Invalid partial
    run_test("false true", true); // Valid, with remaining text
    run_test("\"true\"", false); // Invalid, is a string
}

fn run_test(input: &str, should_pass: bool) {
    println!("Parsing: '{}'", input);
    
    // We now call our main 'parse_value' function
    match parse_value(input) {
        Ok((value, rest)) => {
            if should_pass {
                println!("   -> Parsed: {:?}", value);
                println!("   -> Remaining: '{}'", rest);
            } else {
                // This shouldn't happen if should_pass is false
                println!("   -> !!ERROR!!: Unexpectedly parsed {:?} from '{}'", value, input);
            }
        }
        Err(e) => {
            if should_pass {
                // This shouldn't happen if should_pass is true
                println!("   -> !!ERROR!!: Unexpectedly failed with '{}' for '{}'", e, input);
            } else {
                println!("   -> Correctly failed with: {}", e);
            }
        }
    }
}
