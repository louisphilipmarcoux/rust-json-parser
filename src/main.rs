//! A binary executable that demonstrates how to use the `rill-json` library.
//!
//! This is not part of the library itself, but provides a simple
//! example of both parsing and stringifying JSON.
//!
//! You can run this example with: `cargo run`

use rill_json::{parse_streaming, JsonNumber, JsonValue};
use std::collections::BTreeMap;

fn main() {
    let input = "{ \"key\": [1, true, null] }";
    println!("--- Running Streaming Parser ---");
    println!("Parsing: {}", input);

    // Call the library function to get a parser iterator
    match parse_streaming(input) {
        Ok(parser) => {
            // Iterate over all parser events
            for event in parser {
                match event {
                    Ok(event) => println!("Event: {:?}", event),
                    Err(e) => {
                        println!("{}", e);
                        break;
                    }
                }
            }
        }
        Err(e) => println!("{}", e),
    }

    println!("\n--- Running Stringify Demo ---");

    // 1. Build a native Rust data structure
    let mut items = BTreeMap::new();
    items.insert("key".to_string(), JsonValue::String("value".to_string()));
    items.insert(
        "items".to_string(),
        JsonValue::Array(vec![JsonValue::Number(JsonNumber::I64(1)), JsonValue::Null]),
    );
    let obj = JsonValue::Object(items);
    println!("Serializing: {:?}", obj);

    // 2. Call the library functions to serialize it
    match obj.stringify() {
        Ok(compact) => println!("Compact: {}", compact),
        Err(e) => println!("Compact Error: {}", e),
    }
    match obj.stringify_pretty() {
        Ok(pretty) => println!("Pretty:\n{}", pretty),
        Err(e) => println!("Pretty Error: {}", e),
    }

    println!("\n--- Running JsonValue::parse Demo ---");
    let input_to_parse = r#"
    {
        "user_id": 9007199254740993,
        "username": "big_int_user",
        "active": true,
        "nested": { "values": [1.5, null] }
    }
    "#;
    println!("Parsing input string:\n{}", input_to_parse);
    match JsonValue::parse(input_to_parse) {
        Ok(parsed_value) => {
            println!("Parsed Value: {:?}", parsed_value);
            // Now stringify it pretty
            println!("--- Pretty Output ---");
            println!("{}", parsed_value.stringify_pretty().unwrap());
        }
        Err(e) => println!("Parse Error: {}", e),
    }
}
