// In your new src/main.rs

// Use the library crate. The name "json_parser" comes from
// your Cargo.toml [package] name.
use json_parser::{parse_streaming, JsonValue}; 
use std::collections::HashMap;

fn main() {
    let input = "{ \"key\": [1, true, null] }";
    println!("--- Running Streaming Parser ---");

    // Call the library function
    match parse_streaming(input) {
        Ok(mut parser) => {
            while let Some(event) = parser.next() {
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
    let mut items = HashMap::new();
    items.insert("key".to_string(), JsonValue::String("value".to_string()));
    items.insert(
        "items".to_string(),
        JsonValue::Array(vec![JsonValue::Number(1.0), JsonValue::Null]),
    );
    let obj = JsonValue::Object(items);

    // Call library functions
    println!("Compact: {}", obj.stringify());
    println!("Pretty:\n{}", obj.stringify_pretty());
}