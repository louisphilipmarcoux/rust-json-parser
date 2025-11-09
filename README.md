# **rill-json**

[![Crates.io](https://img.shields.io/crates/v/rill-json.svg)](https://crates.io/crates/rill-json)
[![Docs.rs](https://docs.rs/rill-json/badge.svg)](https://docs.rs/rill-json)
[![CI](https://github.com/louisphilipmarcoux/rill-json/actions/workflows/rust.yml/badge.svg)](https://github.com/louisphilipmarcoux/rill-json/actions)
[![License: MIT OR Apache-2.0](https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue.svg)](https://github.com/louisphilipmarcoux/rill-json/blob/main/LICENSE-MIT)
[![Rust 100% Safe](https://img.shields.io/badge/unsafe-forbidden-success.svg)](https://github.com/louisphilipmarcoux/rill-json/blob/main/src/lib.rs)

A fast, 100% safe, and RFC 8259-compliant streaming JSON parser and serializer, built from scratch in Rust.

rill-json is designed for performance, correctness and safety. It provides a low-memory, event-based in-memory `JsonValue` enum, and a serializer to convert your Rust data back into JSON strings.

## **Key Features**

* **100% Safe Rust:** Contains '#![forbid(unsafe_code)]' to guarantee no unsafe keyword is used.  
* **Streaming Parser:** An 'Iterator' that emits 'ParserEvent's, ideal for parsing large files with minimal memory.  
* **Optimized Performance:** Uses a byte-slice-based tokenizer with a branchless Lookup Table (LUT) and 'memchr' (for "safe SIMD") to achieve high performance.
* **Zero-Allocation String Parsing:** Returns borrowed string slices ('&str') when no JSON escapes are present, avoiding allocations.
* **In-Memory DOM:** Provides a 'JsonValue' enum for convenience, with a 'JsonValue::parse()' function to build an in-memory tree.
* **Serializer Included:** Comes with 'stringify()' and 'stringify\_pretty()' to serialize your Rust data. 'JsonValue' uses 'BTreeMap' for objects to guarantee deterministic key order.
* **RFC 8259 Compliant:** Built to pass the official JSON specification tests.

## **Quick Start**

Add rill-json to your 'Cargo.toml':

    [dependencies]  
    rill-json = "0.1.0" # Check crates.io for the latest version

### **1\. Parsing JSON (Streaming)**

The 'parse\_streaming' function is the primary entry point. It returns an iterator that you can loop over. This is the most memory-efficient way to parse JSON.

    use rill_json::{parse_streaming, ParserEvent};

    fn main() {  
        let json_data = r#"  
            {  
                "id": 123,  
                "name": "Babbage",  
                "active": true  
            }  
        "#;

        let mut parser = parse_streaming(json_data).unwrap();  
        let mut found_name_key = false;

        // Loop over all events  
        while let Some(event) = parser.next() {  
            match event.unwrap() {  
                // We found a key...  
                ParserEvent::Key(key) if key == "name" => {  
                    found_name_key = true;  
                }  
                // ...so the *next* string event is the value we want.  
                ParserEvent::String(value) if found_name_key => {  
                    println!("Found name: {}", value);  
                    break; // Stop parsing  
                }  
                // Reset if we see other values before finding the one we want  
                _ => {  
                    found_name_key = false;  
                }  
            }  
        }  
    }

### **2. Parsing JSON (In-Memory)**

For convenience, you can also parse directly into the 'JsonValue' enum.

    use rill_json::{JsonValue, JsonNumber};
    use std::collections::BTreeMap;

    fn main() {
        let json_data = r#"{ "id": 1815, "active": true }"#;
        let parsed = JsonValue::parse(json_data).unwrap();

        let mut expected_map = BTreeMap::new();
        expected_map.insert("id".to_string(), JsonValue::Number(JsonNumber::I64(1815)));
        expected_map.insert("active".to_string(), JsonValue::Boolean(true));
        let expected_val = JsonValue::Object(expected_map);

        assert_eq!(parsed, expected_val);
        println!("Parsed value: {:?}", parsed);
    }

### **3\. Serializing Data (Stringify)**

You can also use rill-json to create JSON strings from your own Rust data using the 'JsonValue' enum.

    use rill_json::{JsonValue, JsonNumber};
    use std::collections::BTreeMap; // Use BTreeMap to match the lib's implementation

    fn main() {
        // 1. Create native Rust data
        // We use BTreeMap to ensure keys are sorted alphabetically,
        // which is what rill-json does internally for deterministic output.
        let mut user_data = BTreeMap::new();
        user_data.insert(
            "username".to_string(),
            JsonValue::String("ada_l".to_string()),
        );
        user_data.insert(
            "id".to_string(),
            JsonValue::Number(JsonNumber::I64(1815)),
        );
        user_data.insert(
            "projects".to_string(),
            JsonValue::Array(vec![
                JsonValue::String("Analytical Engine".to_string()),
                JsonValue::String("Difference Engine".to_string()),
            ]),
        );
        user_data.insert(
            "active".to_string(),
            JsonValue::Boolean(false),
        );

        // 2. Wrap it in the JsonValue::Object variant
        let json_object = JsonValue::Object(user_data);

        // 3. Stringify it!

        // Compact version (machine-readable)
        let compact_string = json_object.stringify().unwrap();
        println!("--- Compact ---\n{}", compact_string);
        // Output: {"active":false,"id":1815,"projects":["Analytical Engine","Difference Engine"],"username":"ada_l"}

        // Pretty version (human-readable)
        let pretty_string = json_object.stringify_pretty().unwrap();
        println!("\n--- Pretty ---\n{}", pretty_string);
    }

### **License**

This project is dual-licensed under the terms of both the [MIT License](https://www.google.com/search?q=./LICENSE-MIT) and the [Apache License 2.0](https://www.google.com/search?q=./LICENSE-APACHE).
