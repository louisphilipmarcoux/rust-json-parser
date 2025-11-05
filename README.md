# **rill-json**

A fast, 100% safe, and RFC 8259-compliant streaming JSON parser and serializer, built from scratch in Rust.

rill-json is designed for performance and safety. It provides a low-memory, event-based streaming parser and a convenient serializer to convert Rust's native HashMaps and Vecs back into JSON strings.

## **Key Features**

* **100% Safe Rust:** Contains no unsafe code.  
* **Streaming Parser:** An Iterator that emits ParserEvents, ideal for parsing large files with minimal memory.  
* **Optimized Performance:** Uses a byte-slice-based tokenizer with a branchless Lookup Table (LUT) and memchr (for "safe SIMD") to achieve high performance.  
* **Serializer Included:** Comes with stringify() and stringify\_pretty() to serialize your data back to JSON.  
* **RFC 8259 Compliant:** Built to pass the official JSON specification tests.

## **Quick Start**

Add rill-json to your Cargo.toml:

    [dependencies]  
    rill-json = "0.1.0" # Check crates.io for the latest version

### **1\. Parsing JSON (Streaming)**

The parse\_streaming function is the primary entry point. It returns an iterator that you can loop over. This is the most memory-efficient way to parse JSON.

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

### **2\. Serializing Data (Stringify)**

You can also use rill-json to create JSON strings from your own Rust data using the JsonValue enum.

    use rill_json::JsonValue;  
    use std::collections::HashMap;

    fn main() {  
        // 1. Create native Rust data  
        let mut user_data = HashMap::new();  
        user_data.insert(  
            "username".to_string(),  
            JsonValue::String("ada_l".to_string())  
        );  
        user_data.insert(  
            "id".to_string(),  
            JsonValue::Number(1815.0)  
        );  
        user_data.insert(  
            "projects".to_string(),  
            JsonValue::Array(vec![  
                JsonValue::String("Analytical Engine".to_string()),  
                JsonValue::String("Difference Engine".to_string())  
            ])  
        );  
        user_data.insert(  
            "active".to_string(),  
            JsonValue::Boolean(false)  
        );

        // 2. Wrap it in the JsonValue::Object variant  
        let json_object = JsonValue::Object(user_data);

        // 3. Stringify it!  
        
        // Compact version (machine-readable)  
        let compact_string = json_object.stringify();  
        println!("--- Compact ---\n{}", compact_string);  
        
        // Pretty version (human-readable)  
        let pretty_string \= json_object.stringify_pretty();  
        println!("\n--- Pretty ---\n{}", pretty_string);  
    }

### **License**

This project is dual-licensed under the terms of both the [MIT License](https://www.google.com/search?q=./LICENSE-MIT) and the [Apache License 2.0](https://www.google.com/search?q=./LICENSE-APACHE).
