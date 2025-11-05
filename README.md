# **rill-json**

A fast, 100% safe, and RFC 8259-compliant streaming JSON parser and serializer, built from scratch in Rust.

rill-json is designed for performance and safety. It provides a low-memory, event-based streaming parser and a convenient serializer to convert Rust's native HashMaps and Vecs back into JSON strings.

### **Key Features**

* **100% Safe Rust:** Contains no unsafe code.  
* **Streaming Parser:** An Iterator that emits ParserEvents, ideal for parsing large files with minimal memory.  
* **Optimized Performance:** Uses a byte-slice-based tokenizer with a branchless Lookup Table (LUT) and memchr (for "safe SIMD") to achieve high performance.  
* **Serializer Included:** Comes with stringify() and stringify\_pretty() to serialize your data back to JSON.  
* **RFC 8259 Compliant:** Built to pass the official JSON specification tests.

## **Quick Start**

Add rill-json to your Cargo.toml:


    \[dependencies\]  
    rill-json \= "0.1.0" \# Check crates.io for the latest version

### **1\. Parsing JSON (Streaming)**

The parse\_streaming function is the primary entry point. It returns an iterator that you can loop over. This is the most memory-efficient way to parse JSON.

    use rill\_json::{parse\_streaming, ParserEvent};

    fn main() {  
        let json\_data \= r\#"  
            {  
                "id": 123,  
                "name": "Babbage",  
                "active": true  
            }  
        "\#;

        let mut parser \= parse\_streaming(json\_data).unwrap();  
        let mut found\_name\_key \= false;

        // Loop over all events  
        while let Some(event) \= parser.next() {  
            match event.unwrap() {  
                // We found a key...  
                ParserEvent::Key(key) if key \== "name" \=\> {  
                    found\_name\_key \= true;  
                }  
                // ...so the \*next\* string event is the value we want.  
                ParserEvent::String(value) if found\_name\_key \=\> {  
                    println\!("Found name: {}", value);  
                    break; // Stop parsing  
                }  
                // Reset if we see other values before finding the one we want  
                \_ \=\> {  
                    found\_name\_key \= false;  
                }  
            }  
        }  
    }

### **2\. Serializing Data (Stringify)**

You can also use rill-json to create JSON strings from your own Rust data using the JsonValue enum.

    use rill\_json::JsonValue;  
    use std::collections::HashMap;

    fn main() {  
        // 1\. Create native Rust data  
        let mut user\_data \= HashMap::new();  
        user\_data.insert(  
            "username".to\_string(),  
            JsonValue::String("ada\_l".to\_string())  
        );  
        user\_data.insert(  
            "id".to\_string(),  
            JsonValue::Number(1815.0)  
        );  
        user\_data.insert(  
            "projects".to\_string(),  
            JsonValue::Array(vec\!\[  
                JsonValue::String("Analytical Engine".to\_string()),  
                JsonValue::String("Difference Engine".to\_string())  
            \])  
        );  
        user\_data.insert(  
            "active".to\_string(),  
            JsonValue::Boolean(false)  
        );

        // 2\. Wrap it in the JsonValue::Object variant  
        let json\_object \= JsonValue::Object(user\_data);

        // 3\. Stringify it\!  
        
        // Compact version (machine-readable)  
        let compact\_string \= json\_object.stringify();  
        println\!("--- Compact \---\\n{}", compact\_string);  
        
        // Pretty version (human-readable)  
        let pretty\_string \= json\_object.stringify\_pretty();  
        println\!("\\n--- Pretty \---\\n{}", pretty\_string);  
    }

### **License**

This project is dual-licensed under the terms of both the [MIT License](https://www.google.com/search?q=./LICENSE-MIT) and the [Apache License 2.0](https://www.google.com/search?q=./LICENSE-APACHE).