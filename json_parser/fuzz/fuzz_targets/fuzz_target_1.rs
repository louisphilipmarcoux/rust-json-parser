#![no_main]
use libfuzzer_sys::fuzz_target;

// Import your library's parsing function
use json_parser::parse_streaming; 

fuzz_target!(|data: &[u8]| {
    // The fuzzer gives us raw bytes.
    // We only care about inputs that are valid UTF-8.
    if let Ok(s) = std::str::from_utf8(data) {

        // Now, try to parse the string.
        // We are looking for panics, so we just
        // consume the iterator.
        if let Ok(parser) = parse_streaming(s) {
            // By calling .count(), we force the streaming
            // parser to run through the entire input.
            // If any part of the parser logic panics,
            // the fuzzer will catch it.
            let _ = parser.count();
        }
    }
});