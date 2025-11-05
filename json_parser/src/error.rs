// src/error.rs
use std::fmt;

// --- 3. Error Type ---
#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error: {} at line {}, column {}.",
            self.message, self.line, self.column
        )
    }
}
