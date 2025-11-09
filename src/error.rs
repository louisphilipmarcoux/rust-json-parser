//! Contains the primary `ParseError` type for the library.
use std::fmt;

/// The primary error type for all parsing operations.
///
/// This struct contains a human-readable error message and the
/// location (line and column) where the error occurred.
#[derive(Debug, PartialEq)]
pub struct ParseError {
    /// A description of what went wrong.
    pub message: String,
    /// The line number (1-indexed) where the error was detected.
    pub line: usize,
    /// The column number (1-indexed) where the error was detected.
    pub column: usize,
}

// --- Error Formatting ---
// This provides a user-friendly, human-readable error message.
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Error: {} at line {}, column {}.",
            self.message, self.line, self.column
        )
    }
}

// --- Unit Tests ---
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let error = ParseError {
            message: "Unexpected ']'".to_string(),
            line: 10,
            column: 5,
        };
        assert_eq!(
            error.to_string(),
            "Error: Unexpected ']' at line 10, column 5."
        );
    }
}
