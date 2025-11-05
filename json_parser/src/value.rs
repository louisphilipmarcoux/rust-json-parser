// src/value.rs
use std::collections::HashMap;
use std::fmt;

// --- 5. JSON Value Enum (for Stage 16) ---
#[derive(Debug, PartialEq, Clone)]
pub enum JsonValue {
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

// --- 7. Stringify (Serialization - Stage 16) ---
impl JsonValue {
    /// Serializes the JsonValue into a compact JSON string,
    /// as required by Stage 16.
    pub fn stringify(&self) -> String {
        let mut output = String::new();
        // This unwrap is safe because writing to a String never fails.
        Self::write_value(self, &mut output).unwrap();
        output
    }

    /// Recursive helper function to write a value.
    /// Uses fmt::Write for efficient string building.
    fn write_value<W: fmt::Write>(value: &JsonValue, w: &mut W) -> fmt::Result {
        match value {
            JsonValue::Null => w.write_str("null"),
            JsonValue::Boolean(b) => w.write_str(if *b { "true" } else { "false" }),
            JsonValue::Number(n) => write!(w, "{}", n),
            JsonValue::String(s) => Self::write_string(s, w),
            JsonValue::Array(a) => Self::write_array(a, w),
            JsonValue::Object(o) => Self::write_object(o, w),
        }
    }

    /// Helper to write a JSON array.
    fn write_array<W: fmt::Write>(arr: &Vec<JsonValue>, w: &mut W) -> fmt::Result {
        w.write_char('[')?;
        let mut first = true;
        for val in arr {
            if !first {
                w.write_char(',')?;
            }
            Self::write_value(val, w)?;
            first = false;
        }
        w.write_char(']')
    }

    /// Helper to write a JSON object.
    fn write_object<W: fmt::Write>(obj: &HashMap<String, JsonValue>, w: &mut W) -> fmt::Result {
        w.write_char('{')?;
        let mut first = true;
        for (key, val) in obj {
            if !first {
                w.write_char(',')?;
            }
            Self::write_string(key, w)?; // Write the key (which must be a string)
            w.write_char(':')?;
            Self::write_value(val, w)?; // Write the value
            first = false;
        }
        w.write_char('}')
    }

    /// Helper to write an escaped JSON string.
    /// This handles escapes required by Stage 8 and Stage 16.
    fn write_string<W: fmt::Write>(s: &str, w: &mut W) -> fmt::Result {
        w.write_char('"')?;
        for c in s.chars() {
            match c {
                // Standard escapes
                '"' => w.write_str("\\\""),
                '\\' => w.write_str("\\\\"),
                '/' => w.write_str("\\/"), // Optional, but good practice
                '\u{0008}' => w.write_str("\\b"),
                '\u{000C}' => w.write_str("\\f"),
                '\n' => w.write_str("\\n"),
                '\r' => w.write_str("\\r"),
                '\t' => w.write_str("\\t"),
                // Control characters must be escaped
                '\u{0000}'..='\u{001F}' => {
                    write!(w, "\\u{:04x}", c as u32)
                }
                _ => w.write_char(c),
            }?;
        }
        w.write_char('"')
    }

    // --- Pretty Print Bonus ---

    /// The indentation string to use for pretty-printing.
    const INDENT: &'static str = "  ";

    /// Serializes the JsonValue into a human-readable,
    /// indented JSON string.
    pub fn stringify_pretty(&self) -> String {
        let mut output = String::new();
        // This unwrap is safe because writing to a String never fails.
        Self::write_value_pretty(self, &mut output, 0).unwrap();
        output
    }

    /// Recursive helper for pretty-printing a value.
    fn write_value_pretty<W: fmt::Write>(
        value: &JsonValue,
        w: &mut W,
        depth: usize,
    ) -> fmt::Result {
        match value {
            // Primitives are written the same as compact
            JsonValue::Null => w.write_str("null"),
            JsonValue::Boolean(b) => w.write_str(if *b { "true" } else { "false" }),
            JsonValue::Number(n) => write!(w, "{}", n),
            JsonValue::String(s) => Self::write_string(s, w),
            // Composites get new logic
            JsonValue::Array(a) => Self::write_array_pretty(a, w, depth),
            JsonValue::Object(o) => Self::write_object_pretty(o, w, depth),
        }
    }

    /// Helper to pretty-print a JSON array.
    fn write_array_pretty<W: fmt::Write>(
        arr: &Vec<JsonValue>,
        w: &mut W,
        depth: usize,
    ) -> fmt::Result {
        // Empty array is just "[]"
        if arr.is_empty() {
            return w.write_str("[]");
        }

        let new_depth = depth + 1;
        let indent = Self::INDENT.repeat(new_depth);
        let closing_indent = Self::INDENT.repeat(depth);

        w.write_str("[\n")?; // Opening bracket and newline

        let mut first = true;
        for val in arr {
            if !first {
                w.write_str(",\n")?; // Comma and newline before next item
            }
            w.write_str(&indent)?; // Indent
            Self::write_value_pretty(val, w, new_depth)?; // Write the value
            first = false;
        }

        write!(w, "\n{}", closing_indent)?; // Newline and closing indent
        w.write_char(']') // Closing bracket
    }

    /// Helper to pretty-print a JSON object.
    fn write_object_pretty<W: fmt::Write>(
        obj: &HashMap<String, JsonValue>,
        w: &mut W,
        depth: usize,
    ) -> fmt::Result {
        // Empty object is just "{}"
        if obj.is_empty() {
            return w.write_str("{}");
        }

        let new_depth = depth + 1;
        let indent = Self::INDENT.repeat(new_depth);
        let closing_indent = Self::INDENT.repeat(depth);

        w.write_str("{\n")?; // Opening brace and newline

        let mut first = true;
        for (key, val) in obj {
            if !first {
                w.write_str(",\n")?; // Comma and newline before next item
            }
            w.write_str(&indent)?; // Indent
            Self::write_string(key, w)?; // Write the key
            w.write_str(": ")?; // Colon and space
            Self::write_value_pretty(val, w, new_depth)?; // Write the value
            first = false;
        }

        write!(w, "\n{}", closing_indent)?; // Newline and closing indent
        w.write_char('}') // Closing brace
    }
}
