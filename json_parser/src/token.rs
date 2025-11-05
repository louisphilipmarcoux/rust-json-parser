// src/token.rs

// --- 2. Token Structs ---
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    Comma,        // ,
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub(crate) kind: TokenType,
    pub(crate) line: usize,
    pub(crate) column: usize,
}
