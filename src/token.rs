#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // Special
    Integer,
    Identifier,
    Eof,
    // Symbols
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Semicolon,
    // Keywords
    Int,
    Return,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub slice: &'a [u8], // token from the source code (as byte slice)
    pub row: usize,      // beginning position of this token
    pub col: usize,      // ending position of this token
}
