#[derive(Debug)]
pub enum TokenKind<'a> {
    // Special
    Integer(&'a [u8]),
    Identifier(&'a [u8]),
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
    pub kind: TokenKind<'a>,
    pub row: u32,
    pub col: u32,
}
