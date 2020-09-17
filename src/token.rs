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
    Not,
    LogicalNot,
    Hyphen,
    Plus,
    Asterisk,
    Slash,
    Percentage,
    Equal,
    Unequal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Assign,
    And, // Neither bitwise-and nor bitwise-or operation exists,
    Or,  // but we reserve them here for convenience
    LogicalAnd,
    LogicalOr,
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
