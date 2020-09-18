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

#[derive(Debug, Clone)]
pub struct Type {
    pub level: usize, // Level of pointers (e.g. 3 for 'int ***')
}

impl Type {
    pub fn measure(&self) -> usize {
        self.level * 4
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub r#type: Type,
    pub offset: usize,
}

// pub fn mangle_function_name(name: &String) -> String {
//     "__".to_string() + name
// }

// pub fn get_function_epilogue(name: &String) -> String {
//     "_epilogue_".to_string() + name
// }
