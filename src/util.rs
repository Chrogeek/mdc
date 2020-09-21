#[derive(Debug, PartialEq, Clone)]
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
    Et,
    Or, // This token is not actually used
    LogicalAnd,
    LogicalOr,
    Question,
    Colon,
    Comma,
    // Keywords
    Int,
    Return,
    If,
    Else,
    For,
    Do,
    While,
    Break,
    Continue,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String, // token from the source code (as string)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub level: i32, // Level of pointers (e.g. 3 for 'int ***')
}

impl Type {
    pub fn new(level: i32) -> Type {
        Type { level }
    }

    pub fn make_value() -> Type {
        Type { level: 0 }
    }

    pub fn measure(&self) -> i32 {
        assert!(self.level >= 0);
        4
    }

    pub fn is_pointer(&self) -> bool {
        self.level > 0
    }

    pub fn is_value(&self) -> bool {
        self.level == 0
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub r#type: Type,
    pub offset: i32,
}

pub fn mangle_function_name(name: &String) -> String {
    "__".to_string() + name
}

pub fn get_function_epilogue(name: &String) -> String {
    "_e_".to_string() + name
}

pub fn mangle_global_variable(name: &String) -> String {
    "_g_".to_string() + name
}
