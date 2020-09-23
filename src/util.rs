#[derive(PartialEq, Clone)]
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
    LeftBracket,
    RightBracket,
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

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String, // token from the source code (as string)
}

#[derive(Clone, PartialEq)]
pub struct Type {
    level: u32,
    bounds: Vec<u32>,
}

impl Type {
    pub fn make_primitive() -> Type {
        Type {
            level: 0,
            bounds: Vec::new(),
        }
    }

    pub fn measure(&self) -> u32 {
        self.bounds.iter().fold(4, |acc, cur| acc * cur)
    }

    pub fn is_primitive(&self) -> bool {
        self.level == 0 && self.bounds.is_empty()
    }

    pub fn is_pointer(&self) -> bool {
        self.level > 0 && self.bounds.is_empty()
    }

    pub fn is_array(&self) -> bool {
        !self.bounds.is_empty()
    }

    pub fn unwrap_pointer(&mut self) -> &mut Type {
        assert!(self.is_pointer());
        self.level -= 1;
        self
    }

    pub fn unwrap_array(&mut self) -> &mut Type {
        assert!(self.is_array());
        self.bounds.pop();
        self
    }

    pub fn wrap_pointer(&mut self) -> &mut Type {
        assert!(!self.is_array());
        self.level += 1;
        self
    }

    pub fn wrap_array(&mut self, bound: u32) -> &mut Type {
        self.bounds.push(bound);
        self
    }
}

#[derive(Clone)]
pub struct Variable {
    pub ty: Type,
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
