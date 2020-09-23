#[derive(PartialEq, Clone)]
pub enum Token {
    // Special
    Integer(i32),
    Identifier(String),
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

impl Token {
    pub fn unwrap_integer(&self) -> i32 {
        if let Token::Integer(value) = self {
            *value
        } else {
            panic!();
        }
    }

    pub fn unwrap_identifier(&self) -> String {
        if let Token::Identifier(text) = self {
            text.clone()
        } else {
            panic!();
        }
    }
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

    pub fn unwrap_pointer(&self) -> Type {
        assert!(self.is_pointer());
        Type {
            level: self.level - 1,
            bounds: self.bounds.clone(),
        }
    }

    pub fn unwrap_array(&self) -> Type {
        assert!(self.is_array());
        Type {
            bounds: self.bounds[..self.bounds.len() - 1].to_vec(),
            ..*self
        }
    }

    pub fn wrap_pointer(&self) -> Type {
        assert!(!self.is_array());
        Type {
            level: self.level + 1,
            bounds: self.bounds.clone(),
        }
    }

    pub fn wrap_array(&self, bound: u32) -> Type {
        Type {
            bounds: [self.bounds.clone(), vec![bound]].join(&[][..]),
            ..*self
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Value {
    Left,
    Right,
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
