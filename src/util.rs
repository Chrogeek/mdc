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
pub enum Type {
    Primitive,
    Pointer(Box<Type>),
    Array(Box<Type>, u32),
}

impl Type {
    pub fn measure(&self) -> u32 {
        match self {
            Type::Primitive => 4,
            Type::Pointer(_) => 4,
            Type::Array(ty, size) => ty.measure() * size,
        }
    }

    pub fn is_primitive(&self) -> bool {
        if let Type::Primitive = self {
            true
        } else {
            false
        }
    }

    pub fn is_pointer(&self) -> bool {
        if let Type::Pointer(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_array(&self) -> bool {
        if let Type::Array(_, _) = self {
            true
        } else {
            false
        }
    }

    pub fn unwrap_pointer(self) -> Type {
        if let Type::Pointer(ty) = self {
            *ty
        } else {
            panic!();
        }
    }

    pub fn unwrap_array(self) -> Type {
        if let Type::Array(ty, _) = self {
            *ty
        } else {
            panic!();
        }
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
