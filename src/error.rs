use crate::token::Token;

// Ok(Token), Err(error message)
pub type LexicalResult<'a> = Result<Token<'a>, String>;

// pub type SyntaxResult<'a> = Result<(), &'a str>;
