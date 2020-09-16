use crate::token::Token;

// Ok(remaining u8 slice, Token, row, col), Err(remaining u8 slice, error message)
pub type LexicalResult<'a, 'b> = Result<(&'a [u8], Token<'a>, u32, u32), (&'a [u8], &'b str)>;
