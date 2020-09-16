use crate::token::Token;

// Ok(Token), Err((remaining u8 slice, error message))
pub type LexicalResult<'a, 'b> = Result<Token<'a>, (&'a [u8], &'b str)>;
