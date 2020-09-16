use crate::error::*;
use crate::token::*;

pub fn get_token<'a, 'b>(input: &'a [u8], mut row: u32, mut col: u32) -> LexicalResult<'a, 'b> {
    let mut offset = 0usize;
    while offset < input.len() {
        match input[offset] {
            b' ' | b'\r' | b'\t' => {
                offset += 1;
                col += 1;
            }
            b'\n' => {
                offset += 1;
                row += 1;
                col = 0;
            }
            _ => break,
        };
    }
    let start = offset;
    let input = &input[start..];
    offset = 0;
    if input.len() == 0 {
        LexicalResult::Ok((
            input,
            Token {
                kind: TokenKind::Eof,
                row,
                col,
            },
            row,
            col,
        ))
    } else {
        match input[0] {
            b'0'..=b'9' => loop {
                match input[offset] {
                    b'0'..=b'9' => offset += 1,
                    _ => {
                        let (token, remaining) = input.split_at(offset);
                        break LexicalResult::Ok((
                            remaining,
                            Token {
                                kind: TokenKind::Integer(token),
                                row,
                                col,
                            },
                            row,
                            col + offset as u32,
                        ));
                    }
                }
            },
            b'A'..=b'Z' | b'a'..=b'z' | b'_' => loop {
                match input[offset] {
                    b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' => {
                        offset += 1;
                    }
                    _ => {
                        let (token, remaining) = input.split_at(offset);
                        break LexicalResult::Ok((
                            remaining,
                            Token {
                                kind: TokenKind::Identifier(token),
                                row,
                                col,
                            },
                            row,
                            col + offset as u32,
                        ));
                    }
                }
            },
            b';' => LexicalResult::Ok((
                &input[1..],
                Token {
                    kind: TokenKind::Semicolon,
                    row,
                    col,
                },
                row,
                col + 1,
            )),
            b'(' => LexicalResult::Ok((
                &input[1..],
                Token {
                    kind: TokenKind::LeftParenthesis,
                    row,
                    col,
                },
                row,
                col + 1,
            )),
            b')' => LexicalResult::Ok((
                &input[1..],
                Token {
                    kind: TokenKind::RightParenthesis,
                    row,
                    col,
                },
                row,
                col + 1,
            )),
            b'{' => LexicalResult::Ok((
                &input[1..],
                Token {
                    kind: TokenKind::LeftBrace,
                    row,
                    col,
                },
                row,
                col + 1,
            )),
            b'}' => LexicalResult::Ok((
                &input[1..],
                Token {
                    kind: TokenKind::RightBrace,
                    row,
                    col,
                },
                row,
                col + 1,
            )),
            _ => LexicalResult::Err((input, "Unknown symbol")),
        }
    }
}
