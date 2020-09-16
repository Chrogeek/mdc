use crate::error::*;
use crate::token::*;

pub struct Lexer<'a> {
    source: &'a [u8],
    pub row: usize,
    pub col: usize,
    ungot_tokens: Vec<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Lexer {
        Lexer {
            source,
            row: 0,
            col: 0,
            ungot_tokens: Vec::new(),
        }
    }

    fn recognize_keyword<'b>(input: &'b [u8]) -> TokenKind {
        match input {
            b"int" => TokenKind::Int,
            b"return" => TokenKind::Return,
            _ => TokenKind::Identifier,
        }
    }

    pub fn unget_token(&mut self, token: Token<'a>) {
        self.ungot_tokens.push(token);
    }

    pub fn fetch_token<'b>(&mut self) -> LexicalResult<'a, 'b> {
        let ans = if !self.ungot_tokens.is_empty() {
            let token = self.ungot_tokens.pop().unwrap();
            Ok(token)
        } else {
            self.get_token()
        };
        eprintln!("{:?}", ans);
        ans
    }

    fn get_token<'b>(&mut self) -> LexicalResult<'a, 'b> {
        let mut offset = 0;
        while offset < self.source.len() {
            match self.source[offset] {
                b' ' | b'\r' | b'\t' => {
                    offset += 1;
                    self.col += 1;
                }
                b'\n' => {
                    offset += 1;
                    self.row += 1;
                    self.col = 0;
                }
                _ => break,
            };
        }
        self.source = &self.source[offset..];
        let mut offset = 0;
        if self.source.len() == 0 {
            // EOF
            Ok(Token {
                kind: TokenKind::Eof,
                slice: b"",
                row: self.row,
                col: self.col,
            })
        } else {
            match self.source[0] {
                b'0'..=b'9' => loop {
                    // Integer literals
                    if offset >= self.source.len() {
                        self.col += offset;
                        self.source = &self.source[offset..];
                        break Ok(Token {
                            kind: TokenKind::Integer,
                            slice: self.source,
                            row: self.row,
                            col: self.col,
                        });
                    }
                    match self.source[offset] {
                        b'0'..=b'9' => offset += 1,
                        _ => {
                            let token = &self.source[..offset];
                            self.col += offset;
                            self.source = &self.source[offset..];
                            break Ok(Token {
                                kind: TokenKind::Integer,
                                slice: token,
                                row: self.row,
                                col: self.col,
                            });
                        }
                    }
                },
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => loop {
                    // Identifiers & keywords
                    if offset >= self.source.len() {
                        self.col += offset;
                        self.source = &self.source[offset..];
                        break Ok(Token {
                            kind: Self::recognize_keyword(self.source),
                            slice: self.source,
                            row: self.row,
                            col: self.col,
                        });
                    }
                    match self.source[offset] {
                        b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' => {
                            offset += 1;
                        }
                        _ => {
                            let token = &self.source[..offset];
                            self.col += offset;
                            self.source = &self.source[offset..];
                            break Ok(Token {
                                kind: Self::recognize_keyword(token),
                                slice: token,
                                row: self.row,
                                col: self.col,
                            });
                        }
                    }
                },
                b';' => {
                    self.col += 1;
                    self.source = &self.source[1..];
                    Ok(Token {
                        kind: TokenKind::Semicolon,
                        slice: &self.source[0..1],
                        row: self.row,
                        col: self.col,
                    })
                }
                b'(' => {
                    self.col += 1;
                    self.source = &self.source[1..];
                    Ok(Token {
                        kind: TokenKind::LeftParenthesis,
                        slice: &self.source[0..1],
                        row: self.row,
                        col: self.col,
                    })
                }
                b')' => {
                    self.col += 1;
                    self.source = &self.source[1..];
                    Ok(Token {
                        kind: TokenKind::RightParenthesis,
                        slice: &self.source[0..1],
                        row: self.row,
                        col: self.col,
                    })
                }
                b'{' => {
                    self.col += 1;
                    self.source = &self.source[1..];
                    Ok(Token {
                        kind: TokenKind::LeftBrace,
                        slice: &self.source[0..1],
                        row: self.row,
                        col: self.col,
                    })
                }
                b'}' => {
                    self.col += 1;
                    self.source = &self.source[1..];
                    Ok(Token {
                        kind: TokenKind::RightBrace,
                        slice: &self.source[0..1],
                        row: self.row,
                        col: self.col,
                    })
                }
                _ => Err((self.source, "Unknown symbol")),
            }
        }
    }
}
