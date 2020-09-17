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

    pub fn fetch_token(&mut self) -> LexicalResult<'a> {
        let ans = if !self.ungot_tokens.is_empty() {
            let token = self.ungot_tokens.pop().unwrap();
            Ok(token)
        } else {
            self.get_token()
        };
        // eprintln!("{:?}", ans);
        ans
    }

    fn get_token(&mut self) -> LexicalResult<'a> {
        macro_rules! make_single_symbol_match_arm {
            ($target: ident) => {{
                self.col += 1;
                let (token, remaining) = self.source.split_at(1);
                self.source = remaining;
                Ok(Token {
                    kind: TokenKind::$target,
                    slice: token,
                    row: self.row,
                    col: self.col,
                })
            }};
        }

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
                        let (token, remaining) = self.source.split_at(offset);
                        self.source = remaining;
                        break Ok(Token {
                            kind: TokenKind::Integer,
                            slice: token,
                            row: self.row,
                            col: self.col,
                        });
                    }
                    match self.source[offset] {
                        b'0'..=b'9' => offset += 1,
                        _ => {
                            self.col += offset;
                            let (token, remaining) = self.source.split_at(offset);
                            self.source = remaining;
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
                        let (token, remaining) = self.source.split_at(offset);
                        self.source = remaining;
                        break Ok(Token {
                            kind: Self::recognize_keyword(self.source),
                            slice: token,
                            row: self.row,
                            col: self.col,
                        });
                    }
                    match self.source[offset] {
                        b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' => {
                            offset += 1;
                        }
                        _ => {
                            self.col += offset;
                            let (token, remaining) = self.source.split_at(offset);
                            self.source = remaining;
                            break Ok(Token {
                                kind: Self::recognize_keyword(token),
                                slice: token,
                                row: self.row,
                                col: self.col,
                            });
                        }
                    }
                },
                b';' => make_single_symbol_match_arm!(Semicolon),
                b'(' => make_single_symbol_match_arm!(LeftParenthesis),
                b')' => make_single_symbol_match_arm!(RightParenthesis),
                b'{' => make_single_symbol_match_arm!(LeftBrace),
                b'}' => make_single_symbol_match_arm!(RightBrace),
                b'~' => make_single_symbol_match_arm!(Not),
                b'!' => make_single_symbol_match_arm!(LogicalNot),
                b'-' => make_single_symbol_match_arm!(Hyphen),
                b'+' => make_single_symbol_match_arm!(Plus),
                b'*' => make_single_symbol_match_arm!(Asterisk),
                b'/' => make_single_symbol_match_arm!(Slash),
                b'%' => make_single_symbol_match_arm!(Percentage),
                _ => Err(format!(
                    "Line {}, column {}: Unknown symbol '{}'",
                    self.row, self.col, self.source[0]
                )
                .to_string()),
            }
        }
    }
}
