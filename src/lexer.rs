use crate::util::*;

pub struct Lexer<'a> {
    source: &'a [u8],
    ungot_tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [u8]) -> Lexer {
        Lexer {
            source,
            ungot_tokens: Vec::new(),
        }
    }

    fn recognize_keyword<'b>(input: &'b [u8]) -> Token {
        match input {
            b"int" => Token::Int,
            b"return" => Token::Return,
            b"if" => Token::If,
            b"else" => Token::Else,
            b"for" => Token::For,
            b"do" => Token::Do,
            b"while" => Token::While,
            b"break" => Token::Break,
            b"continue" => Token::Continue,
            _ => Token::Identifier(String::from_utf8_lossy(input).to_string()),
        }
    }

    pub fn unget_token(&mut self, token: Token) {
        self.ungot_tokens.push(token);
    }

    pub fn fetch_token(&mut self) -> Token {
        if !self.ungot_tokens.is_empty() {
            self.ungot_tokens.pop().unwrap()
        } else {
            self.get_token()
        }
    }

    fn get_token(&mut self) -> Token {
        macro_rules! make_single_symbol_match_arm {
            ($target: ident) => {{
                let (_, remaining) = self.source.split_at(1);
                self.source = remaining;
                Token::$target
            }};
        }

        macro_rules! make_double_symbol_match_arm {
            ($second: expr, $target1: ident, $target2: ident) => {{
                if self.source.len() < 2 || self.source[1] != $second {
                    let (_, remaining) = self.source.split_at(1);
                    self.source = remaining;
                    Token::$target1
                } else {
                    let (_, remaining) = self.source.split_at(2);
                    self.source = remaining;
                    Token::$target2
                }
            }};
        }

        let mut offset = 0;
        while offset < self.source.len() {
            match self.source[offset] {
                b' ' | b'\r' | b'\t' | b'\n' => {
                    offset += 1;
                }
                _ => break,
            };
        }
        self.source = &self.source[offset..];
        let mut offset = 0;
        if self.source.len() == 0 {
            Token::Eof
        } else {
            match self.source[0] {
                b'0'..=b'9' => {
                    loop {
                        // Integer literals
                        if offset >= self.source.len() {
                            break;
                        }
                        match self.source[offset] {
                            b'0'..=b'9' => offset += 1,
                            _ => break,
                        }
                    }
                    let (token, remaining) = self.source.split_at(offset);
                    self.source = remaining;
                    Token::Integer(String::from_utf8_lossy(token).parse::<i32>().unwrap())
                }
                b'A'..=b'Z' | b'a'..=b'z' | b'_' => {
                    loop {
                        // Identifiers & keywords
                        if offset >= self.source.len() {
                            break;
                        }
                        match self.source[offset] {
                            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_' => offset += 1,
                            _ => break,
                        }
                    }
                    let (token, remaining) = self.source.split_at(offset);
                    self.source = remaining;
                    Lexer::recognize_keyword(token)
                }
                b';' => make_single_symbol_match_arm!(Semicolon),
                b'(' => make_single_symbol_match_arm!(LeftParenthesis),
                b')' => make_single_symbol_match_arm!(RightParenthesis),
                b'{' => make_single_symbol_match_arm!(LeftBrace),
                b'}' => make_single_symbol_match_arm!(RightBrace),
                b'[' => make_single_symbol_match_arm!(LeftBracket),
                b']' => make_single_symbol_match_arm!(RightBracket),
                b'~' => make_single_symbol_match_arm!(Not),
                b'!' => make_double_symbol_match_arm!(b'=', LogicalNot, Unequal),
                b'-' => make_single_symbol_match_arm!(Hyphen),
                b'+' => make_single_symbol_match_arm!(Plus),
                b'*' => make_single_symbol_match_arm!(Asterisk),
                b'/' => make_single_symbol_match_arm!(Slash),
                b'%' => make_single_symbol_match_arm!(Percentage),
                b'=' => make_double_symbol_match_arm!(b'=', Assign, Equal),
                b'<' => make_double_symbol_match_arm!(b'=', Less, LessEqual),
                b'>' => make_double_symbol_match_arm!(b'=', Greater, GreaterEqual),
                b'&' => make_double_symbol_match_arm!(b'&', Et, LogicalAnd),
                b'|' => make_double_symbol_match_arm!(b'|', Or, LogicalOr),
                b'?' => make_single_symbol_match_arm!(Question),
                b':' => make_single_symbol_match_arm!(Colon),
                b',' => make_single_symbol_match_arm!(Comma),
                _ => panic!(),
            }
        }
    }
}
