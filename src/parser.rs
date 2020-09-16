use crate::ast::*;
use crate::lexer::*;
use crate::token::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a [u8]) -> Parser {
        Parser {
            lexer: Lexer::new(source),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        Program {
            function: self.parse_function(),
        }
    }

    fn parse_function(&mut self) -> Function {
        self.expect_token(TokenKind::Int);
        let name =
            String::from_utf8_lossy(self.expect_token(TokenKind::Identifier).slice).to_string();
        self.expect_token(TokenKind::LeftParenthesis);
        self.expect_token(TokenKind::RightParenthesis);
        self.expect_token(TokenKind::LeftBrace);
        let body = self.parse_statement();
        self.expect_token(TokenKind::RightBrace);
        Function { name, body }
    }

    fn parse_statement(&mut self) -> Statement {
        let ans = if let Some(_) = self.accept_token(TokenKind::Return) {
            Statement::Return(self.parse_expression())
        } else {
            unreachable!();
        };
        self.expect_token(TokenKind::Semicolon);
        ans
    }

    fn parse_expression(&mut self) -> Expression {
        let token = self.expect_token(TokenKind::Integer);
        let value = String::from_utf8(token.slice.to_vec())
            .unwrap()
            .parse()
            .unwrap();
        Expression::IntegerLiteral(value)
    }

    fn accept_token(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.lexer.fetch_token().unwrap();
        if token.kind == kind {
            Some(token)
        } else {
            self.lexer.unget_token(token);
            None
        }
    }

    fn expect_token(&mut self, kind: TokenKind) -> Token {
        let token = self.lexer.fetch_token().unwrap();
        assert_eq!(
            token.kind, kind,
            "Line {}, column {}: Expected {:?}, got {:?}",
            self.lexer.row, self.lexer.col, kind, token.kind
        );
        token
    }
}
