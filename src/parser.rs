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
        let function = self.parse_function();
        self.expect_token(TokenKind::Eof);
        Program { function }
    }

    // fn parse_type(&mut self) {} // Reserved interface

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
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Expression {
        let mut expr = self.parse_multiplicative();
        loop {
            if let Some(_) = self.accept_token(TokenKind::Plus) {
                expr = Expression::Addition(Box::new(expr), Box::new(self.parse_multiplicative()));
            } else if let Some(_) = self.accept_token(TokenKind::Hyphen) {
                expr =
                    Expression::Subtraction(Box::new(expr), Box::new(self.parse_multiplicative()));
            } else {
                break expr;
            }
        }
    }

    fn parse_multiplicative(&mut self) -> Expression {
        let mut expr = self.parse_unary();
        loop {
            if let Some(_) = self.accept_token(TokenKind::Asterisk) {
                expr = Expression::Multiplication(Box::new(expr), Box::new(self.parse_unary()));
            } else if let Some(_) = self.accept_token(TokenKind::Slash) {
                expr = Expression::Division(Box::new(expr), Box::new(self.parse_unary()));
            } else if let Some(_) = self.accept_token(TokenKind::Percentage) {
                expr = Expression::Modulus(Box::new(expr), Box::new(self.parse_unary()));
            } else {
                break expr;
            }
        }
    }

    fn parse_unary(&mut self) -> Expression {
        if let Some(_) = self.accept_token(TokenKind::Hyphen) {
            Expression::Negation(Box::new(self.parse_unary()))
        } else if let Some(_) = self.accept_token(TokenKind::Not) {
            Expression::Not(Box::new(self.parse_unary()))
        } else if let Some(_) = self.accept_token(TokenKind::LogicalNot) {
            Expression::LogicalNot(Box::new(self.parse_unary()))
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Expression {
        if let Some(_) = self.accept_token(TokenKind::LeftParenthesis) {
            let expression = self.parse_expression();
            self.expect_token(TokenKind::RightParenthesis);
            expression
        } else {
            let token = self.expect_token(TokenKind::Integer);
            let value = String::from_utf8(token.slice.to_vec()).unwrap();
            let value = value // Should not invoke any error
                .parse()
                .expect(&format!("Cannot cast '{}' to an 'int' literal", value));
            Expression::IntegerLiteral(value)
        }
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
