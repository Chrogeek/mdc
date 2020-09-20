use crate::ast::*;
use crate::lexer::*;
use crate::util::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl Parser<'_> {
    pub fn new(source: &'_ [u8]) -> Parser {
        Parser {
            lexer: Lexer::new(source),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let function = self.parse_function();
        self.expect_token(TokenKind::Eof);
        Program { function }
    }

    fn parse_type(&mut self) -> Type {
        self.expect_token(TokenKind::Int);
        Type { level: 1 }
    }

    fn parse_function(&mut self) -> Function {
        let r#type = self.parse_type();
        let name = self.expect_token(TokenKind::Identifier).text;
        self.expect_token(TokenKind::LeftParenthesis);
        self.expect_token(TokenKind::RightParenthesis);
        Function {
            r#type,
            name,
            body: self.parse_compound(),
        }
    }

    fn parse_compound(&mut self) -> Compound {
        self.expect_token(TokenKind::LeftBrace);
        let mut items = Vec::new();
        while let None = self.accept_token(TokenKind::RightBrace) {
            items.push(self.parse_block_item());
        }
        Compound { items }
    }

    fn parse_statement(&mut self) -> Statement {
        if let Some(_) = self.accept_token(TokenKind::Semicolon) {
            Statement::Empty
        } else if let Some(_) = self.accept_token(TokenKind::Return) {
            let ans = Statement::Return(self.parse_expression());
            self.expect_token(TokenKind::Semicolon);
            ans
        } else if let Some(_) = self.accept_token(TokenKind::If) {
            self.expect_token(TokenKind::LeftParenthesis);
            let condition = self.parse_expression();
            self.expect_token(TokenKind::RightParenthesis);
            let true_branch = Box::new(self.parse_statement());
            let false_branch = if let Some(_) = self.accept_token(TokenKind::Else) {
                Some(Box::new(self.parse_statement()))
            } else {
                None
            };
            Statement::If {
                condition,
                true_branch,
                false_branch,
            }
        } else if self.try_token(TokenKind::LeftBrace) {
            Statement::Compound(self.parse_compound())
        } else if let Some(_) = self.accept_token(TokenKind::For) {
            self.expect_token(TokenKind::LeftParenthesis);
            // initializer
            let initializer = if let Some(_) = self.accept_token(TokenKind::Semicolon) {
                None
            } else {
                Some(Box::new(if self.try_token(TokenKind::Int) {
                    BlockItem::Declaration(self.parse_declaration())
                } else {
                    let expression = self.parse_expression();
                    self.expect_token(TokenKind::Semicolon);
                    BlockItem::Statement(Statement::Expression(expression))
                }))
            };
            // condition
            let condition = if self.try_token(TokenKind::Semicolon) {
                None
            } else {
                Some(self.parse_expression())
            };
            self.expect_token(TokenKind::Semicolon);
            // modifier
            let modifier = if self.try_token(TokenKind::RightParenthesis) {
                None
            } else {
                Some(self.parse_expression())
            };
            self.expect_token(TokenKind::RightParenthesis);
            let body = self.parse_statement();
            Statement::Loop {
                initializer,
                condition,
                body: Box::new(body),
                modifier,
            }
        } else if let Some(_) = self.accept_token(TokenKind::Do) {
            let body = self.parse_statement();
            self.expect_token(TokenKind::While);
            self.expect_token(TokenKind::LeftParenthesis);
            let condition = self.parse_expression();
            self.expect_token(TokenKind::RightParenthesis);
            Statement::Loop {
                initializer: None,
                condition: Some(condition),
                body: Box::new(body),
                modifier: None,
            }
        } else if let Some(_) = self.accept_token(TokenKind::While) {
            self.expect_token(TokenKind::LeftParenthesis);
            let condition = self.parse_expression();
            self.expect_token(TokenKind::RightParenthesis);
            let body = self.parse_statement();
            Statement::Loop {
                initializer: None,
                condition: Some(condition),
                body: Box::new(body),
                modifier: None,
            }
        } else if let Some(_) = self.accept_token(TokenKind::Break) {
            self.expect_token(TokenKind::Semicolon);
            Statement::Break
        } else if let Some(_) = self.accept_token(TokenKind::Continue) {
            self.expect_token(TokenKind::Semicolon);
            Statement::Continue
        } else {
            let ans = Statement::Expression(self.parse_expression());
            self.expect_token(TokenKind::Semicolon);
            ans
        }
    }

    fn parse_declaration(&mut self) -> Declaration {
        self.expect_token(TokenKind::Int);
        let ans = Declaration {
            r#type: Type { level: 0 },
            name: self.expect_token(TokenKind::Identifier).text,
            default: if let Some(_) = self.accept_token(TokenKind::Assign) {
                Some(self.parse_expression())
            } else {
                None
            },
        };
        self.expect_token(TokenKind::Semicolon);
        ans
    }

    fn parse_block_item(&mut self) -> BlockItem {
        if self.try_token(TokenKind::Int) {
            BlockItem::Declaration(self.parse_declaration())
        } else {
            BlockItem::Statement(self.parse_statement())
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Expression {
        match self.accept_token(TokenKind::Identifier) {
            Some(token) => {
                if self.try_token(TokenKind::Assign) {
                    let name = token.text;
                    self.expect_token(TokenKind::Assign);
                    Expression::Assignment(name, Box::new(self.parse_expression()))
                } else {
                    self.lexer.unget_token(token);
                    self.parse_ternary()
                }
            }
            None => self.parse_ternary(),
        }
    }

    fn parse_ternary(&mut self) -> Expression {
        let condition = self.parse_logical_or();
        if let Some(_) = self.accept_token(TokenKind::Question) {
            let true_part = self.parse_expression();
            self.expect_token(TokenKind::Colon);
            let false_part = self.parse_ternary();
            Expression::Ternary(
                Box::new(condition),
                Box::new(true_part),
                Box::new(false_part),
            )
        } else {
            condition
        }
    }

    fn parse_logical_or(&mut self) -> Expression {
        let mut expr = self.parse_logical_and();
        loop {
            if let Some(_) = self.accept_token(TokenKind::LogicalOr) {
                expr = Expression::LogicalOr(Box::new(expr), Box::new(self.parse_logical_and()));
            } else {
                break expr;
            }
        }
    }

    fn parse_logical_and(&mut self) -> Expression {
        let mut expr = self.parse_equality();
        loop {
            if let Some(_) = self.accept_token(TokenKind::LogicalAnd) {
                expr = Expression::LogicalAnd(Box::new(expr), Box::new(self.parse_equality()));
            } else {
                break expr;
            }
        }
    }

    fn parse_equality(&mut self) -> Expression {
        let mut expr = self.parse_relational();
        loop {
            if let Some(_) = self.accept_token(TokenKind::Equal) {
                expr = Expression::Equal(Box::new(expr), Box::new(self.parse_relational()));
            } else if let Some(_) = self.accept_token(TokenKind::Unequal) {
                expr = Expression::Unequal(Box::new(expr), Box::new(self.parse_relational()));
            } else {
                break expr;
            }
        }
    }

    fn parse_relational(&mut self) -> Expression {
        let mut expr = self.parse_additive();
        loop {
            if let Some(_) = self.accept_token(TokenKind::Less) {
                expr = Expression::Less(Box::new(expr), Box::new(self.parse_additive()));
            } else if let Some(_) = self.accept_token(TokenKind::LessEqual) {
                expr = Expression::LessEqual(Box::new(expr), Box::new(self.parse_additive()));
            } else if let Some(_) = self.accept_token(TokenKind::Greater) {
                expr = Expression::Greater(Box::new(expr), Box::new(self.parse_additive()));
            } else if let Some(_) = self.accept_token(TokenKind::GreaterEqual) {
                expr = Expression::GreaterEqual(Box::new(expr), Box::new(self.parse_additive()));
            } else {
                break expr;
            }
        }
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
        } else if let Some(name) = self.accept_token(TokenKind::Identifier) {
            let name = name.text;
            Expression::Identifier(name)
        } else {
            let token = self.expect_token(TokenKind::Integer);
            let value = token.text;
            let value = value // Should not invoke any error
                .parse()
                .expect(&format!("Cannot cast '{}' to an 'int' literal", value));
            Expression::IntegerLiteral(value)
        }
    }

    // Accepts a token with specified kind.
    // If fails, returns 'None' and put the token back.
    fn accept_token(&mut self, kind: TokenKind) -> Option<Token> {
        let token = self.lexer.fetch_token();
        if token.kind == kind {
            Some(token)
        } else {
            self.lexer.unget_token(token);
            None
        }
    }

    // Expects the next token to be with the specified kind, panics if fails.
    fn expect_token(&mut self, kind: TokenKind) -> Token {
        let token = self.lexer.fetch_token();
        assert_eq!(
            token.kind, kind,
            "Line {}, column {}: Expected {:?}, got {:?}",
            self.lexer.row, self.lexer.col, kind, token.kind
        );
        token
    }

    // Like 'accept_token', but always puts the token back, whether succeeds or not
    fn try_token(&mut self, kind: TokenKind) -> bool {
        let token = self.lexer.fetch_token();
        if token.kind == kind {
            self.lexer.unget_token(token);
            true
        } else {
            self.lexer.unget_token(token);
            false
        }
    }
}
