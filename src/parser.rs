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
        let mut items = Vec::new();
        while self.accept_token(TokenKind::Eof).is_none() {
            let t1 = self.expect_token(TokenKind::Int);
            let t2 = self.expect_token(TokenKind::Identifier);
            if self.try_token(TokenKind::LeftParenthesis) {
                self.lexer.unget_token(t2);
                self.lexer.unget_token(t1);
                items.push(ProgramItem::Function(self.parse_function()));
            } else {
                self.lexer.unget_token(t2);
                self.lexer.unget_token(t1);
                items.push(ProgramItem::Declaration(self.parse_declaration()));
            }
        }
        Program { items }
    }

    fn parse_type(&mut self) -> Type {
        self.expect_token(TokenKind::Int);
        Type { level: 0 }
    }

    fn parse_parameter_list(&mut self) -> Vec<(String, Type)> {
        let mut ans = Vec::new();
        if !self.try_token(TokenKind::RightParenthesis) {
            loop {
                let r#type = self.parse_type();
                let name = self.expect_token(TokenKind::Identifier).text;
                ans.push((name, r#type));
                if self.accept_token(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        ans
    }

    fn parse_argument_list(&mut self) -> Vec<Expression> {
        let mut ans = Vec::new();
        if !self.try_token(TokenKind::RightParenthesis) {
            loop {
                ans.push(self.parse_expression());
                if self.accept_token(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        self.expect_token(TokenKind::RightParenthesis);
        ans
    }

    fn parse_function(&mut self) -> Function {
        let r#type = self.parse_type();
        let name = self.expect_token(TokenKind::Identifier).text;
        self.expect_token(TokenKind::LeftParenthesis);
        let parameters = self.parse_parameter_list();
        self.expect_token(TokenKind::RightParenthesis);
        Function {
            r#type,
            name,
            parameters,
            body: if self.accept_token(TokenKind::Semicolon).is_some() {
                None
            } else {
                Some(self.parse_compound().items)
            },
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
        if self.accept_token(TokenKind::Semicolon).is_some() {
            Statement::Empty
        } else if self.accept_token(TokenKind::Return).is_some() {
            let ans = Statement::Return(self.parse_expression());
            self.expect_token(TokenKind::Semicolon);
            ans
        } else if self.accept_token(TokenKind::If).is_some() {
            self.expect_token(TokenKind::LeftParenthesis);
            let condition = self.parse_expression();
            self.expect_token(TokenKind::RightParenthesis);
            let true_branch = Box::new(self.parse_statement());
            let false_branch = if self.accept_token(TokenKind::Else).is_some() {
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
        } else if self.accept_token(TokenKind::For).is_some() {
            self.expect_token(TokenKind::LeftParenthesis);
            // initializer
            let initializer = if self.accept_token(TokenKind::Semicolon).is_some() {
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
        } else if self.accept_token(TokenKind::Do).is_some() {
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
        } else if self.accept_token(TokenKind::While).is_some() {
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
        } else if self.accept_token(TokenKind::Break).is_some() {
            self.expect_token(TokenKind::Semicolon);
            Statement::Break
        } else if self.accept_token(TokenKind::Continue).is_some() {
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
            default: if self.accept_token(TokenKind::Assign).is_some() {
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
                    Expression {
                        kind: ExpressionKind::Assignment(name, Box::new(self.parse_expression())),
                        is_lvalue: false,
                    }
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
        if self.accept_token(TokenKind::Question).is_some() {
            let true_part = self.parse_expression();
            self.expect_token(TokenKind::Colon);
            let false_part = self.parse_ternary();
            Expression {
                kind: ExpressionKind::Ternary(
                    Box::new(condition),
                    Box::new(true_part),
                    Box::new(false_part),
                ),
                is_lvalue: false,
            }
        } else {
            condition
        }
    }

    fn parse_logical_or(&mut self) -> Expression {
        let mut expr = self.parse_logical_and();
        loop {
            if self.accept_token(TokenKind::LogicalOr).is_some() {
                expr = Expression {
                    kind: ExpressionKind::LogicalOr(
                        Box::new(expr),
                        Box::new(self.parse_logical_and()),
                    ),
                    is_lvalue: false,
                };
            } else {
                break expr;
            }
        }
    }

    fn parse_logical_and(&mut self) -> Expression {
        let mut expr = self.parse_equality();
        loop {
            if self.accept_token(TokenKind::LogicalAnd).is_some() {
                expr = Expression {
                    kind: ExpressionKind::LogicalAnd(
                        Box::new(expr),
                        Box::new(self.parse_equality()),
                    ),
                    is_lvalue: false,
                };
            } else {
                break expr;
            }
        }
    }

    fn parse_equality(&mut self) -> Expression {
        let mut expr = self.parse_relational();
        loop {
            if self.accept_token(TokenKind::Equal).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Equal(Box::new(expr), Box::new(self.parse_relational())),
                    is_lvalue: false,
                };
            } else if self.accept_token(TokenKind::Unequal).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Unequal(
                        Box::new(expr),
                        Box::new(self.parse_relational()),
                    ),
                    is_lvalue: false,
                };
            } else {
                break expr;
            }
        }
    }

    fn parse_relational(&mut self) -> Expression {
        let mut expr = self.parse_additive();
        loop {
            if self.accept_token(TokenKind::Less).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Less(Box::new(expr), Box::new(self.parse_additive())),
                    is_lvalue: false,
                };
            } else if self.accept_token(TokenKind::LessEqual).is_some() {
                expr = Expression {
                    kind: ExpressionKind::LessEqual(
                        Box::new(expr),
                        Box::new(self.parse_additive()),
                    ),
                    is_lvalue: false,
                };
            } else if self.accept_token(TokenKind::Greater).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Greater(Box::new(expr), Box::new(self.parse_additive())),
                    is_lvalue: false,
                };
            } else if self.accept_token(TokenKind::GreaterEqual).is_some() {
                expr = Expression {
                    kind: ExpressionKind::GreaterEqual(
                        Box::new(expr),
                        Box::new(self.parse_additive()),
                    ),
                    is_lvalue: false,
                };
            } else {
                break expr;
            }
        }
    }

    fn parse_additive(&mut self) -> Expression {
        let mut expr = self.parse_multiplicative();
        loop {
            if self.accept_token(TokenKind::Plus).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Addition(
                        Box::new(expr),
                        Box::new(self.parse_multiplicative()),
                    ),
                    is_lvalue: false,
                };
            } else if self.accept_token(TokenKind::Hyphen).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Subtraction(
                        Box::new(expr),
                        Box::new(self.parse_multiplicative()),
                    ),
                    is_lvalue: false,
                };
            } else {
                break expr;
            }
        }
    }

    fn parse_multiplicative(&mut self) -> Expression {
        let mut expr = self.parse_unary();
        loop {
            if self.accept_token(TokenKind::Asterisk).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Multiplication(
                        Box::new(expr),
                        Box::new(self.parse_unary()),
                    ),
                    is_lvalue: false,
                };
            } else if self.accept_token(TokenKind::Slash).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Division(Box::new(expr), Box::new(self.parse_unary())),
                    is_lvalue: false,
                };
            } else if self.accept_token(TokenKind::Percentage).is_some() {
                expr = Expression {
                    kind: ExpressionKind::Modulus(Box::new(expr), Box::new(self.parse_unary())),
                    is_lvalue: false,
                };
            } else {
                break expr;
            }
        }
    }

    fn parse_unary(&mut self) -> Expression {
        if self.accept_token(TokenKind::Hyphen).is_some() {
            Expression {
                kind: ExpressionKind::Negation(Box::new(self.parse_unary())),
                is_lvalue: false,
            }
        } else if self.accept_token(TokenKind::Not).is_some() {
            Expression {
                kind: ExpressionKind::Not(Box::new(self.parse_unary())),
                is_lvalue: false,
            }
        } else if self.accept_token(TokenKind::LogicalNot).is_some() {
            Expression {
                kind: ExpressionKind::LogicalNot(Box::new(self.parse_unary())),
                is_lvalue: false,
            }
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Expression {
        if let Some(token) = self.accept_token(TokenKind::Identifier) {
            if self.accept_token(TokenKind::LeftParenthesis).is_some() {
                Expression {
                    kind: ExpressionKind::FunctionCall(token.text, self.parse_argument_list()),
                    is_lvalue: false,
                }
            } else {
                self.lexer.unget_token(token);
                self.parse_primary()
            }
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Expression {
        if self.accept_token(TokenKind::LeftParenthesis).is_some() {
            let expression = self.parse_expression();
            self.expect_token(TokenKind::RightParenthesis);
            expression
        } else if let Some(name) = self.accept_token(TokenKind::Identifier) {
            let name = name.text;
            Expression {
                kind: ExpressionKind::Identifier(name),
                is_lvalue: true,
            }
        } else {
            let token = self.expect_token(TokenKind::Integer);
            let value = token.text;
            let value = value.parse().unwrap(); // Should not invoke any error
            Expression {
                kind: ExpressionKind::IntegerLiteral(value),
                is_lvalue: false,
            }
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
        assert_eq!(token.kind, kind);
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
