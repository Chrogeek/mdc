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
            assert!(self.try_token(TokenKind::Int));
            let mut t = self.parse_type();
            let token = self.expect_token(TokenKind::Identifier);
            let next_parenthesis = self.try_token(TokenKind::LeftParenthesis);

            self.lexer.unget_token(token);
            while t.is_pointer() {
                self.lexer.unget_token(Token {
                    kind: TokenKind::Asterisk,
                    text: "*".to_string(),
                });
                t = t.unwrap_pointer();
            }
            self.lexer.unget_token(Token {
                kind: TokenKind::Int,
                text: "int".to_string(),
            });

            if next_parenthesis {
                items.push(ProgramItem::Function(self.parse_function()));
            } else {
                items.push(ProgramItem::Declaration(self.parse_declaration()));
            }
        }
        Program { items }
    }

    fn parse_type(&mut self) -> Type {
        self.expect_token(TokenKind::Int);
        let mut ty = Type::make_primitive();
        while self.accept_token(TokenKind::Asterisk).is_some() {
            ty = ty.wrap_pointer();
        }
        ty
    }

    fn parse_parameter_list(&mut self) -> Vec<(String, Type)> {
        let mut ans = Vec::new();
        if !self.try_token(TokenKind::RightParenthesis) {
            loop {
                let ty = self.parse_type();
                let name = self.expect_token(TokenKind::Identifier).text;
                ans.push((name, ty));
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
        ans
    }

    fn parse_function(&mut self) -> Function {
        let ty = self.parse_type();
        let name = self.expect_token(TokenKind::Identifier).text;
        self.expect_token(TokenKind::LeftParenthesis);
        let parameters = self.parse_parameter_list();
        self.expect_token(TokenKind::RightParenthesis);
        Function {
            ty,
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
        assert!(self.try_token(TokenKind::Int));
        let mut ty = self.parse_type();
        let name = self.expect_token(TokenKind::Identifier).text;
        let ans = if self.accept_token(TokenKind::Assign).is_some() {
            Declaration {
                ty,
                name,
                default: Some(self.parse_expression()),
            }
        } else {
            let mut bounds = Vec::new();
            while self.accept_token(TokenKind::LeftBracket).is_some() {
                let index = self
                    .expect_token(TokenKind::Integer)
                    .text
                    .parse::<i32>()
                    .unwrap() as u32;
                assert!(index > 0);
                bounds.push(index);
                self.expect_token(TokenKind::RightBracket);
            }
            for size in bounds.into_iter().rev() {
                ty = ty.wrap_array(size);
            }
            Declaration {
                ty,
                name,
                default: None,
            }
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
        let left = self.parse_ternary();
        if self.accept_token(TokenKind::Assign).is_some() {
            Expression::Assignment(Box::new(left), Box::new(self.parse_expression()))
        } else {
            left
        }
    }

    fn parse_ternary(&mut self) -> Expression {
        let condition = self.parse_logical_or();
        if self.accept_token(TokenKind::Question).is_some() {
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
            if self.accept_token(TokenKind::LogicalOr).is_some() {
                expr = Expression::LogicalOr(Box::new(expr), Box::new(self.parse_logical_and()));
            } else {
                break expr;
            }
        }
    }

    fn parse_logical_and(&mut self) -> Expression {
        let mut expr = self.parse_equality();
        loop {
            if self.accept_token(TokenKind::LogicalAnd).is_some() {
                expr = Expression::LogicalAnd(Box::new(expr), Box::new(self.parse_equality()));
            } else {
                break expr;
            }
        }
    }

    fn parse_equality(&mut self) -> Expression {
        let mut expr = self.parse_relational();
        loop {
            if self.accept_token(TokenKind::Equal).is_some() {
                expr = Expression::Equal(Box::new(expr), Box::new(self.parse_relational()));
            } else if self.accept_token(TokenKind::Unequal).is_some() {
                expr = Expression::Unequal(Box::new(expr), Box::new(self.parse_relational()));
            } else {
                break expr;
            }
        }
    }

    fn parse_relational(&mut self) -> Expression {
        let mut expr = self.parse_additive();
        loop {
            if self.accept_token(TokenKind::Less).is_some() {
                expr = Expression::Less(Box::new(expr), Box::new(self.parse_additive()));
            } else if self.accept_token(TokenKind::LessEqual).is_some() {
                expr = Expression::LessEqual(Box::new(expr), Box::new(self.parse_additive()));
            } else if self.accept_token(TokenKind::Greater).is_some() {
                expr = Expression::Greater(Box::new(expr), Box::new(self.parse_additive()));
            } else if self.accept_token(TokenKind::GreaterEqual).is_some() {
                expr = Expression::GreaterEqual(Box::new(expr), Box::new(self.parse_additive()));
            } else {
                break expr;
            }
        }
    }

    fn parse_additive(&mut self) -> Expression {
        let mut expr = self.parse_multiplicative();
        loop {
            if self.accept_token(TokenKind::Plus).is_some() {
                expr = Expression::Addition(Box::new(expr), Box::new(self.parse_multiplicative()));
            } else if self.accept_token(TokenKind::Hyphen).is_some() {
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
            if self.accept_token(TokenKind::Asterisk).is_some() {
                expr = Expression::Multiplication(Box::new(expr), Box::new(self.parse_unary()));
            } else if self.accept_token(TokenKind::Slash).is_some() {
                expr = Expression::Division(Box::new(expr), Box::new(self.parse_unary()));
            } else if self.accept_token(TokenKind::Percentage).is_some() {
                expr = Expression::Modulus(Box::new(expr), Box::new(self.parse_unary()));
            } else {
                break expr;
            }
        }
    }

    fn parse_unary(&mut self) -> Expression {
        if self.accept_token(TokenKind::Hyphen).is_some() {
            Expression::Negation(Box::new(self.parse_unary()))
        } else if self.accept_token(TokenKind::Not).is_some() {
            Expression::Not(Box::new(self.parse_unary()))
        } else if self.accept_token(TokenKind::LogicalNot).is_some() {
            Expression::LogicalNot(Box::new(self.parse_unary()))
        } else if self.accept_token(TokenKind::Asterisk).is_some() {
            Expression::Dereference(Box::new(self.parse_unary()))
        } else if self.accept_token(TokenKind::Et).is_some() {
            Expression::Reference(Box::new(self.parse_unary()))
        } else if let Some(token) = self.accept_token(TokenKind::LeftParenthesis) {
            if self.try_token(TokenKind::Int) {
                let target = self.parse_type();
                self.expect_token(TokenKind::RightParenthesis);
                let sub = self.parse_unary();
                Expression::Convert(target, Box::new(sub))
            } else {
                self.lexer.unget_token(token);
                self.parse_postfix()
            }
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Expression {
        let base = if let Some(token) = self.accept_token(TokenKind::Identifier) {
            if self.accept_token(TokenKind::LeftParenthesis).is_some() {
                let ans = Expression::FunctionCall(token.text, self.parse_argument_list());
                self.expect_token(TokenKind::RightParenthesis);
                ans
            } else {
                self.lexer.unget_token(token);
                self.parse_primary()
            }
        } else {
            self.parse_primary()
        };
        let mut indices = Vec::new();
        while self.accept_token(TokenKind::LeftBracket).is_some() {
            indices.push(self.parse_expression());
            self.expect_token(TokenKind::RightBracket);
        }
        if indices.len() == 0 {
            base
        } else {
            Expression::Index(Box::new(base), indices)
        }
    }

    fn parse_primary(&mut self) -> Expression {
        if self.accept_token(TokenKind::LeftParenthesis).is_some() {
            let expression = self.parse_expression();
            self.expect_token(TokenKind::RightParenthesis);
            expression
        } else if let Some(name) = self.accept_token(TokenKind::Identifier) {
            let name = name.text;
            Expression::Identifier(name)
        } else {
            let token = self.expect_token(TokenKind::Integer);
            let value = token.text;
            let value = value.parse().unwrap(); // Should not invoke any error
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
        assert!(token.kind == kind);
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
