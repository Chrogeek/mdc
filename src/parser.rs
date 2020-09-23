use crate::ast::*;
use crate::lexer::*;
use crate::util::*;
use std::mem::*;

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
        while self.accept_token(Token::Eof).is_none() {
            assert!(self.try_token(Token::Int));
            let mut t = self.parse_type();
            let token = self.expect_token(Token::Identifier(String::new()));
            let next_parenthesis = self.try_token(Token::LeftParenthesis);

            self.lexer.unget_token(token);
            while t.is_pointer() {
                self.lexer.unget_token(Token::Asterisk);
                t = t.unwrap_pointer();
            }
            self.lexer.unget_token(Token::Int);

            if next_parenthesis {
                items.push(ProgramItem::Function(self.parse_function()));
            } else {
                items.push(ProgramItem::Declaration(self.parse_declaration()));
            }
        }
        Program { items }
    }

    fn parse_type(&mut self) -> Type {
        self.expect_token(Token::Int);
        let mut ty = Type::make_primitive();
        while self.accept_token(Token::Asterisk).is_some() {
            ty = ty.wrap_pointer();
        }
        ty
    }

    fn parse_parameter_list(&mut self) -> Vec<(String, Type)> {
        let mut ans = Vec::new();
        if !self.try_token(Token::RightParenthesis) {
            loop {
                let ty = self.parse_type();
                let name = self
                    .expect_token(Token::Identifier(String::new()))
                    .unwrap_identifier();
                ans.push((name, ty));
                if self.accept_token(Token::Comma).is_none() {
                    break;
                }
            }
        }
        ans
    }

    fn parse_argument_list(&mut self) -> Vec<Expression> {
        let mut ans = Vec::new();
        if !self.try_token(Token::RightParenthesis) {
            loop {
                ans.push(self.parse_expression());
                if self.accept_token(Token::Comma).is_none() {
                    break;
                }
            }
        }
        ans
    }

    fn parse_function(&mut self) -> Function {
        let ty = self.parse_type();
        let name = self
            .expect_token(Token::Identifier(String::new()))
            .unwrap_identifier();
        self.expect_token(Token::LeftParenthesis);
        let parameters = self.parse_parameter_list();
        self.expect_token(Token::RightParenthesis);
        Function {
            ty,
            name,
            parameters,
            body: if self.accept_token(Token::Semicolon).is_some() {
                None
            } else {
                Some(self.parse_compound().items)
            },
        }
    }

    fn parse_compound(&mut self) -> Compound {
        self.expect_token(Token::LeftBrace);
        let mut items = Vec::new();
        while let None = self.accept_token(Token::RightBrace) {
            items.push(self.parse_block_item());
        }
        Compound { items }
    }

    fn parse_statement(&mut self) -> Statement {
        if self.accept_token(Token::Semicolon).is_some() {
            Statement::Empty
        } else if self.accept_token(Token::Return).is_some() {
            let ans = Statement::Return(self.parse_expression());
            self.expect_token(Token::Semicolon);
            ans
        } else if self.accept_token(Token::If).is_some() {
            self.expect_token(Token::LeftParenthesis);
            let condition = self.parse_expression();
            self.expect_token(Token::RightParenthesis);
            let true_branch = Box::new(self.parse_statement());
            let false_branch = if self.accept_token(Token::Else).is_some() {
                Some(Box::new(self.parse_statement()))
            } else {
                None
            };
            Statement::If {
                condition,
                true_branch,
                false_branch,
            }
        } else if self.try_token(Token::LeftBrace) {
            Statement::Compound(self.parse_compound())
        } else if self.accept_token(Token::For).is_some() {
            self.expect_token(Token::LeftParenthesis);
            // initializer
            let initializer = if self.accept_token(Token::Semicolon).is_some() {
                None
            } else {
                Some(Box::new(if self.try_token(Token::Int) {
                    BlockItem::Declaration(self.parse_declaration())
                } else {
                    let expression = self.parse_expression();
                    self.expect_token(Token::Semicolon);
                    BlockItem::Statement(Statement::Expression(expression))
                }))
            };
            // condition
            let condition = if self.try_token(Token::Semicolon) {
                None
            } else {
                Some(self.parse_expression())
            };
            self.expect_token(Token::Semicolon);
            // modifier
            let modifier = if self.try_token(Token::RightParenthesis) {
                None
            } else {
                Some(self.parse_expression())
            };
            self.expect_token(Token::RightParenthesis);
            let body = self.parse_statement();
            Statement::Loop {
                initializer,
                condition,
                body: Box::new(body),
                modifier,
            }
        } else if self.accept_token(Token::Do).is_some() {
            let body = self.parse_statement();
            self.expect_token(Token::While);
            self.expect_token(Token::LeftParenthesis);
            let condition = self.parse_expression();
            self.expect_token(Token::RightParenthesis);
            Statement::Loop {
                initializer: None,
                condition: Some(condition),
                body: Box::new(body),
                modifier: None,
            }
        } else if self.accept_token(Token::While).is_some() {
            self.expect_token(Token::LeftParenthesis);
            let condition = self.parse_expression();
            self.expect_token(Token::RightParenthesis);
            let body = self.parse_statement();
            Statement::Loop {
                initializer: None,
                condition: Some(condition),
                body: Box::new(body),
                modifier: None,
            }
        } else if self.accept_token(Token::Break).is_some() {
            self.expect_token(Token::Semicolon);
            Statement::Break
        } else if self.accept_token(Token::Continue).is_some() {
            self.expect_token(Token::Semicolon);
            Statement::Continue
        } else {
            let ans = Statement::Expression(self.parse_expression());
            self.expect_token(Token::Semicolon);
            ans
        }
    }

    fn parse_declaration(&mut self) -> Declaration {
        assert!(self.try_token(Token::Int));
        let mut ty = self.parse_type();
        let name = self
            .expect_token(Token::Identifier(String::new()))
            .unwrap_identifier();
        let ans = if self.accept_token(Token::Assign).is_some() {
            Declaration {
                ty,
                name,
                default: Some(self.parse_expression()),
            }
        } else {
            let mut bounds = Vec::new();
            while self.accept_token(Token::LeftBracket).is_some() {
                let index = self.expect_token(Token::Integer(0)).unwrap_integer();
                assert!(index > 0);
                bounds.push(index as u32);
                self.expect_token(Token::RightBracket);
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
        self.expect_token(Token::Semicolon);
        ans
    }

    fn parse_block_item(&mut self) -> BlockItem {
        if self.try_token(Token::Int) {
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
        if self.accept_token(Token::Assign).is_some() {
            Expression::Assignment(Box::new(left), Box::new(self.parse_expression()))
        } else {
            left
        }
    }

    fn parse_ternary(&mut self) -> Expression {
        let condition = self.parse_logical_or();
        if self.accept_token(Token::Question).is_some() {
            let true_part = self.parse_expression();
            self.expect_token(Token::Colon);
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
            if self.accept_token(Token::LogicalOr).is_some() {
                expr = Expression::LogicalOr(Box::new(expr), Box::new(self.parse_logical_and()));
            } else {
                break expr;
            }
        }
    }

    fn parse_logical_and(&mut self) -> Expression {
        let mut expr = self.parse_equality();
        loop {
            if self.accept_token(Token::LogicalAnd).is_some() {
                expr = Expression::LogicalAnd(Box::new(expr), Box::new(self.parse_equality()));
            } else {
                break expr;
            }
        }
    }

    fn parse_equality(&mut self) -> Expression {
        let mut expr = self.parse_relational();
        loop {
            if self.accept_token(Token::Equal).is_some() {
                expr = Expression::Equal(Box::new(expr), Box::new(self.parse_relational()));
            } else if self.accept_token(Token::Unequal).is_some() {
                expr = Expression::Unequal(Box::new(expr), Box::new(self.parse_relational()));
            } else {
                break expr;
            }
        }
    }

    fn parse_relational(&mut self) -> Expression {
        let mut expr = self.parse_additive();
        loop {
            if self.accept_token(Token::Less).is_some() {
                expr = Expression::Less(Box::new(expr), Box::new(self.parse_additive()));
            } else if self.accept_token(Token::LessEqual).is_some() {
                expr = Expression::LessEqual(Box::new(expr), Box::new(self.parse_additive()));
            } else if self.accept_token(Token::Greater).is_some() {
                expr = Expression::Greater(Box::new(expr), Box::new(self.parse_additive()));
            } else if self.accept_token(Token::GreaterEqual).is_some() {
                expr = Expression::GreaterEqual(Box::new(expr), Box::new(self.parse_additive()));
            } else {
                break expr;
            }
        }
    }

    fn parse_additive(&mut self) -> Expression {
        let mut expr = self.parse_multiplicative();
        loop {
            if self.accept_token(Token::Plus).is_some() {
                expr = Expression::Addition(Box::new(expr), Box::new(self.parse_multiplicative()));
            } else if self.accept_token(Token::Hyphen).is_some() {
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
            if self.accept_token(Token::Asterisk).is_some() {
                expr = Expression::Multiplication(Box::new(expr), Box::new(self.parse_unary()));
            } else if self.accept_token(Token::Slash).is_some() {
                expr = Expression::Division(Box::new(expr), Box::new(self.parse_unary()));
            } else if self.accept_token(Token::Percentage).is_some() {
                expr = Expression::Modulus(Box::new(expr), Box::new(self.parse_unary()));
            } else {
                break expr;
            }
        }
    }

    fn parse_unary(&mut self) -> Expression {
        if self.accept_token(Token::Hyphen).is_some() {
            Expression::Negation(Box::new(self.parse_unary()))
        } else if self.accept_token(Token::Not).is_some() {
            Expression::Not(Box::new(self.parse_unary()))
        } else if self.accept_token(Token::LogicalNot).is_some() {
            Expression::LogicalNot(Box::new(self.parse_unary()))
        } else if self.accept_token(Token::Asterisk).is_some() {
            Expression::Dereference(Box::new(self.parse_unary()))
        } else if self.accept_token(Token::Et).is_some() {
            Expression::Reference(Box::new(self.parse_unary()))
        } else if let Some(token) = self.accept_token(Token::LeftParenthesis) {
            if self.try_token(Token::Int) {
                let target = self.parse_type();
                self.expect_token(Token::RightParenthesis);
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
        let base = if let Some(token) = self.accept_token(Token::Identifier(String::new())) {
            if self.accept_token(Token::LeftParenthesis).is_some() {
                let ans =
                    Expression::FunctionCall(token.unwrap_identifier(), self.parse_argument_list());
                self.expect_token(Token::RightParenthesis);
                ans
            } else {
                self.lexer.unget_token(token);
                self.parse_primary()
            }
        } else {
            self.parse_primary()
        };
        let mut indices = Vec::new();
        while self.accept_token(Token::LeftBracket).is_some() {
            indices.push(self.parse_expression());
            self.expect_token(Token::RightBracket);
        }
        if indices.len() == 0 {
            base
        } else {
            Expression::Index(Box::new(base), indices)
        }
    }

    fn parse_primary(&mut self) -> Expression {
        if self.accept_token(Token::LeftParenthesis).is_some() {
            let expression = self.parse_expression();
            self.expect_token(Token::RightParenthesis);
            expression
        } else if let Some(Token::Identifier(name)) =
            self.accept_token(Token::Identifier(String::new()))
        {
            Expression::Identifier(name)
        } else {
            Expression::IntegerLiteral(self.expect_token(Token::Integer(0)).unwrap_integer())
        }
    }

    // Accepts a token with specified kind.
    // If fails, returns 'None' and put the token back.
    fn accept_token(&mut self, kind: Token) -> Option<Token> {
        let token = self.lexer.fetch_token();
        if discriminant(&token) == discriminant(&kind) {
            Some(token)
        } else {
            self.lexer.unget_token(token);
            None
        }
    }

    // Expects the next token to be with the specified kind, panics if fails.
    fn expect_token(&mut self, kind: Token) -> Token {
        let token = self.lexer.fetch_token();
        assert!(discriminant(&token) == discriminant(&kind));
        token
    }

    // Like 'accept_token', but always puts the token back, whether succeeds or not
    fn try_token(&mut self, kind: Token) -> bool {
        let token = self.lexer.fetch_token();
        self.lexer.unget_token(token.clone());
        discriminant(&token) == discriminant(&kind)
    }
}
