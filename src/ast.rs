use crate::ir::Context;

pub trait Ast {
    fn accept(&self, visitor: &mut Context);
}

#[derive(Debug)]
pub struct Program {
    pub function: Function,
}

impl Ast for Program {
    fn accept(&self, visitor: &mut Context) {
        visitor.visit_program(self);
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Statement,
}

impl Ast for Function {
    fn accept(&self, visitor: &mut Context) {
        visitor.visit_function(self);
    }
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

impl Ast for Statement {
    fn accept(&self, visitor: &mut Context) {
        visitor.visit_statement(self);
    }
}

#[derive(Debug)]
pub enum Expression {
    IntegerLiteral(i32),
    Negation(Box<Expression>),
    Not(Box<Expression>),
    LogicalNot(Box<Expression>),
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Modulus(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Unequal(Box<Expression>, Box<Expression>),
    Less(Box<Expression>, Box<Expression>),
    LessEqual(Box<Expression>, Box<Expression>),
    Greater(Box<Expression>, Box<Expression>),
    GreaterEqual(Box<Expression>, Box<Expression>),
    LogicalAnd(Box<Expression>, Box<Expression>),
    LogicalOr(Box<Expression>, Box<Expression>),
}
