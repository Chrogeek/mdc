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
}

// impl Expression {
//     pub fn accept(&self, visitor: &mut Context) {
//         visitor.visit_expression(self);
//     }
// }
