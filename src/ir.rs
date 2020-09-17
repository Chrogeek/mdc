use crate::ast::*;
use crate::generator::*;
use std::io::Write;

pub enum Instruction {
    Directive(String),
    Push(i32),
    Return,
    Negate,
    Not,
    LogicalNot,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

pub struct Context {
    ir: Vec<Instruction>,
}

impl Context {
    pub fn new() -> Context {
        Context { ir: Vec::new() }
    }

    pub fn visit_program(&mut self, program: &Program) {
        if program.function.name != "main" {
            panic!("No entry point 'main' defined");
        }
        self.ir.push(Instruction::Directive(".text".to_string()));
        program.function.accept(self);
    }

    pub fn visit_function(&mut self, function: &Function) {
        self.ir.push(Instruction::Directive(format!(
            ".globl {0}\n{0}:",
            function.name
        )));
        function.body.accept(self);
    }

    pub fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Return(expression) => {
                self.visit_expression(expression);
                self.ir.push(Instruction::Return);
            }
        };
    }

    pub fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::IntegerLiteral(value) => self.ir.push(Instruction::Push(*value)),
            Expression::Negation(rhs) => {
                self.visit_expression(rhs);
                self.ir.push(Instruction::Negate);
            }
            Expression::Not(rhs) => {
                self.visit_expression(rhs);
                self.ir.push(Instruction::Not);
            }
            Expression::LogicalNot(rhs) => {
                self.visit_expression(rhs);
                self.ir.push(Instruction::LogicalNot);
            }
            Expression::Addition(lhs, rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
                self.ir.push(Instruction::Add);
            }
            Expression::Subtraction(lhs, rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
                self.ir.push(Instruction::Subtract);
            }
            Expression::Multiplication(lhs, rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
                self.ir.push(Instruction::Multiply);
            }
            Expression::Division(lhs, rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
                self.ir.push(Instruction::Divide);
            }
            Expression::Modulus(lhs, rhs) => {
                self.visit_expression(lhs);
                self.visit_expression(rhs);
                self.ir.push(Instruction::Modulo);
            }
        }
    }

    pub fn generate(&self, output: &mut impl Write) -> Result<(), std::io::Error> {
        for instruction in self.ir.iter() {
            generate_instruction(instruction, output)?;
        }
        Ok(())
    }
}
