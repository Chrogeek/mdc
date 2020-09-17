use crate::ast::*;
use crate::generator::*;
use std::io::Write;

pub enum Instruction {
    Directive(String),
    Push(i32),
    Return,
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
                let value = expression.accept(self);
                self.ir.push(Instruction::Push(value));
                self.ir.push(Instruction::Return);
            }
        };
    }

    pub fn visit_expression(&mut self, expression: &Expression) -> i32 {
        match expression {
            Expression::IntegerLiteral(value) => value.clone(),
        }
    }

    pub fn generate(&self, output: &mut impl Write) {
        for instruction in self.ir.iter() {
            generate_instruction(instruction, output);
        }
    }
}
