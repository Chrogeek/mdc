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
    Equal,
    Unequal,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    LogicalAnd,
    LogicalOr,
}

pub struct Context {
    pub ir: Vec<Instruction>,
}

impl Context {
    pub fn new() -> Context {
        Context { ir: Vec::new() }
    }

    pub fn assemble(&self, output: &mut impl Write) -> Result<(), std::io::Error> {
        for instruction in self.ir.iter() {
            generate_instruction(instruction, output)?;
        }
        Ok(())
    }
}
