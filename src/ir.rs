use crate::generator::*;
use crate::util::*;
use std::collections::HashMap;
use std::io::Write;

pub enum Instruction {
    Directive(String),
    Push(i32),
    Pop,
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
    Load,
    Store,
    FrameAddress(usize),
}

pub struct Context {
    pub ir: Vec<Instruction>,
    variables: HashMap<String, Variable>,
    stack_position: usize,
}

impl Context {
    pub fn new() -> Context {
        Context {
            ir: Vec::new(),
            variables: HashMap::new(),
            stack_position: 8,
        }
    }

    pub fn assemble(&self, output: &mut impl Write) -> Result<(), std::io::Error> {
        for instruction in self.ir.iter() {
            generate_instruction(instruction, output)?;
        }
        Ok(())
    }

    pub fn create_variable(&mut self, r#type: &Type, name: &String) -> &Variable {
        self.stack_position += r#type.measure();
        if let None = self.variables.insert(
            name.clone(),
            Variable {
                r#type: r#type.clone(),
                offset: self.stack_position,
            },
        ) {
            self.variables.get(name).unwrap()
        } else {
            panic!("Redefinition of variable {}", name);
        }
    }

    pub fn access_variable(&self, name: &String) -> &Variable {
        self.variables.get(name).unwrap()
    }
}
