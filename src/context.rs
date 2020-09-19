use crate::util::*;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::Write;

#[derive(Debug)]
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
    Locate(String),
    Allocate(usize, String),
    SetFrame(String),
    EndFrame,
    JumpOnZero(String),
    Jump(String),
    Label(String),
}

pub struct Context {
    pub ir: Vec<Instruction>,
    variables: HashSet<String>,
    current_function: Option<String>,
    label_count: usize,
}

impl Context {
    pub fn new() -> Context {
        Context {
            ir: Vec::new(),
            variables: HashSet::new(),
            current_function: None,
            label_count: 0,
        }
    }

    pub fn create_variable(&mut self, r#type: &Type, name: &String) {
        if self.variables.insert(name.clone()) {
            self.ir
                .push(Instruction::Allocate(r#type.measure(), name.clone()));
        } else {
            panic!("Redefinition of variable {}", name);
        }
    }

    pub fn access_variable(&mut self, name: &String) {
        if self.variables.contains(name) {
            self.ir.push(Instruction::Locate(name.clone()));
        } else {
            panic!("Undefined variable: {}", name);
        }
    }

    pub fn enter_function(&mut self, function_name: &String) {
        self.current_function = Some(function_name.clone());
        self.ir.push(Instruction::Directive(format!(
            ".globl {0}\n{0}:",
            mangle_function_name(function_name)
        )));
    }

    pub fn exit_function(&mut self) {
        self.ir.push(Instruction::Directive(format!(
            "{0}:",
            get_function_epilogue(&self.current_function.clone().unwrap())
        )));
        self.current_function = None;
    }

    pub fn next_label(&mut self) -> String {
        self.label_count += 1;
        "_label_".to_string() + &self.label_count.to_string()
    }

    pub fn assemble(&mut self, output: &mut impl Write) -> Result<(), std::io::Error> {
        let mut offset: usize = 0;
        let mut lookup = HashMap::<String, usize>::new();

        macro_rules! write_instructions {
            ($($instruction: expr),+) => {{
                $(writeln!(output, $instruction)?;)+
            }}
        }

        macro_rules! write_binary_operator_instructions {
            ($($instruction: expr),+) => {
                offset -= 4;
                write_instructions!("lw t1, 4(sp)", "lw t2, 0(sp)", $($instruction),+, "addi sp, sp, 4", "sw t1, 0(sp)");
            }
        }

        for ir in self.ir.iter() {
            match ir {
                Instruction::Directive(text) => {
                    writeln!(output, "{}", text)?;
                }
                Instruction::Push(value) => {
                    offset += 4;
                    write_instructions!("addi sp, sp, -4");
                    writeln!(output, "li t1, {}", value)?;
                    write_instructions!("sw t1, 0(sp)");
                }
                Instruction::Pop => {
                    offset -= 4;
                    write_instructions!("addi sp, sp, 4");
                }
                Instruction::Return => {
                    offset -= 4;
                    write_instructions!("lw a0, 0(sp)", "addi sp, sp, 4");
                    writeln!(
                        output,
                        "j {}",
                        get_function_epilogue(&self.current_function.clone().unwrap())
                    )?;
                }
                Instruction::Negate => {
                    write_instructions!("lw t1, 0(sp)", "neg t1, t1", "sw t1, 0(sp)");
                }
                Instruction::Not => {
                    write_instructions!("lw t1, 0(sp)", "not t1, t1", "sw t1, 0(sp)");
                }
                Instruction::LogicalNot => {
                    write_instructions!("lw t1, 0(sp)", "seqz t1, t1", "sw t1, 0(sp)");
                }
                Instruction::Add => {
                    write_binary_operator_instructions!("add t1, t1, t2");
                }
                Instruction::Subtract => {
                    write_binary_operator_instructions!("sub t1, t1, t2");
                }
                Instruction::Multiply => {
                    write_binary_operator_instructions!("mul t1, t1, t2");
                }
                Instruction::Divide => {
                    write_binary_operator_instructions!("div t1, t1, t2");
                }
                Instruction::Modulo => {
                    write_binary_operator_instructions!("rem t1, t1, t2");
                }
                Instruction::Equal => {
                    write_binary_operator_instructions!("sub t1, t1, t2", "seqz t1, t1");
                }
                Instruction::Unequal => {
                    write_binary_operator_instructions!("sub t1, t1, t2", "snez t1, t1");
                }
                Instruction::Less => {
                    write_binary_operator_instructions!("slt t1, t1, t2");
                }
                Instruction::LessEqual => {
                    write_binary_operator_instructions!("slt t1, t2, t1", "seqz t1, t1");
                }
                Instruction::Greater => {
                    write_binary_operator_instructions!("slt t1, t2, t1");
                }
                Instruction::GreaterEqual => {
                    write_binary_operator_instructions!("slt t1, t1, t2", "seqz t1, t1");
                }
                Instruction::LogicalAnd => {
                    write_binary_operator_instructions!(
                        "snez t1, t1",
                        "snez t2, t2",
                        "and t1, t1, t2"
                    );
                }
                Instruction::LogicalOr => {
                    write_binary_operator_instructions!("or t1, t1, t2", "snez t1, t1");
                }
                Instruction::Load => {
                    write_instructions!("lw t1, 0(sp)", "lw t1, 0(t1)", "sw t1, 0(sp)");
                }
                Instruction::Store => {
                    offset -= 4;
                    write_instructions!(
                        "lw t1, 4(sp)",
                        "lw t2, 0(sp)",
                        "addi sp, sp, 4",
                        "sw t1, 0(t2)"
                    );
                }
                Instruction::Allocate(size, name) => {
                    offset += size;
                    lookup.insert(name.clone(), offset);
                    writeln!(output, "addi sp, sp, -{}", size)?;
                }
                Instruction::Locate(name) => {
                    offset += 4;
                    write_instructions!("addi sp, sp, -4");
                    writeln!(output, "addi t1, fp, -{}", lookup.get(name).unwrap())?;
                    write_instructions!("sw t1, 0(sp)");
                }
                Instruction::SetFrame(name) => {
                    write_instructions!("addi sp, sp, -4", "sw fp, 0(sp)"); // save fp
                    offset = 0;
                    write_instructions!("addi fp, sp, 0"); // start new stack frame
                    self.current_function = Some(name.clone());
                }
                Instruction::EndFrame => {
                    offset = 0;
                    write_instructions!("addi sp, fp, 0"); // free local variables
                    write_instructions!("lw t1, 0(sp)", "addi sp, sp, 4", "addi fp, t1, 0");
                    // restore fp
                    write_instructions!("jr ra"); // jump return
                    self.current_function = None;
                }
                Instruction::JumpOnZero(label) => {
                    offset -= 4;
                    write_instructions!("lw t1, 0(sp)", "addi sp, sp, 4");
                    writeln!(output, "beqz t1, {}", label)?;
                }
                Instruction::Jump(label) => {
                    writeln!(output, "j {}", label)?;
                }
                Instruction::Label(label) => {
                    writeln!(output, "{}:", label)?;
                }
            };
        }
        Ok(())
    }
}
