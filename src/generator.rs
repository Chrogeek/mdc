use crate::ir::Instruction;
use std::io::Write;

pub fn generate_instruction(ir: &Instruction, output: &mut impl Write) {
    match ir {
        Instruction::Directive(text) => {
            writeln!(output, "{}", text).unwrap();
        }
        Instruction::Push(value) => {
            writeln!(output, "addi sp, sp, -4").unwrap();
            writeln!(output, "li t1, {}", value).unwrap();
            writeln!(output, "sw t1, 0(sp)").unwrap();
        }
        Instruction::Return => {
            writeln!(output, "lw a0, 0(sp)").unwrap();
            writeln!(output, "addi sp, sp, 4").unwrap();
            writeln!(output, "jr ra").unwrap();
        }
    }
}
