use crate::ir::Instruction;
use std::io::Write;

pub fn generate_instruction(
    ir: &Instruction,
    output: &mut impl Write,
) -> Result<(), std::io::Error> {
    match ir {
        Instruction::Directive(text) => {
            writeln!(output, "{}", text)?;
        }
        Instruction::Push(value) => {
            writeln!(output, "addi sp, sp, -4")?;
            writeln!(output, "li t1, {}", value)?;
            writeln!(output, "sw t1, 0(sp)")?;
        }
        Instruction::Return => {
            writeln!(output, "lw a0, 0(sp)")?;
            writeln!(output, "addi sp, sp, 4")?;
            writeln!(output, "jr ra")?;
        }
        Instruction::Negate => {
            writeln!(output, "lw t1, 0(sp)")?;
            writeln!(output, "neg t1, t1")?;
            writeln!(output, "sw t1, 0(sp)")?;
        }
        Instruction::Not => {
            writeln!(output, "lw t1, 0(sp)")?;
            writeln!(output, "not t1, t1")?;
            writeln!(output, "sw t1, 0(sp)")?;
        }
        Instruction::LogicalNot => {
            writeln!(output, "lw t1, 0(sp)")?;
            writeln!(output, "seqz t1, t1")?;
            writeln!(output, "sw t1, 0(sp)")?;
        }
    };
    Ok(())
}
