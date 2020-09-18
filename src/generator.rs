use crate::ir::Instruction;
use std::io::Write;

pub fn generate_instruction(
    ir: &Instruction,
    output: &mut impl Write,
) -> Result<(), std::io::Error> {
    macro_rules! write_instructions {
        ($($instruction: expr),+) => {{
            $(writeln!(output, $instruction)?;)+
        }}
    }

    macro_rules! write_binary_operator_instructions {
        ($($instruction: expr),+) => {
            write_instructions!("lw t1, 4(sp)", "lw t2, 0(sp)", $($instruction),+, "sw t1, 4(sp)", "addi sp, sp, 4");
        }
    }

    match ir {
        Instruction::Directive(text) => {
            writeln!(output, "{}", text)?;
        }
        Instruction::Push(value) => {
            write_instructions!("addi sp, sp, -4");
            writeln!(output, "li t1, {}", value)?;
            write_instructions!("sw t1, 0(sp)");
        }
        Instruction::Pop => {
            write_instructions!("addi sp, sp, 4");
        }
        Instruction::Return => {
            write_instructions!("lw a0, 0(sp)", "addi sp, sp, 4", "jr ra");
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
            write_binary_operator_instructions!("snez t1, t1", "snez t2, t2", "and t1, t1, t2");
        }
        Instruction::LogicalOr => {
            write_binary_operator_instructions!("or t1, t1, t2", "snez t1, t1");
        }
        Instruction::Load => {
            write_instructions!("lw t1, 0(sp)", "lw t1, 0(t1)", "sw t1, 0(sp)");
        }
        Instruction::Store => {
            write_instructions!(
                "lw t1, 4(sp)",
                "lw t2, 0(sp)",
                "addi sp, sp, 4",
                "sw t1, 0(t2)"
            );
        }
        Instruction::FrameAddress(address) => {
            write_instructions!("addi sp, sp, -4");
            writeln!(output, "addi t1, fp, -{}", address)?;
            write_instructions!("sw t1, 0(sp)");
        }
    };
    Ok(())
}
