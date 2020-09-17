use crate::ast::Ast;
use crate::ir::Context;
use crate::parser::*;
use std::io::Write;

pub fn run(input: &[u8], output: &mut impl Write) {
    let mut parser = Parser::new(input);

    let program = parser.parse_program();

    let mut context = Context::new();

    program.accept(&mut context);

    context.generate(output);

    // writeln!(output, "{:?}", program).unwrap();
}
