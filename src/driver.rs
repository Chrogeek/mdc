use crate::ast::Ast;
use crate::ir::Context;
use crate::parser::*;
use std::io::Write;

pub fn run(input: &[u8], output: &mut impl Write) -> Result<(), std::io::Error> {
    let mut parser = Parser::new(input);
    let mut context = Context::new();
    parser.parse_program().emit(&mut context);
    context.assemble(output)
}
