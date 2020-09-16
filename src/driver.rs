use crate::parser::*;
use std::io::Write;

pub fn run(input: &[u8], output: &mut impl Write) {
    let mut parser = Parser::new(input);

    let program = parser.parse_program();

    // writeln!(output, "{:?}", program).unwrap();
}
