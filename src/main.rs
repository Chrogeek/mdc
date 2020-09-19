mod ast;
mod context;
mod lexer;
mod parser;
mod util;

use crate::ast::Ast;
use crate::context::Context;
use crate::parser::Parser;

fn main() {
    let file = std::env::args()
        .nth(1)
        .expect("Usage: minidecaf <source file>");
    let input = std::fs::read(&file).expect(&format!("Unable to read input source file {}", &file));
    let mut output = Vec::<u8>::new();
    let mut context = Context::new(&mut output);
    Parser::new(input.as_slice())
        .parse_program()
        .emit(&mut context);
    print!("{}", String::from_utf8(output).unwrap());
}
