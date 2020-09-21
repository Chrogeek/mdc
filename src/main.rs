mod ast;
mod context;
mod lexer;
mod parser;
mod util;

use crate::ast::Ast;
use crate::context::Context;
use crate::parser::Parser;

fn main() {
    let file = std::env::args().nth(1).unwrap();
    let input = std::fs::read(&file).unwrap();
    let (mut text, mut data, mut bss) = (Vec::<u8>::new(), Vec::<u8>::new(), Vec::<u8>::new());
    let mut context = Context::new(&mut text, &mut data, &mut bss);
    Parser::new(input.as_slice())
        .parse_program()
        .emit(&mut context);
    print!("{}", String::from_utf8(data).unwrap());
    print!("{}", String::from_utf8(bss).unwrap());
    print!("{}", String::from_utf8(text).unwrap());
}
