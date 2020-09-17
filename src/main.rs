mod ast;
mod driver;
mod error;
mod generator;
mod ir;
mod lexer;
mod parser;
mod token;

fn main() {
    let file = std::env::args()
        .nth(1)
        .expect("Usage: minidecaf <source file>");
    let input = std::fs::read(&file).expect(&format!("Unable to read input source file {}", &file));
    driver::run(input.as_slice(), &mut std::io::stdout());
}
