mod ast;
mod driver;
mod error;
mod generator;
mod lexer;
mod parser;
mod token;

fn main() {
    let file = std::env::args()
        .nth(1)
        .expect("Usage: minidecaf <source file>");
    let input = std::fs::read(file).unwrap();
    driver::run(input.as_slice(), &mut std::io::stdout());
}
