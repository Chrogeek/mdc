mod ast;
mod context;
mod driver;
mod lexer;
mod parser;
mod util;

fn main() -> Result<(), std::io::Error> {
    let file = std::env::args()
        .nth(1)
        .expect("Usage: minidecaf <source file>");
    let input = std::fs::read(&file).expect(&format!("Unable to read input source file {}", &file));
    driver::run(input.as_slice(), &mut std::io::stdout())
}
