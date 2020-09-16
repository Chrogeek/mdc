use crate::lexer::get_token;
use crate::token::TokenKind;

pub fn run(mut input: &[u8], output: &mut impl std::io::Write) {
    let (mut row, mut col) = (0u32, 0u32);
    loop {
        let token = get_token(input, row, col);
        if token.is_err() {
            writeln!(output, "{:?}", token.unwrap_err()).unwrap();
            break;
        } else {
            let token = token.unwrap();
            writeln!(
                output,
                "Token: {:?}, row = {}, col = {}",
                token.1, token.2, token.3
            )
            .unwrap();
            input = token.0;
            row = token.2;
            col = token.3;
            if let TokenKind::Eof = token.1.kind {
                break;
            }
        }
    }
}
