use std::io::Read;
use std::io::Write;

use super::lexer::Lexer;
use super::token::TokenType;


const PROMPT: &'static str = ">> ";


pub fn start<R, W>(mut in_: R, mut out: W)
    where R: Read,
          W: Write
{
    loop {
        print!("{}", PROMPT);
        out.flush().unwrap();

        let mut s = String::new();
        in_.read_to_string(&mut s).expect("Did not enter a correct string");

        let mut l = Lexer::new(s);
        let mut tok = l.next_token();
        while tok.token_type != TokenType::EOF {
            write!(&mut out, "{:?}\n", tok).unwrap();
            out.flush().unwrap();
            tok = l.next_token();
        }
    }
}
