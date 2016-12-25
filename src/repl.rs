use std::io::Stdin;
use std::io::Stdout;
use std::io::Write;

use super::lexer::Lexer;
use super::token::TokenType;


const PROMPT: &'static str = ">> ";


pub fn start(mut in_: Stdin, mut out: Stdout) {
    loop {
        print!("{}", PROMPT);
        out.flush().unwrap();

        let mut s = String::new();
        in_.read_line(&mut s).expect("Did not enter a correct string");

        let mut l = Lexer::new(s);
        let mut tok = l.next_token();
        while tok.token_type != TokenType::EOF {
            write!(&mut out, "{:?}\n", tok).unwrap();
            out.flush().unwrap();
            tok = l.next_token();
        }
    }
}
