use std::io::Stdin;
use std::io::Stdout;
use std::io::Write;

use super::lexer::Lexer;
use super::parser::Parser;
use super::ast::Node;


const PROMPT: &'static str = ">> ";


pub fn start(in_: Stdin, mut out: Stdout) {
    loop {
        print!("{}", PROMPT);
        out.flush().unwrap();

        let mut s = String::new();
        in_.read_line(&mut s).expect("Did not enter a correct string");

        let mut l = Lexer::new(s);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        if p.errors().len() != 0 {
            print_parser_errors(&mut out, p.errors());
            continue;
        }

        write!(&mut out, "{}\n", program.string()).expect("Failed to write to stdout");
    }
}

fn print_parser_errors(out: &mut Stdout, errors: &[String]) {
    for err in errors {
        write!(out, "{}\n", err).expect("Failed to write to stdout");
        out.flush().unwrap();
    }
}
