use std::io::Stdin;
use std::io::Stdout;
use std::io::Write;

use super::lexer::Lexer;
use super::parser::Parser;
use super::eval::Eval;
use super::object::Env;


const PROMPT: &'static str = ">> ";


pub fn start(in_: Stdin, mut out: Stdout) {
    let mut env = Env::new();

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

        let eval = Eval::new(program, env);
        let evaluated = eval.eval();
        env = eval.env();

        write!(&mut out, "{}\n", evaluated.inspect()).expect("Failed to write to stdout");
    }
}

fn print_parser_errors(out: &mut Stdout, errors: &[String]) {
    for err in errors {
        write!(out, "{}\n", err).expect("Failed to write to stdout");
        out.flush().unwrap();
    }
}
