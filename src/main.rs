extern crate users;

mod token;
mod lexer;
mod repl;
mod ast;
mod parser;
mod object;

use std::io::stdin;
use std::io::stdout;

use users::get_user_by_uid;
use users::get_current_uid;

use repl::start;

fn main() {
    let user = get_user_by_uid(get_current_uid()).unwrap();
    println!("Hello {}! This is the Monkey programming language!", user.name());
    println!("Feel free to type in commands");

    start(stdin(), stdout());
}
