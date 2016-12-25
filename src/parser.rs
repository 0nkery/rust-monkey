use super::lexer::Lexer;
use super::token::Token;
use super::ast::Program;
use super::ast::Node;
use super::ast::Statement;


struct Parser<'a> {
    lexer: &'a mut Lexer,
    cur_token: Option<Token>,
    peek_token: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer) -> Self {
        let mut p = Parser {
            lexer: lexer,
            cur_token: None,
            peek_token: None,
        };

        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = Some(self.lexer.next_token());
    }

    fn parse_program(&self) -> Program {
        Program::new()
    }
}


#[test]
fn test_let_statements() {
    let input = String::from("
let x = 5;
let y = 10;
let foobar = 838383;
    ");

    let mut l = Lexer::new(input);
    let p = Parser::new(&mut l);

    let program = p.parse_program();
    assert!(program.statements.len() == 3,
            "program.statements does not contain 3 statements. Got {}",
            program.statements.len());

    let tests = vec!["x", "y", "foobar"];

    for ((i, expected), stmt) in tests.iter().enumerate().zip(program.statements) {
        assert!(stmt.token_literal() == "let",
                "[{}] stmt.token_literal not 'let'. Got {}",
                i,
                stmt.token_literal());

        if let Statement::Let { name, .. } = stmt {
            assert!(name.value == *expected,
                    "stmt.name.value is not {}. Got {}",
                    expected,
                    name.value);

            assert!(name.token_literal() == *expected,
                    "stmt.name is not {}. Got {:?}",
                    expected,
                    name);
        } else {
            panic!("stmt is not Statement::Let");
        }
    }
}
