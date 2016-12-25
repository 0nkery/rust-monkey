use super::lexer::Lexer;
use super::token::Token;
use super::token::TokenType;
use super::ast::Program;
use super::ast::Node;
use super::ast::Statement;
use super::ast::Identifier;


struct Parser<'a> {
    lexer: &'a mut Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    fn new(lexer: &'a mut Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer: lexer,
            cur_token: cur_token,
            peek_token: peek_token,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_stmt(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_stmt(),
            _ => None,
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = Identifier::new(self.cur_token.clone(), self.cur_token.literal.clone());

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        while self.cur_token.token_type != TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Let {
            token: let_token,
            name: name,
            value: unsafe { ::std::mem::uninitialized() },
        })
    }

    fn expect_peek(&mut self, tt: TokenType) -> bool {
        if self.peek_token.token_type == tt {
            self.next_token();
            true
        } else {
            self.peek_error(tt);
            false
        }
    }

    fn peek_error(&mut self, tt: TokenType) {
        let err_msg = format!("Expected next token to be {:?}, got {:?} instead",
                              tt,
                              self.peek_token.token_type);
        self.errors.push(err_msg);
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.token_type != TokenType::EOF {
            let maybe_stmt = self.parse_stmt();
            if let Some(stmt) = maybe_stmt {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
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
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();
    check_parser_errors(&p);

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

#[cfg(test)]
fn check_parser_errors<'a>(parser: &'a Parser) {
    if parser.errors.len() == 0 {
        return;
    }

    for err in &parser.errors {
        println!("{}", err);
    }
    panic!("There are parser errors!");
}
