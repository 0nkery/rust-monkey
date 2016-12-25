use super::lexer::Lexer;
use super::token::Token;
use super::ast;


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
}
