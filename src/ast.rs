use super::token::Token;

pub trait Node<'a> {
    fn token_literal(&'a self) -> &'a str;
}

pub enum Statement<'a> {
    Let {
        token: Token,
        name: &'a Identifier,
        value: Expression,
    },
    Empty,
}

impl<'a> Node<'a> for Statement<'a> {
    fn token_literal(&self) -> &str {
        match *self {
            Statement::Let { ref token, .. } => &token.literal,
            ref Empty => "",
        }
    }
}

pub enum Expression {}

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    pub fn new() -> Self {
        Program { statements: Vec::new() }
    }
}

impl<'a> Node<'a> for Program<'a> {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    token: Token,
    pub value: String,
}

impl<'a> Node<'a> for Identifier {
    fn token_literal(&'a self) -> &'a str {
        &self.token.literal
    }
}
