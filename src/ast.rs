use super::token::Token;

pub trait Node<'a> {
    fn token_literal(&'a self) -> &'a str;
}

pub trait Statement<'a>: Node<'a> {
    fn statement_node(&self);
}

pub trait Expression<'a>: Node<'a> {
    fn expression_nod(&self);
}

struct Program<'a> {
    statements: Vec<&'a Statement<'a>>,
}

impl<'a> Node<'a> for Program<'a> {
    fn token_literal(&self) -> &'a str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

struct LetStatement<'a, E>
    where E: Expression<'a>
{
    token: Token,
    name: &'a Identifier,
    value: E,
}

impl<'a, E> Node<'a> for LetStatement<'a, E>
    where E: Expression<'a>
{
    fn token_literal(&'a self) -> &'a str {
        &self.token.literal
    }
}

impl<'a, E> Statement<'a> for LetStatement<'a, E>
    where E: Expression<'a>
{
    fn statement_node(&self) {}
}

struct Identifier {
    token: Token,
    value: String,
}

impl<'a> Node<'a> for Identifier {
    fn token_literal(&'a self) -> &'a str {
        &self.token.literal
    }
}
