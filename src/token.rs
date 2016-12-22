#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EOF,
    // Identifiers + literals.
    Ident,
    Int,
    // Operators.
    Assign,
    Plus,
    // Delimiters.
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    // Keywords.
    Function,
    Let,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(tt: TokenType, literal: String) -> Self {
        Token {
            token_type: tt,
            literal: literal,
        }
    }
}
