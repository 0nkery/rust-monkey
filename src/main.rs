enum TokenType {
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

struct Token {
    token_type: TokenType,
    literal: String,
}


fn main() {
    println!("Hello, world!");
}
