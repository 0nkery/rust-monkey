use super::token::TokenType;
use super::token::Token;


struct Lexer {
    input: String,
    // Current position in input.
    position: usize,
    // Current reading position.
    read_position: usize,
    // Current char under examination.
    ch: char,
}

impl Lexer {
    fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: '\\',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\\';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn next_token(&mut self) -> Token {
        let tt = match self.ch {
            '=' => TokenType::Assign,
            ';' => TokenType::Semicolon,
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            ',' => TokenType::Comma,
            '+' => TokenType::Plus,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            '\\' => TokenType::EOF,
            _ => TokenType::Illegal,
        };
        let tok = Token::new(tt, self.ch.to_string());

        self.read_char();

        tok
    }
}


#[test]
fn test_next_token() {
    let input = String::from("let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
    ");
    let mut lexer = Lexer::new(input);

    let tests = vec![(TokenType::Let, "let"),
                     (TokenType::Ident, "five"),
                     (TokenType::Assign, "="),
                     (TokenType::Int, "5"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::Let, "let"),
                     (TokenType::Ident, "ten"),
                     (TokenType::Assign, "="),
                     (TokenType::Int, "10"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::Let, "let"),
                     (TokenType::Ident, "add"),
                     (TokenType::Assign, "="),
                     (TokenType::Function, "fn"),
                     (TokenType::LeftParen, "("),
                     (TokenType::Ident, "x"),
                     (TokenType::Comma, ","),
                     (TokenType::Ident, "y"),
                     (TokenType::RightParen, ")"),
                     (TokenType::LeftBrace, "{"),
                     (TokenType::Ident, "x"),
                     (TokenType::Plus, "+"),
                     (TokenType::Ident, "y"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::RightBrace, "}"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::Let, "let"),
                     (TokenType::Ident, "result"),
                     (TokenType::Assign, "="),
                     (TokenType::Ident, "add"),
                     (TokenType::LeftParen, "("),
                     (TokenType::Ident, "five"),
                     (TokenType::Comma, ","),
                     (TokenType::Ident, "ten"),
                     (TokenType::RightParen, ")"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::EOF, "")];

    for (expected_type, expected_literal) in tests {
        let tok = lexer.next_token();

        assert!(tok.token_type == expected_type,
                "Wrong TokenType. Expected - {:?}, got - {:?}.",
                expected_type,
                tok.token_type);

        assert!(tok.literal == expected_literal,
                "Wrong literal. Expected - {:?}, got - {:?}.",
                expected_literal,
                tok.literal);
    }
}
