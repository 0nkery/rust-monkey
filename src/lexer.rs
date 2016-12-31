use super::token::TokenType;
use super::token::Token;


pub struct Lexer {
    input: String,
    // Current position in input.
    position: usize,
    // Current reading position.
    read_position: usize,
    // Current char under examination.
    ch: char,
}

const EOF: char = '\0';

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: EOF,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = EOF;
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            return EOF;
        } else {
            return self.input.chars().nth(self.read_position).unwrap();
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_' || ch == '?'
    }

    fn read_ident(&mut self) -> String {
        let position = self.position;
        while self.is_letter(self.ch) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == '"' {
                break;
            }
        }

        self.input[position..self.position].to_string()
    }

    pub fn next_token(&mut self) -> Token {
        while self.ch.is_whitespace() {
            self.read_char();
        }

        let tok = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==".to_string())
                } else {
                    Token::new(TokenType::Assign, self.ch.to_string())
                }
            }
            ';' => Token::new(TokenType::Semicolon, self.ch.to_string()),
            ':' => Token::new(TokenType::Colon, self.ch.to_string()),
            '(' => Token::new(TokenType::LeftParen, self.ch.to_string()),
            ')' => Token::new(TokenType::RightParen, self.ch.to_string()),
            ',' => Token::new(TokenType::Comma, self.ch.to_string()),
            '+' => Token::new(TokenType::Plus, self.ch.to_string()),
            '{' => Token::new(TokenType::LeftBrace, self.ch.to_string()),
            '}' => Token::new(TokenType::RightBrace, self.ch.to_string()),
            '[' => Token::new(TokenType::LeftBracket, self.ch.to_string()),
            ']' => Token::new(TokenType::RightBracket, self.ch.to_string()),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=".to_string())
                } else {
                    Token::new(TokenType::Bang, self.ch.to_string())
                }
            }
            '-' => Token::new(TokenType::Minus, self.ch.to_string()),
            '/' => Token::new(TokenType::Slash, self.ch.to_string()),
            '*' => Token::new(TokenType::Asterisk, self.ch.to_string()),
            '<' => Token::new(TokenType::LessThan, self.ch.to_string()),
            '>' => Token::new(TokenType::GreaterThan, self.ch.to_string()),
            EOF => Token::new(TokenType::EOF, "".to_string()),
            ch @ _ if self.is_letter(ch) => {
                let ident = self.read_ident();
                let tt = TokenType::lookup_ident(&ident);
                return Token::new(tt, ident);
            }
            ch @ _ if ch.is_digit(10) => {
                let literal = self.read_number();
                return Token::new(TokenType::Int, literal);
            }
            '"' => Token::new(TokenType::String, self.read_string()),
            _ => Token::new(TokenType::Illegal, self.ch.to_string()),
        };

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
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        \"foobar\"
        \"foo bar\"
        [1, 2];
        {\"foo\": \"bar\"}
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
                     (TokenType::Bang, "!"),
                     (TokenType::Minus, "-"),
                     (TokenType::Slash, "/"),
                     (TokenType::Asterisk, "*"),
                     (TokenType::Int, "5"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::Int, "5"),
                     (TokenType::LessThan, "<"),
                     (TokenType::Int, "10"),
                     (TokenType::GreaterThan, ">"),
                     (TokenType::Int, "5"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::If, "if"),
                     (TokenType::LeftParen, "("),
                     (TokenType::Int, "5"),
                     (TokenType::LessThan, "<"),
                     (TokenType::Int, "10"),
                     (TokenType::RightParen, ")"),
                     (TokenType::LeftBrace, "{"),
                     (TokenType::Return, "return"),
                     (TokenType::True, "true"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::RightBrace, "}"),
                     (TokenType::Else, "else"),
                     (TokenType::LeftBrace, "{"),
                     (TokenType::Return, "return"),
                     (TokenType::False, "false"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::RightBrace, "}"),
                     (TokenType::Int, "10"),
                     (TokenType::Eq, "=="),
                     (TokenType::Int, "10"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::Int, "10"),
                     (TokenType::NotEq, "!="),
                     (TokenType::Int, "9"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::String, "foobar"),
                     (TokenType::String, "foo bar"),
                     (TokenType::LeftBracket, "["),
                     (TokenType::Int, "1"),
                     (TokenType::Comma, ","),
                     (TokenType::Int, "2"),
                     (TokenType::RightBracket, "]"),
                     (TokenType::Semicolon, ";"),
                     (TokenType::LeftBrace, "{"),
                     (TokenType::String, "foo"),
                     (TokenType::Colon, ":"),
                     (TokenType::String, "bar"),
                     (TokenType::RightBrace, "}"),
                     (TokenType::EOF, "")];

    for (i, &(ref expected_type, ref expected_literal)) in tests.iter().enumerate() {
        let tok = lexer.next_token();

        assert!(tok.token_type == *expected_type,
                "Wrong TokenType [{}]. Expected - {:?}, got - {:?}.",
                i,
                expected_type,
                tok.token_type);

        assert!(tok.literal == *expected_literal,
                "Wrong literal [{}]. Expected - {:?}, got - {:?}.",
                i,
                expected_literal,
                tok.literal);
    }
}
