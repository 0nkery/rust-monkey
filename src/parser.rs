use super::lexer::Lexer;
use super::token::Token;
use super::token::TokenType;
use super::ast::Program;
use super::ast::Node;
use super::ast::Statement;
use super::ast::Expression;


enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call
}


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
            TokenType::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Statement> {
        let let_token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone()
        };

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

    fn parse_return_stmt(&mut self) -> Option<Statement> {
        let return_token = self.cur_token.clone();

        self.next_token();

        while self.cur_token.token_type != TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Return {
            token: return_token,
            value: unsafe { ::std::mem::uninitialized() },
        })
    }

    fn parse_expr_stmt(&mut self) -> Option<Statement> {
        let expr_token = self.cur_token.clone();
        let expr = self.parse_expr(Precedence::Lowest)
                       .expect("Unable to parse expression");

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Expression {
            token: expr_token,
            expression: expr
        })
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expression> {
        self.parse_prefix()
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match self.cur_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expr(),
            ref tt @ _ => {
                let err_msg = format!("No prefix parse fn for {:?} found.", tt);
                self.errors.push(err_msg);

                None
            }
        }
    }

    fn parse_identifier(&self) -> Option<Expression> {
        Some(Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone()
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let literal_token = self.cur_token.clone();
        let parse_result = self.cur_token.literal.parse();

        match parse_result {
            Ok(value) => Some(Expression::IntegerLiteral {
                token: literal_token,
                value: value
            }),
            Err(_) => {
                let msg = format!("Could not parse {} as integer",
                                  literal_token.literal);
                self.errors.push(msg);

                None
            }
        }
    }

    fn parse_prefix_expr(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();

        self.next_token();

        let right = self.parse_expr(Precedence::Prefix);

        if let Some(right) = right {
            Some(Expression::Prefix {
                token: token,
                operator: operator,
                right: Box::new(right)
            })
        } else {
            None
        }
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

#[cfg(test)]
fn check_integer_literal(il: &Expression, test_value: i64) {
    match *il {
        Expression::IntegerLiteral { ref value, .. } => {
            assert!(*value == test_value,
                    "il.value is not {}. Got {}",
                    test_value,
                    value);
            assert!(il.token_literal() == test_value.to_string(),
                    "il.token_literal() is not {}. Got {}",
                    test_value,
                    il.token_literal());
        },
        _ => panic!("il is not Expression::IntegerLiteral. Got {:?}", il)
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
            if let Expression::Identifier { ref value, .. } = name {
                assert!(value == *expected,
                        "stmt.name.value is not {}. Got {}",
                        expected,
                        value);

                assert!(name.token_literal() == *expected,
                        "stmt.name is not {}. Got {:?}",
                        expected,
                        name);
            } else {
                panic!("name is not Expression::Identifier. Got {:?}", name);
            }
        } else {
            panic!("stmt is not Statement::Let. Got {:?}", stmt);
        }
    }
}

#[test]
fn test_return_statements() {
    let input = String::from("
return 5;
return 10;
return 993322;
    ");

    let mut l = Lexer::new(input);
    let mut p = Parser::new(&mut l);

    let program = p.parse_program();
    check_parser_errors(&p);

    assert!(program.statements.len() == 3,
            "program.statements does not contain 3 statements. Got {}",
            program.statements.len());

    for stmt in program.statements {
        if let Statement::Return { .. } = stmt {
            assert!(stmt.token_literal() == "return",
                    "stmt.token_literal() isn't 'return'. Got {}",
                    stmt.token_literal());
        } else {
            panic!("stmt is not Statement::Return. Got {:?}", stmt);
        }
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let mut l = Lexer::new(input.to_string());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();
    check_parser_errors(&p);

    assert!(program.statements.len() == 1,
            "program has not enough statements. Got {}",
            program.statements.len());

    if let Statement::Expression { ref expression, .. } = program.statements[0] {
        if let Expression::Identifier { ref value, .. } = *expression {
            assert!(value == "foobar", "value not 'foobar'. Got '{}'", value);
            assert!(expression.token_literal() == "foobar",
                    "token_literal is not 'foobar'. Got {}",
                    expression.token_literal());
        } else {
            panic!("expression is not Expression::Identifier. Got {:?}",
                   expression);
        }
    } else {
        panic!("program.statements[0] is not Statement::Expression. Got {:?}",
               program.statements[0]);
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";

    let mut l = Lexer::new(input.to_string());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();
    check_parser_errors(&p);

    assert!(program.statements.len() == 1,
            "program has not enough statements. Got {}",
            program.statements.len());

    if let Statement::Expression { ref expression, .. } = program.statements[0] {
        if let Expression::IntegerLiteral { ref value, .. } = *expression {
            assert!(*value == 5, "value is not 5. Got {}", value);
            assert!(expression.token_literal() == "5",
                    "token_literal() is not '5'. Got {}",
                    expression.token_literal());
        } else {
            panic!("expression is not Expression::IntegerLiteral. Got {:?}",
                   expression);
        }
    } else {
        panic!("program.statements[0] is not Statement::Expression. Got {:?}",
               program.statements[0]);
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    let tests = vec![
        ("!5", "!", 5),
        ("-15", "-", 15)
    ];

    for (input, op, value) in tests {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert!(program.statements.len() == 1,
                "program.statements does not contain 1 statement. Got {}",
                program.statements.len());

        if let Statement::Expression { ref expression, .. } = program.statements[0] {
            if let Expression::Prefix { ref operator, .. } = *expression {
                assert!(operator == op,
                        "operator is not {}. Got {}",
                        op,
                        operator);
                check_integer_literal(expression, value);

            } else {
                panic!("expression is not Expression::Prefix. Got {:?}",
                       expression);
            }
        } else {
            panic!("program.statements[0] is not Statement::Expression. Got {:?}",
                   program.statements[0]);
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let tests = vec![
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5)
    ];

    for (input, left_val, op, right_val) in tests {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert!(program.statements.len() == 1,
                "program.statements does not contain 1 statement. Got {}",
                program.statements.len());

        if let Statement::Expression { ref expression, .. } = program.statements[0] {
            if let Expression::Infix {
                ref left,
                ref operator,
                ref right, ..
            } = *expression {

                check_integer_literal(left, left_val);
                assert!(operator == op,
                        "operator is not {}. Got {}",
                        op,
                        operator);
                check_integer_literal(right, right_val);

            } else {
                panic!("expression is not Expression::Infix. Got {:?}",
                       expression);
            }
        } else {
            panic!("program.statements[0] is not Statement::Expression. Got {:?}",
                   program.statements[0]);
        }
    }
}
