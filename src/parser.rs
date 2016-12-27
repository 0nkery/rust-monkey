use super::lexer::Lexer;
use super::token::Token;
use super::token::TokenType;
use super::ast::Program;
use super::ast::Statement;
use super::ast::Expression;


#[derive(PartialEq, PartialOrd)]
enum Precedence
{
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}


pub struct Parser<'a>
{
    lexer: &'a mut Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a>
{
    pub fn new(lexer: &'a mut Lexer) -> Self {
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
            value: self.cur_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expr(Precedence::Lowest);
        if value.is_none() {
            return None;
        }

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Let {
            token: let_token,
            name: name,
            value: value.unwrap(),
        })
    }

    fn parse_return_stmt(&mut self) -> Option<Statement> {
        let return_token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expr(Precedence::Lowest);

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        Some(Statement::Return {
            token: return_token,
            value: return_value,
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
            expression: expr,
        })
    }

    fn parse_block_stmt(&mut self) ->Option<Statement> {
        let token = self.cur_token.clone();
        let mut statements = Vec::new();

        self.next_token();

        while self.cur_token.token_type != TokenType::RightBrace {
            let stmt = self.parse_stmt();
            if stmt.is_some() {
                statements.push(stmt.unwrap());
            }
            self.next_token();
        }

        Some(Statement::Block {
            token: token,
            statements: statements
        })
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left = self.parse_prefix();

        while self.peek_token.token_type != TokenType::Semicolon &&
              precedence < self.peek_precedence() {
            self.next_token();
            let expr = self.parse_infix_expr(left.clone().unwrap());

            if expr.is_none() {
                return left;
            }

            left = expr;
        }

        left
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match self.cur_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expr(),
            TokenType::True | TokenType::False => self.parse_boolean(),
            TokenType::LeftParen => self.parse_grouped_expr(),
            TokenType::If => self.parse_if_expr(),
            TokenType::Function => self.parse_function_literal(),
            ref tt @ _ => {
                let err_msg = format!("No prefix parse fn for {:?} found.", tt);
                self.errors.push(err_msg);

                None
            },
        }
    }

    fn parse_identifier(&self) -> Option<Expression> {
        Some(Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let literal_token = self.cur_token.clone();
        let parse_result = self.cur_token.literal.parse();

        match parse_result {
            Ok(value) => {
                Some(Expression::IntegerLiteral {
                    token: literal_token,
                    value: value,
                })
            },
            Err(_) => {
                let msg = format!("Could not parse {} as integer", literal_token.literal);
                self.errors.push(msg);

                None
            },
        }
    }

    fn parse_boolean(&self) -> Option<Expression> {
        Some(Expression::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token.token_type == TokenType::True
        })
    }

    fn parse_grouped_expr(&mut self) -> Option<Expression> {
        self.next_token();

        let expr = self.parse_expr(Precedence::Lowest);

        if self.expect_peek(TokenType::RightParen) {
            expr
        } else {
            None
        }
    }

    fn parse_if_expr(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LeftParen) {
            return None;
        }

        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest);

        if condition.is_none() {
            return None;
        }

        if !self.expect_peek(TokenType::RightParen) {
            return None;
        }
        if !self.expect_peek(TokenType::LeftBrace) {
            return None;
        }

        let consequence = self.parse_block_stmt();

        if consequence.is_none() {
            return None;
        }

        let mut alternative = None;

        if self.peek_token.token_type == TokenType::Else {
            self.next_token();

            if !self.expect_peek(TokenType::LeftBrace) {
                return None;
            }

            let alternative_maybe = self.parse_block_stmt();
            if alternative_maybe.is_some() {
                alternative = Some(Box::new(alternative_maybe.unwrap()));
            }
        }

        Some(Expression::If {
            token: token,
            condition: Box::new(condition.unwrap()),
            consequence: Box::new(consequence.unwrap()),
            alternative: alternative
        })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LeftParen) {
            return None;
        }

        let parameters = self.parse_function_parameters();
        if parameters.is_none() {
            return None;
        }

        if !self.expect_peek(TokenType::LeftBrace) {
            return None;
        }

        let body = self.parse_block_stmt();
        if body.is_none() {
            return None;
        }

        Some(Expression::FunctionLiteral {
            token: token,
            parameters: parameters.unwrap(),
            body: Box::new(body.unwrap()),
        })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut identifiers = Vec::new();

        if self.peek_token.token_type == TokenType::RightParen {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        identifiers.push(ident);

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token();
            self.next_token();

            let ident = Expression::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone()
            };
            identifiers.push(ident);
        }

        if self.expect_peek(TokenType::RightParen) {
            Some(identifiers)
        } else {
            None
        }
    }

    fn parse_prefix_expr(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();

        self.next_token();

        let right = self.parse_expr(Precedence::Prefix);

        if let Some(r) = right {
            Some(Expression::Prefix {
                token: token,
                operator: operator,
                right: Box::new(r),
            })
        } else {
            None
        }
    }

    fn precedence(&self, tt: &TokenType) -> Precedence {
        match *tt {
            TokenType::Eq => Precedence::Equals,
            TokenType::NotEq => Precedence::Equals,
            TokenType::LessThan => Precedence::LessGreater,
            TokenType::GreaterThan => Precedence::LessGreater,
            TokenType::Plus => Precedence::Sum,
            TokenType::Minus => Precedence::Sum,
            TokenType::Slash => Precedence::Product,
            TokenType::Asterisk => Precedence::Product,
            TokenType::LeftParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence { self.precedence(&self.peek_token.token_type) }

    fn cur_precedence(&self) -> Precedence { self.precedence(&self.cur_token.token_type) }

    fn parse_infix_expr(&mut self, left: Expression) -> Option<Expression> {
        match self.cur_token.token_type {
            TokenType::Minus | TokenType::Plus |
            TokenType::Asterisk | TokenType::Slash |
            TokenType::LessThan | TokenType::GreaterThan |
            TokenType::Eq | TokenType::NotEq => {
                let token = self.cur_token.clone();
                let op = self.cur_token.literal.clone();

                let precedence = self.cur_precedence();
                self.next_token();
                let right = self.parse_expr(precedence);

                if let Some(r) = right {
                    Some(Expression::Infix {
                        token: token,
                        operator: op,
                        left: Box::new(left),
                        right: Box::new(r),
                    })
                } else {
                    None
                }
            },
            TokenType::LeftParen => self.parse_call_expr(left),
            ref tt @ _ => {
                let msg = format!("Infix parse func for {:?} not found.", tt);
                self.errors.push(msg);

                None
            }
        }
        
    }

    fn parse_call_expr(&mut self, func: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let args = self.parse_call_args();

        if args.is_none() {
            return None;
        }

        Some(Expression::Call {
            token: token,
            arguments: args.unwrap(),
            function: Box::new(func)
        })
    }

    fn parse_call_args(&mut self) -> Option<Vec<Expression>> {
        let mut args = Vec::new();

        if self.peek_token.token_type == TokenType::RightParen {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest);
        if expr.is_none() {
            return None;
        }
        args.push(expr.unwrap());

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token();
            self.next_token();

            let expr = self.parse_expr(Precedence::Lowest);
            if expr.is_none() {
                return None;
            }
            args.push(expr.unwrap());
        }

        if self.expect_peek(TokenType::RightParen) {
            Some(args)
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

    pub fn parse_program(&mut self) -> Program {
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

    pub fn errors(&self) -> &[String] {
        &self.errors
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
        _ => panic!("il is not Expression::IntegerLiteral. Got {:?}", il),
    }
}

#[cfg(test)]
fn check_infix_expression(ie: &Expression, exp_left: &str, exp_op: &str, exp_right: &str) {
    if let Expression::Infix { ref left, ref operator, ref right, .. } = *ie {
        assert!(left.string() == exp_left,
                "Infix: left is not {}. Got {}",
                exp_left,
                left.string());
        assert!(operator == exp_op,
                "Infix: op is not {}. Got {}",
                exp_left,
                operator);
        assert!(right.string() == exp_right,
                "Infix: right is not {}. Got {}",
                exp_right,
                right.string());
    } else {
        panic!("Expression is not Expression::Infix. Got {:?}", ie);
    }
}

#[cfg(test)]
fn check_identifier(ident: &Expression, exp_value: &str) {
    if let Expression::Identifier { ref value, .. } = *ident {
        assert!(value == exp_value,
                "ident.value is not {}. Got {}",
                exp_value,
                value);
        assert!(ident.token_literal() == exp_value,
                "ident.token_literal() is not {}. Got {}",
                exp_value,
                ident.token_literal());
    } else {
        panic!("ident is not Expression::Identifier. Got {:?}", ident);
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
    let tests = vec![("!5", "!", 5), ("-15", "-", 15)];

    for (input, op, value) in tests {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert!(program.statements.len() == 1,
                "program.statements does not contain 1 statement. Got {}",
                program.statements.len());

        if let Statement::Expression { ref expression, .. } = program.statements[0] {
            if let Expression::Prefix { ref operator, ref right, .. } = *expression {
                assert!(operator == op, "operator is not {}. Got {}", op, operator);
                check_integer_literal(right, value);

            } else {
                panic!("expression is not Expression::Prefix. Got {:?}", expression);
            }
        } else {
            panic!("program.statements[0] is not Statement::Expression. Got {:?}",
                   program.statements[0]);
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let tests = vec![("5 + 5;", 5, "+", 5),
                     ("5 - 5;", 5, "-", 5),
                     ("5 * 5;", 5, "*", 5),
                     ("5 / 5;", 5, "/", 5),
                     ("5 > 5;", 5, ">", 5),
                     ("5 < 5;", 5, "<", 5),
                     ("5 == 5;", 5, "==", 5),
                     ("5 != 5;", 5, "!=", 5)];

    for (input, left_val, op, right_val) in tests {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert!(program.statements.len() == 1,
                "program.statements does not contain 1 statement. Got {}",
                program.statements.len());

        if let Statement::Expression { ref expression, .. } = program.statements[0] {
            if let Expression::Infix { ref left, ref operator, ref right, .. } = *expression {

                check_integer_literal(left, left_val);
                assert!(operator == op, "operator is not {}. Got {}", op, operator);
                check_integer_literal(right, right_val);

            } else {
                panic!("expression is not Expression::Infix. Got {:?}", expression);
            }
        } else {
            panic!("program.statements[0] is not Statement::Expression. Got {:?}",
                   program.statements[0]);
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        (
            "-a * b",
            "((-a) * b)"
        ),
        (
            "!-a",
            "(!(-a))"
        ),
        (
            "a + b + c",
            "((a + b) + c)"
        ),
        (
            "a + b - c",
            "((a + b) - c)"
        ),
        (
            "a * b * c",
            "((a * b) * c)"
        ),
        (
            "a * b / c",
            "((a * b) / c)"
        ),
        (
            "a + b / c",
            "(a + (b / c))"
        ),
        (
            "a + b * c + d / e - f",
            "(((a + (b * c)) + (d / e)) - f)"
        ),
        (
            "3 + 4; -5 * 5",
            "(3 + 4)((-5) * 5)"
        ),
        (
            "5 > 4 == 3 < 4",
            "((5 > 4) == (3 < 4))"
        ),
        (
            "5 < 4 != 3 > 4",
            "((5 < 4) != (3 > 4))"
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
        ),
        (
            "true",
            "true"
        ),
        (
            "false",
            "false"
        ),
        (
            "3 > 5 == false",
            "((3 > 5) == false)"
        ),
        (
            "3 < 5 == true",
            "((3 < 5) == true)"
        ),
        (
            "1 + (2 + 3) + 4",
            "((1 + (2 + 3)) + 4)"
        ),
        (
            "(5 + 5) * 2",
            "((5 + 5) * 2)"
        ),
        (
            "2 / (5 + 5)",
            "(2 / (5 + 5))"
        ),
        (
            "-(5 + 5)",
            "(-(5 + 5))"
        ),
        (
            "!(true == true)",
            "(!(true == true))"
        )
    ];

    for (input, expected) in tests {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let actual = program.string();
        assert!(actual == expected, "Expected {}. Got {}", expected, actual);
    }
}


#[test]
fn test_boolean_expression() {
    let tests = vec![
        ("true;", true, "true"),
        ("false;", false, "false")
    ];

    for (input, expected_value, expected_literal) in tests {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert!(program.statements.len() == 1,
                "program.statements does not contain 1 statement. Got {}",
                program.statements.len());

        if let Statement::Expression { ref expression, .. } = program.statements[0] {
            if let Expression::Boolean { value, .. } = *expression {
                assert!(value == expected_value,
                        "value is not {}. Got {}",
                        expected_value,
                        value);
                assert!(expression.token_literal() == expected_literal,
                        "token_literal is not {}. Got {}",
                        expected_literal,
                        expression.token_literal());
            } else {
                panic!("expression is not Expression::Boolean. Got {:?}",
                       expression);
            }
        } else {
            panic!("stmt is not Statement::Expression. Got {:?}",
                   program.statements[0]);
        }
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let mut l = Lexer::new(input.to_string());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();
    check_parser_errors(&p);

    assert!(program.statements.len() == 1,
            "program.statements does not contain 1 statement. Got {}",
            program.statements.len());

    if let Statement::Expression { ref expression, .. } = program.statements[0] {
        if let Expression::If { ref condition, ref consequence, ref alternative, .. } = *expression {
            check_infix_expression(condition, "x", "<", "y");
            if let Statement::Block { ref statements, .. } = *consequence.as_ref() {
                assert!(statements.len() == 1,
                        "Consequence does not contain 1 statement. Got {}",
                        statements.len());
                if let Statement::Expression { ref expression, .. } = statements[0] {
                    check_identifier(expression, "x");
                } else {
                    panic!("consequence is not Statement::Expression. Got {:?}", consequence);
                }
            } else {
                panic!("consequence is not Statement::Block. Got {:?}", consequence);
            }

            assert!(alternative.is_none(),
                    "expression.alternative was not None. Got {:?}",
                    alternative);
        } else {
            panic!("expression is not Expression::If. Got {:?}",
                   expression);
        }
    } else {
        panic!("program.statements[0] is not Statement::Expression. Got {:?}",
               program.statements[0]);
    }
}


#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";

    let mut l = Lexer::new(input.to_string());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();
    check_parser_errors(&p);

    assert!(program.statements.len() == 1,
            "program.statements does not contain 1 statement. Got {}",
            program.statements.len());

    if let Statement::Expression { ref expression, .. } = program.statements[0] {
        if let Expression::FunctionLiteral { ref parameters, ref body, .. } = *expression {
            assert!(parameters.len() == 2,
                    "parameters wrong. Want 2, Got {}",
                    parameters.len());
            if let Statement::Block { ref statements, .. } = *body.as_ref() {
                assert!(statements.len() == 1,
                        "statements should have 1 statement. Got {}",
                        statements.len());
            }
        } else {
            panic!("expression is not Expression::FunctionLiteral. Got {:?}",
                   expression);
        }
    } else {
        panic!("program.statements[0] is not Statement::Expression. Got {:?}",
               program.statements[0]);
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let mut l = Lexer::new(input.to_string());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();
    check_parser_errors(&p);

    assert!(program.statements.len() == 1,
            "program.statements does not contain 1 statement. Got {}",
            program.statements.len());

    if let Statement::Expression { ref expression, .. } = program.statements[0] {
        if let Expression::Call { ref arguments, ref function, .. } = *expression {
            check_identifier(function, "add");
            assert!(arguments.len() == 3,
                    "parameters wrong. Want 3, Got {}",
                    arguments.len());
            check_infix_expression(&arguments[1], "2", "*", "3");
            check_infix_expression(&arguments[2], "4", "+", "5");
        } else {
            panic!("expression is not Expression::Call. Got {:?}",
                   expression);
        }
    } else {
        panic!("program.statements[0] is not Statement::Expression. Got {:?}",
               program.statements[0]);
    }
}