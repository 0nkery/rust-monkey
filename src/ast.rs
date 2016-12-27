use super::token::Token;
use super::token::TokenType;

pub trait Node<'a> {
    fn token_literal(&'a self) -> &'a str;
    fn string(&self) -> String;
}

#[derive(Debug)]
pub enum Statement {
    Let {
        token: Token,
        name: Expression,
        value: Expression,
    },
    Return {
        token: Token,
        value: Expression,
    },
    Expression {
        token: Token,
        expression: Expression,
    },
}

impl<'a> Node<'a> for Statement {
    fn token_literal(&self) -> &str {
        match *self {
            Statement::Let { ref token, .. } => &token.literal,
            Statement::Return { ref token, .. } => &token.literal,
            Statement::Expression { ref token, .. } => &token.literal,
        }
    }

    fn string(&self) -> String {
        let s = match *self {
            Statement::Let { ref name, ref value, .. } => {
                format!("{} {} = {};", self.token_literal(), name.string(), value.string())
            }
            Statement::Return { ref value, .. } => {
                format!("{} {};", self.token_literal(), value.string())
            }
            Statement::Expression { ref expression, .. } => format!("{}", expression.string()),
        };

        s
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier {
        token: Token,
        value: String,
    },
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    Prefix {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Boolean {
        token: Token,
        value: bool
    }
}

impl<'a> Node<'a> for Expression {
    fn token_literal(&self) -> &str {
        match *self {
            Expression::Identifier { ref token, .. } => &token.literal,
            Expression::IntegerLiteral { ref token, .. } => &token.literal,
            Expression::Prefix { ref token, .. } => &token.literal,
            Expression::Infix { ref token, .. } => &token.literal,
            Expression::Boolean { ref token, .. } => &token.literal,
        }
    }
    fn string(&self) -> String {
        match *self {
            Expression::Identifier { ref value, .. } => value.clone(),
            Expression::IntegerLiteral { ref token, .. } => token.literal.to_string(),
            Expression::Prefix { ref operator, ref right, .. } => {
                format!("({}{})", operator, right.string())
            }
            Expression::Infix { ref left, ref operator, ref right, .. } => {
                format!("({} {} {})", left.string(), operator, right.string())
            },
            Expression::Boolean { ref token, .. } => token.literal.to_string()
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: Vec::new() }
    }
}

impl<'a> Node<'a> for Program {
    fn token_literal(&self) -> &str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }

    fn string(&self) -> String {
        let mut s = String::new();

        for stmt in &self.statements {
            s += &stmt.string();
        }

        s
    }
}


#[test]
fn test_string() {
    let program = Program {
        statements: vec![Statement::Let {
                             token: Token::new(TokenType::Let, "let".to_string()),
                             name: Expression::Identifier {
                                 token: Token::new(TokenType::Ident, "my_var".to_string()),
                                 value: "my_var".to_string(),
                             },
                             value: Expression::Identifier {
                                 token: Token::new(TokenType::Ident, "another_var".to_string()),
                                 value: "another_var".to_string(),
                             },
                         }],
    };

    assert!(program.string() == "let my_var = another_var;",
            "program.string() wrong. Got {}",
            program.string());
}
