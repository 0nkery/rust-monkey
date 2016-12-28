use super::object::Object;
use super::lexer::Lexer;
use super::parser::Parser;
use super::ast::Program;
use super::ast::Statement;
use super::ast::Expression;


pub fn eval(program: Program) -> Object {
    let mut result = Object::Null;

    for stmt in program.statements {
        result = match stmt {
            Statement::Expression { ref expression, .. } => eval_expr(expression),
            _ => Object::Null
        };
    }

    result
}

fn eval_expr(expr: &Expression) -> Object {
    match *expr {
        Expression::IntegerLiteral { value, .. } => Object::Integer(value),
        _ => Object::Null
    }
}


#[cfg(test)]
fn check_eval(input: &str) -> Object {
    let mut l = Lexer::new(input.to_string());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();

    eval(program)
}

#[cfg(test)]
fn check_integer_object(obj: Object, expected: i64) {
    match obj {
        Object::Integer(val) => {
            assert!(val == expected,
                    "Object has wrong value. Got {}, want {}",
                    val,
                    expected);
        },
        _ => {
            panic!("Object is not an Integer. Got {:?}", obj);
        }
    }
}


#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10)
    ];

    for (input, expected) in tests {
        let evaluated = check_eval(input);
        check_integer_object(evaluated, expected);
    }
}