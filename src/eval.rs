use super::object::Object;

use super::ast::Program;
use super::ast::Statement;
use super::ast::Expression;


const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;


pub fn eval(program: Program) -> Object {
    let mut result = Object::Null;

    for stmt in program.statements {
        result = match stmt {
            Statement::Expression { ref expression, .. } => eval_expr(expression),
            _ => NULL
        };
    }

    result
}

fn eval_expr(expr: &Expression) -> Object {
    match *expr {
        Expression::IntegerLiteral { value, .. } => Object::Integer(value),
        Expression::Boolean { value, .. } => to_boolean_object(value),
        Expression::Prefix { ref operator, ref right, .. } => {
            let right = eval_expr(right);
            eval_prefix_expr(operator, right)
        },
        Expression::Infix { ref left, ref right, ref operator, .. } => {
            let left = eval_expr(left);
            let right = eval_expr(right);
            eval_infix_expr(operator, left, right)
        }
        _ => NULL
    }
}

fn to_boolean_object(val: bool) -> Object {
    if val { TRUE } else { FALSE }
}

fn eval_prefix_expr(op: &str, right: Object) -> Object {
    match op {
        "!" => eval_bang_operator_expr(right),
        "-" => eval_minus_prefix_operator_expr(right),
        _ => NULL
    }
}

fn eval_bang_operator_expr(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE
    }
}

fn eval_minus_prefix_operator_expr(right: Object) -> Object {
    match right {
        Object::Integer(val) => Object::Integer(-val),
        _ => NULL,
    }
}

fn eval_infix_expr(op: &str, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left_val), Object::Integer(right_val)) => {
            match op {
                "+" => Object::Integer(left_val + right_val),
                "-" => Object::Integer(left_val - right_val),
                "*" => Object::Integer(left_val * right_val),
                "/" => Object::Integer(left_val / right_val),
                "<" => to_boolean_object(left_val < right_val),
                ">" => to_boolean_object(left_val > right_val),
                "==" => to_boolean_object(left_val == right_val),
                "!=" => to_boolean_object(left_val != right_val),
                _ => NULL
            }
        }
        _ => NULL
    }
}


#[cfg(test)]
use super::lexer::Lexer;
#[cfg(test)]
use super::parser::Parser;

#[cfg(test)]
fn test_eval(input: &str) -> Object {
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
        _ => panic!("Object is not an Integer. Got {:?}", obj)
    }
}

#[cfg(test)]
fn check_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Boolean(val) => {
            assert!(val == expected,
                    "Object has wrong value. Got {}, want {}",
                    val,
                    expected);
        },
        _ => panic!("Object is not an Boolean. Got {:?}", obj)
    }
}


#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        check_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        check_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        check_boolean_object(evaluated, expected);
    }
}