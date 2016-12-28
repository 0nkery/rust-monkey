use super::object::Object;
use super::object::NULL;
use super::object::TRUE;
use super::object::FALSE;
use super::ast::Program;
use super::ast::Statement;
use super::ast::Expression;


pub fn eval(program: Program) -> Object {
    eval_statements(&program.statements)
}

fn eval_statements(stmts: &Vec<Statement>) -> Object {
    let mut result = NULL;

    for stmt in stmts {
        result = eval_statement(&stmt);
    }

    result
}

fn eval_statement(stmt: &Statement) -> Object {
    match *stmt {
        Statement::Expression { ref expression, .. } => eval_expr(expression),
        Statement::Block { ref statements, .. } => eval_statements(statements),
        _ => NULL
    }
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
        },
        Expression::If { ref condition, ref consequence, ref alternative, .. } => {
            let cond = eval_expr(condition);

            if cond.is_truthy() {
                eval_statement(consequence)
            } else if let Some(ref alt) = *alternative {
                eval_statement(alt)
            } else {
                NULL
            }
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
    match (op, left, right) {
        (_, Object::Integer(lval), Object::Integer(rval)) => {
            match op {
                "+" => Object::Integer(lval + rval),
                "-" => Object::Integer(lval - rval),
                "*" => Object::Integer(lval * rval),
                "/" => Object::Integer(lval / rval),
                "<" => to_boolean_object(lval < rval),
                ">" => to_boolean_object(lval > rval),
                "==" => to_boolean_object(lval == rval),
                "!=" => to_boolean_object(lval != rval),
                _ => NULL
            }
        },
        ("==", lval @ _, rval @ _) => to_boolean_object(lval == rval),
        ("!=", lval @ _, rval @ _) => to_boolean_object(lval != rval),
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

#[cfg(test)]
fn check_null_object(obj: Object) {
    assert!(obj == NULL, "obj is not NULL. Got {:?}", obj);
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
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false!= true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
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

#[test]
fn test_if_else_expressions() {
    let tests = vec![
        ("if (true) { 10 }", Object::Integer(10)),
        ("if (false) { 10 }", NULL),
        ("if (1) { 10 }", Object::Integer(10)),
        ("if (1 < 2) { 10 }", Object::Integer(10)),
        ("if (1 > 2) { 10 }", NULL),
        ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
        ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match expected {
            Object::Integer(val) => check_integer_object(evaluated, val),
            Object::Boolean(val) => check_boolean_object(evaluated, val),
            NULL => check_null_object(evaluated),
        }
    }
}