use super::object::Object;
use super::object::Env;
use super::object::NULL;
use super::object::TRUE;
use super::object::FALSE;
use super::ast::Program;
use super::ast::Statement;
use super::ast::Expression;


fn to_boolean_object(val: bool) -> Object {
    if val {
        TRUE
    } else {
        FALSE
    }
}


pub struct Eval {
    env: Env,
    program: Program,
}

impl Eval {
    pub fn new(program: Program, env: Env) -> Self {
        Eval {
            env: env,
            program: program,
        }
    }

    pub fn env(self) -> Env {
        self.env
    }

    pub fn eval(&self) -> Object {
        self.eval_statements(&self.program.statements, false)
    }

    fn eval_statements(&self, stmts: &Vec<Statement>, nested: bool) -> Object {
        let mut result = NULL;

        for stmt in stmts {
            let result = self.eval_statement(&stmt);

            match result {
                Object::ReturnValue(val) => {
                    if nested {
                        return Object::ReturnValue(val);
                    } else {
                        return *val;
                    }
                }
                r @ Object::Error(..) => {
                    return r;
                }
                _ => {
                    continue;
                }
            }
        }

        result
    }

    fn eval_statement(&self, stmt: &Statement) -> Object {
        match *stmt {
            Statement::Expression { ref expression, .. } => self.eval_expr(expression),
            Statement::Block { ref statements, .. } => self.eval_statements(statements, true),
            Statement::Return { ref value, .. } => {
                let mut return_val = NULL;
                if let Some(ref v) = *value {
                    return_val = self.eval_expr(v);
                }

                if return_val.is_error() {
                    return return_val;
                } else {
                    return Object::ReturnValue(Box::new(return_val));
                }
            }
            Statement::Let { ref value, ref name, .. } => {
                let val = self.eval_expr(value);
                if val.is_error() {
                    return val;
                }
                if let Expression::Identifier { ref value, .. } = *name {
                    self.env.set(value, val);
                    return val;
                } else {
                    return Object::Error("Internal interpreter error. Not an \
                                          Expression::Identifier in Statement::Let::expression."
                        .to_string());
                }
            }
        }
    }

    fn eval_expr(&self, expr: &Expression) -> Object {
        match *expr {
            Expression::IntegerLiteral { value, .. } => Object::Integer(value),
            Expression::Boolean { value, .. } => to_boolean_object(value),
            Expression::Prefix { ref operator, ref right, .. } => {
                let right = self.eval_expr(right);

                if right.is_error() {
                    return right;
                }

                self.eval_prefix_expr(operator, right)
            }
            Expression::Infix { ref left, ref right, ref operator, .. } => {
                let left = self.eval_expr(left);

                if left.is_error() {
                    return left;
                }

                let right = self.eval_expr(right);

                if right.is_error() {
                    return right;
                }

                self.eval_infix_expr(operator, left, right)
            }
            Expression::If { ref condition, ref consequence, ref alternative, .. } => {
                let cond = self.eval_expr(condition);

                if cond.is_error() {
                    return cond;
                }

                if cond.is_truthy() {
                    self.eval_statement(consequence)
                } else if let Some(ref alt) = *alternative {
                    self.eval_statement(alt)
                } else {
                    NULL
                }
            }
            _ => NULL,
        }
    }

    fn eval_prefix_expr(&self, op: &str, right: Object) -> Object {
        match op {
            "!" => self.eval_bang_operator_expr(right),
            "-" => self.eval_minus_prefix_operator_expr(right),
            _ => Object::Error(format!("Unknown operator: {}{}", op, right)),
        }
    }

    fn eval_bang_operator_expr(&self, right: Object) -> Object {
        match right {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        }
    }

    fn eval_minus_prefix_operator_expr(&self, right: Object) -> Object {
        match right {
            Object::Integer(val) => Object::Integer(-val),
            r @ _ => Object::Error(format!("Unknown operator: -{}", r)),
        }
    }

    fn eval_infix_expr(&self, op: &str, left: Object, right: Object) -> Object {
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
                    _ => {
                        Object::Error(format!("Unknown operator: {} {} {}",
                                              Object::Integer(lval),
                                              op,
                                              Object::Integer(rval)))
                    }
                }
            }
            ("==", lval, rval) => to_boolean_object(lval == rval),
            ("!=", lval, rval) => to_boolean_object(lval != rval),
            (_, l @ Object::Integer(..), r @ Object::Boolean(..)) |
            (_, l @ Object::Boolean(..), r @ Object::Integer(..)) => {
                Object::Error(format!("Type mismatch: {} {} {}", l, op, r))
            }
            (_, l, r) => Object::Error(format!("Unknown operator: {} {} {}", l, op, r)),
        }
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
    let eval = Eval::new(program, Env::new());

    eval.eval()
}

#[cfg(test)]
fn check_integer_object(obj: Object, expected: i64) {
    match obj {
        Object::Integer(val) => {
            assert!(val == expected, "Object has wrong value. Got {}, want {}", val, expected);
        }
        _ => panic!("Object is not an Integer. Got {:?}", obj),
    }
}

#[cfg(test)]
fn check_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Boolean(val) => {
            assert!(val == expected, "Object has wrong value. Got {}, want {}", val, expected);
        }
        _ => panic!("Object is not an Boolean. Got {:?}", obj),
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
            NULL => check_null_object(evaluated),
            _ => panic!("Fix the test."),
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) {
            if (10 > 1) {
              return 10;
            }

            return 1;
         }", 10),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        check_integer_object(evaluated, expected);
    }
}

#[test]
fn test_error_handling() {
    let tests = vec![("5 + true;", "Type mismatch: Integer + Boolean"),
                     ("5 + true; 5;", "Type mismatch: Integer + Boolean"),
                     ("-true", "Unknown operator: -Boolean"),
                     ("true + false;", "Unknown operator: Boolean + Boolean"),
                     ("5; true + false; 5", "Unknown operator: Boolean + Boolean"),
                     ("if (10 > 1) { true + false; }", "Unknown operator: Boolean + Boolean"),
                     ("if (10 > 1) {
                if (10 > 1) {
                    return \
                       true + false;
                }
                return 1;
            }",
                      "Unknown operator: Boolean + Boolean"),
                     ("foobar", "Identifier not found: foobar")];

    for (input, expected) in tests {
        let evaluated = test_eval(input);

        if let Object::Error(ref msg) = evaluated {
            assert!(msg == expected, "Wrong error message. Expected\n{}\nGot\n{}", expected, msg);
        } else {
            panic!("Evaluated object is not Object::Error. Got {:?}", evaluated);
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in tests {
        check_integer_object(test_eval(input), expected);
    }
}
