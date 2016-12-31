use std::collections::HashMap;

use super::object::Object;
use super::object::Env;
use super::object::NULL;
use super::object::TRUE;
use super::object::FALSE;
use super::ast::Program;
use super::ast::Statement;
use super::ast::Expression;
use super::builtins;
use super::object::BuiltinFn;


fn to_boolean_object(val: bool) -> Object {
    if val {
        TRUE
    } else {
        FALSE
    }
}


pub struct Eval {
    env: Env,
    builtins: Env,
}

impl Eval {
    pub fn new(env: Env) -> Self {
        let mut builtins = Env::new();
        builtins.set("len", builtins::LEN);
        builtins.set("first", builtins::FIRST);
        builtins.set("last", builtins::LAST);
        builtins.set("rest", builtins::REST);
        builtins.set("push", builtins::PUSH);

        Eval {
            env: env,
            builtins: builtins,
        }
    }

    pub fn env(self) -> Env {
        self.env
    }

    pub fn eval(&mut self, program: Program) -> Object {
        self.eval_statements(&program.statements, false)
    }

    fn eval_statements(&mut self, stmts: &Vec<Statement>, nested: bool) -> Object {
        let mut result = NULL;

        for stmt in stmts {
            result = self.eval_statement(&stmt);

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

    fn eval_statement(&mut self, stmt: &Statement) -> Object {
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
                    return NULL;
                } else {
                    return Object::Error("Internal interpreter error. Not an \
                                          Expression::Identifier in Statement::Let::expression."
                        .to_string());
                }
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expression) -> Object {
        match *expr {
            Expression::IntegerLiteral { value, .. } => Object::Integer(value),
            Expression::Boolean { value, .. } => to_boolean_object(value),
            Expression::String { ref value, .. } => Object::String(value.clone()),
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
            Expression::Identifier { ref value, .. } => {
                let mut val = self.env.get(value);
                if val.is_error() {
                    val = self.builtins.get(value);
                }

                val
            }
            Expression::FunctionLiteral { ref parameters, ref body, .. } => {
                Object::Function {
                    parameters: parameters.clone(),
                    body: *body.clone(),
                    env: self.env.clone(),
                }
            }
            Expression::Call { ref function, ref arguments, .. } => {
                let func = self.eval_expr(function);
                if func.is_error() {
                    return func;
                }

                let mut args = self.eval_expressions(arguments);
                let fail = args.len() == 1 && args[0].is_error();
                if fail {
                    return args.remove(0);
                }
                self.apply_function(func, args)
            }
            Expression::Array { ref elements, .. } => {
                let mut elems = self.eval_expressions(elements);

                let fail = elems.len() == 1 && elems[0].is_error();
                if fail {
                    elems.remove(0)
                } else {
                    Object::Array(elems)
                }
            }
            Expression::Index { ref left, ref index, .. } => {
                let left = self.eval_expr(left);
                if left.is_error() {
                    return left;
                }

                let index = self.eval_expr(index);
                if index.is_error() {
                    return index;
                }

                self.eval_index_expr(left, index)
            }
            Expression::Hash { ref pairs, .. } => {
                let mut map = HashMap::new();

                for &(ref key_expr, ref value_expr) in pairs {
                    let key = self.eval_expr(&key_expr);
                    if key.is_error() {
                        return key;
                    }

                    if !key.is_hashable() {
                        return Object::Error(format!("Unusable as hash key: {}", key));
                    }

                    let value = self.eval_expr(&value_expr);
                    if value.is_error() {
                        return value;
                    }

                    let hashed = value.hash_key();
                    println!("{}", hashed);
                    map.insert(hashed, (key, value));
                }

                Object::Hash(map)
            }
        }
    }

    fn eval_expressions(&mut self, expressions: &[Expression]) -> Vec<Object> {
        let mut result = Vec::new();

        for expr in expressions {
            let evaluated = self.eval_expr(expr);
            if evaluated.is_error() {
                return vec![evaluated];
            } else {
                result.push(evaluated);
            }
        }

        result
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
            Object::Boolean(true) => FALSE,
            Object::Boolean(false) => TRUE,
            Object::Null => TRUE,
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
            ("==", Object::Boolean(lval), Object::Boolean(rval)) => to_boolean_object(lval == rval),
            ("!=", Object::Boolean(lval), Object::Boolean(rval)) => to_boolean_object(lval != rval),
            ("+", Object::String(ref lval), Object::String(ref rval)) => {
                Object::String(format!("{}{}", lval, rval))
            }

            (_, l @ Object::Integer(..), r @ Object::Boolean(..)) |
            (_, l @ Object::Boolean(..), r @ Object::Integer(..)) => {
                Object::Error(format!("Type mismatch: {} {} {}", l, op, r))
            }
            (_, l, r) => Object::Error(format!("Unknown operator: {} {} {}", l, op, r)),
        }
    }

    fn apply_function(&self, mut f: Object, args: Vec<Object>) -> Object {
        match f {
            Object::Function { ref mut env, ref parameters, ref body, .. } => {
                for (param, arg) in parameters.iter().zip(args) {
                    if let Expression::Identifier { ref value, .. } = *param {
                        env.set(value, arg);
                    } else {
                        return Object::Error("Function param is not Identifier.".to_string());
                    }
                }

                let mut eval = Eval::new(env.clone());
                let evaluated = eval.eval_statement(body);

                if let Object::ReturnValue(obj) = evaluated {
                    return *obj;
                } else {
                    return evaluated;
                }
            }
            Object::Builtin(BuiltinFn(func)) => func(&args),
            _ => Object::Error(format!("Not a function: {}", f)),
        }
    }

    fn eval_index_expr(&self, left: Object, index: Object) -> Object {
        match (left, index) {
            (Object::Array(elems), Object::Integer(idx)) => self.eval_array_index_expr(elems, idx),
            (Object::Hash(ref map), ref obj) if obj.is_hashable() => {
                let pair = map.get(&obj.hash_key());
                if let Some(&(_, ref value)) = pair {
                    value.clone()
                } else {
                    NULL
                }
            }
            (Object::Hash(..), obj) => Object::Error(format!("Unusable as hash key: {}", obj)),
            (left, _) => Object::Error(format!("Index operator is not supported: {}", left)),
        }
    }

    fn eval_array_index_expr(&self, array: Vec<Object>, index: i64) -> Object {
        let idx = index as usize;
        let max = array.len() - 1;

        if idx > max {
            NULL
        } else {
            array[idx].clone()
        }
    }
}


#[cfg(test)]
use super::lexer::Lexer;
#[cfg(test)]
use super::parser::Parser;
#[cfg(test)]
use super::ast::Node;

#[cfg(test)]
fn test_eval(input: &str) -> Object {
    let mut l = Lexer::new(input.to_string());
    let mut p = Parser::new(&mut l);
    let program = p.parse_program();
    let mut eval = Eval::new(Env::new());

    eval.eval(program)
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
    assert!(obj.is_null(), "obj is not NULL. Got {:?}", obj);
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
            Object::Null => check_null_object(evaluated),
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
                     ("foobar", "Identifier not found: foobar"),
                     ("\"Hello\" - \"World\"", "Unknown operator: String - String")];

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

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";

    let evaluated = test_eval(input);

    if let Object::Function { parameters, body, .. } = evaluated {
        assert!(parameters.len() == 1, "Function has wrong parameters - {:?}", parameters);
        assert!(parameters[0].string() == "x", "parameter is not 'x'. Got {:?}", parameters[0]);

        let expected_body = "(x + 2)";
        assert!(body.string() == expected_body,
                "Body is not {}. Got {}",
                expected_body,
                body.string());
    } else {
        panic!("Object is not a Function. Got {:?}", evaluated);
    }
}

#[test]
fn test_function_application() {
    let tests = vec![("let identity = fn(x) { x; }; identity(5);", 5),
                     ("let identity = fn(x) { return x; }; identity(5);", 5),
                     ("let double = fn(x) { x * 2; }; double(5);", 10),
                     ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
                     ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
                     ("fn (x) { x; }(5)", 5)];

    for (input, expected) in tests {
        check_integer_object(test_eval(input), expected);
    }
}

#[test]
fn test_closures() {
    let input = "let newAdder = fn(x) {
                   fn(y) { x + y };
                 };

                 let addTwo = newAdder(2);
                 addTwo(2);";

    check_integer_object(test_eval(input), 4);
}

#[test]
fn test_string() {
    let input = "\"Hello World!\"";

    let evaluated = test_eval(input);

    if let Object::String(ref val) = evaluated {
        assert!(val == "Hello World!", "String has wrong value. Got {}", val);
    } else {
        panic!("Object is not String. Got {:?}", evaluated);
    }
}

#[test]
fn test_string_concatenation() {
    let input = "\"Hello\" + \" \" + \"World!\"";

    let evaluated = test_eval(input);
    if let Object::String(ref val) = evaluated {
        assert!(val == "Hello World!", "String has wrong value. Got {}", val);
    } else {
        panic!("Object is not a String. Got {:?}", evaluated);
    }
}

#[test]
fn test_builtin_functions() {
    let tests = vec![("len(\"\")", Object::Integer(0)),
                     ("len(\"four\")", Object::Integer(4)),
                     ("len(\"hello world\")", Object::Integer(11)),
                     ("len(1)",
                      Object::Error("Argument to 'len' is not supported. Got Integer".to_string())),
                     ("len(\"one\", \"two\")",
                      Object::Error("Wrong number of arguments. Got 2, want 1".to_string())),
                     ("len([1, 2, 3])", Object::Integer(3)),
                     ("len([])", Object::Integer(0)),
                     ("first([1, 2, 3])", Object::Integer(1)),
                     ("first([])", Object::Null),
                     ("last([1, 2, 3])", Object::Integer(3)),
                     ("last([])", Object::Null),
                     ("rest([])", Object::Null),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);

        match expected {
            Object::Integer(val) => check_integer_object(evaluated, val),
            Object::Error(expected) => {
                if let Object::Error(msg) = evaluated {
                    assert!(msg == expected,
                            "Wrong error message. Expected\n{}\nGot\n{}",
                            expected,
                            msg);
                } else {
                    panic!("Object is not an Error. Got {:?}", evaluated);
                }
            }
            Object::Null => check_null_object(evaluated),
            _ => panic!("Fix the tests."),
        }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let mut evaluated = test_eval(input);
    if let Object::Array(ref mut elements) = evaluated {
        assert!(elements.len() == 3, "Array has wrong number of elements. Got {}", elements.len());
        check_integer_object(elements.remove(0), 1);
        check_integer_object(elements.remove(0), 4);
        check_integer_object(elements.remove(0), 6);
    } else {
        panic!("Object is not an Array. Got {:?}", evaluated);
    }
}

#[test]
fn test_array_index_expressions() {
    let tests = vec![
        ("[1, 2, 3][0]", Object::Integer(1)),
        ("[1, 2, 3][1]", Object::Integer(2)),
        ("[1, 2, 3][2]", Object::Integer(3)),
        ("let i = 0; [1][i];", Object::Integer(1)),
        ("[1, 2, 3][1 + 1]", Object::Integer(3)),
        ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
        ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Object::Integer(6)),
        ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];", Object::Integer(2)),
        ("[1, 2, 3][3]", NULL),
        ("[1, 2, 3][-1]", NULL),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match expected {
            Object::Integer(val) => check_integer_object(evaluated, val),
            Object::Null => check_null_object(evaluated),
            _ => panic!("Fix the tests."),
        }
    }
}
