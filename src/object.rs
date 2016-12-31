use std::fmt;
use std::collections::HashMap;

use super::ast::Expression;
use super::ast::Statement;
use super::ast::Node;


pub struct BuiltinFn(fn(&[Object]) -> Object);

impl Clone for BuiltinFn {
    fn clone(&self) -> Self {
        BuiltinFn(self.0)
    }
}

impl fmt::Debug for BuiltinFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BuiltinFn")
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function {
        parameters: Vec<Expression>,
        body: Statement,
        env: Env,
    },
    Builtin(BuiltinFn),
}

impl Object {
    pub fn inspect(&self) -> String {
        match *self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::String(ref val) => format!("{}", val),
            Object::Null => "null".to_string(),
            Object::ReturnValue(ref obj) => obj.inspect(),
            Object::Error(ref msg) => format!("ERROR: {}", msg),
            Object::Function { ref parameters, ref body, .. } => {
                let mut params = Vec::new();
                for p in parameters {
                    params.push(p.string());
                }

                format!("fn({}){{\n{}\n}}", params.join(", "), body.string())
            }
            Object::Builtin(..) => "builtin function".to_string(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match *self {
            Object::Boolean(true) => true,
            Object::Boolean(false) => false,
            ref o if o.is_null() => false,
            _ => true,
        }
    }

    pub fn is_error(&self) -> bool {
        match *self {
            Object::Error(..) => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        match *self {
            Object::Null => true,
            _ => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Integer(..) => write!(f, "Integer"),
            Object::Boolean(..) => write!(f, "Boolean"),
            Object::String(..) => write!(f, "String"),
            _ => write!(f, ""),
        }
    }
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;


#[derive(Debug, Clone)]
pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Self {
        Env { store: HashMap::new() }
    }

    pub fn get<'a>(&'a self, name: &str) -> Object {
        match self.store.get(name) {
            Some(obj) => obj.clone(),
            None => Object::Error(format!("Identifier not found: {}", name)),
        }
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }
}
