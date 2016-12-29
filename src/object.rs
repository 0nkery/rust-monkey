use std::fmt;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

impl Object {
    pub fn inspect(&self) -> String {
        match *self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "null".to_string(),
            Object::ReturnValue(ref obj) => obj.inspect(),
            Object::Error(ref msg) => format!("ERROR: {}", msg),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match *self {
            TRUE => true,
            NULL | FALSE => false,
            _ => true,
        }
    }

    pub fn is_error(&self) -> bool {
        match *self {
            Object::Error(..) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Integer(..) => write!(f, "Integer"),
            Object::Boolean(..) => write!(f, "Boolean"),
            _ => write!(f, ""),
        }
    }
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;


pub struct Env {
    store: HashMap<String, Object>,
}

impl Env {
    pub fn new() -> Self {
        Env { store: HashMap::new() }
    }

    pub fn get<'a>(&'a self, name: &str) -> Result<&'a Object, String> {
        Ok(&self.store[name])
    }

    pub fn set(&mut self, name: &str, val: Object) {
        self.store.insert(name.to_string(), val);
    }
}
