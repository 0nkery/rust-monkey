use std::fmt;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use super::ast::Expression;
use super::ast::Statement;
use super::ast::Node;


pub struct BuiltinFn(pub fn(&[Object]) -> Object);

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HashKey(u64);

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
    Array(Vec<Object>),
    Hash(HashMap<HashKey, (Object, Object)>),
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
            Object::Array(ref elems) => {
                format!("[{}]", elems.iter().map(|el| el.inspect()).collect::<Vec<_>>().join(", "))
            }
            Object::Hash(ref map) => {
                format!("{{{}}}",
                        map.values()
                            .map(|&(ref key, ref value)| {
                                format!("{}: {}", key.inspect(), value.inspect())
                            })
                            .collect::<Vec<_>>()
                            .join(", "))
            }
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

    pub fn hash_key(&self) -> HashKey {
        match *self {
            Object::Integer(val) => HashKey(val as u64),
            Object::Boolean(val) => {
                HashKey(if val {
                    1
                } else {
                    0
                })
            }
            Object::String(ref val) => {
                let mut hasher = DefaultHasher::new();
                val.hash(&mut hasher);
                HashKey(hasher.finish())
            }
            _ => panic!("Got object not allowed to be hash key: {}", *self),
        }
    }

    pub fn is_hashable(&self) -> bool {
        match *self {
            Object::Integer(..) |
            Object::Boolean(..) |
            Object::String(..) => true,
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
            Object::Builtin(..) => write!(f, "Builtin"),
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
