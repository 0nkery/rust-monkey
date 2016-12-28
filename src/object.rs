#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>)
}

impl Object {
    pub fn inspect(&self) -> String {
        match *self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "null".to_string(),
            Object::ReturnValue(ref obj) => obj.inspect(),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match *self {
            TRUE => true,
            NULL | FALSE => false,
            _ => true,
        }
    }
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;