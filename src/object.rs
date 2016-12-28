enum ObjectKind {
    Integer,
    Boolean,
    Null
}


enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl Object {
    fn inspect(&self) -> String {
        match *self {
            Object::Integer(val) => format!("{}", val),
            Object::Boolean(val) => format!("{}", val),
            Object::Null => "null".to_string(),
        }
    }

    fn kind(&self) -> ObjectKind {
        match *self {
            Object::Integer => ObjectKind::Integer,
            Object::Boolean => ObjectKind::Boolean,
            Object::Null => ObjectKind::Null,
        }
    }
}