use super::object::Object;
use super::object::BuiltinFn;


pub fn len(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("Wrong number of arguments. Got {}, want 1", args.len()));
    }

    match args[0] {
        Object::String(ref val) => Object::Integer(val.len() as i64),
        ref o @ _ => Object::Error(format!("Argument to 'len' is not supported. Got {}", o)),
    }
}

pub const LEN: Object = Object::Builtin(BuiltinFn(len));
