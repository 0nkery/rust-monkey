use super::object::Object;
use super::object::BuiltinFn;


pub fn len(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("Wrong number of arguments. Got {}, want 1", args.len()));
    }

    match args[0] {
        Object::String(ref val) => Object::Integer(val.len() as i64),
        Object::Array(ref elems) => Object::Integer(elems.len() as i64),
        ref o @ _ => Object::Error(format!("Argument to 'len' is not supported. Got {}", o)),
    }
}

pub const LEN: Object = Object::Builtin(BuiltinFn(len));


pub fn first(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("Wrong number of arguments. Got {}, want 1", args.len()));
    }

    if let Object::Array(ref elems) = args[0] {
        if elems.len() > 0 {
            elems[0].clone()
        } else {
            Object::Null
        }
    } else {
        Object::Error(format!("Argument to 'first' must be Array. Got {}", args[0]))
    }
}

pub const FIRST: Object = Object::Builtin(BuiltinFn(first));


pub fn last(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("Wrong number of arguments. Got {}, want 1", args.len()));
    }

    if let Object::Array(ref elems) = args[0] {
        let length = elems.len();
        if length > 0 {
            elems[length - 1].clone()
        } else {
            Object::Null
        }
    } else {
        Object::Error(format!("Argument to 'last' must be Array. Got {}", args[0]))
    }
}

pub const LAST: Object = Object::Builtin(BuiltinFn(last));


pub fn rest(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!("Wrong number of arguments. Got {}, want 1", args.len()));
    }

    if let Object::Array(ref elems) = args[0] {
        let length = elems.len();
        if length > 0 {
            let mut new_elems = elems.clone();
            new_elems.remove(0);
            Object::Array(new_elems)
        } else {
            Object::Null
        }
    } else {
        Object::Error(format!("Argument to 'rest' must be Array. Got {}", args[0]))
    }
}

pub const REST: Object = Object::Builtin(BuiltinFn(rest));


pub fn push(args: &[Object]) -> Object {
    if args.len() != 2 {
        return Object::Error(format!("Wrong number of arguments. Got {}, want 2", args.len()));
    }

    if let Object::Array(ref elems) = args[0] {
        let mut new_elems = elems.clone();
        new_elems.push(args[1].clone());
        Object::Array(new_elems)
    } else {
        Object::Error(format!("Argument to 'push' must be Array. Got {}", args[0]))
    }
}

pub const PUSH: Object = Object::Builtin(BuiltinFn(push));
