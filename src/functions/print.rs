use super::*;
use lazy_static::*;
use std::collections::{BTreeMap, BTreeSet};

pub struct PrintFunction;

const MSG_ARG: &'static str = "msg";

lazy_static! {
    static ref ARGS: BTreeSet<&'static str> = {
        let mut set = BTreeSet::new();
        set.insert(MSG_ARG);
        set
    };
}

impl Function for PrintFunction {
    fn name(&self) -> &'static str {
        "print"
    }

    fn single_arg_call(&self, arg: &Value) {
        println!("{}", arg);
    }

    fn named_args_call(&self, args: BTreeMap<&str, Value>) {
        assert!(args.len() == ARGS.len());
        assert!(args.keys().into_iter().all(|arg| ARGS.contains(arg)));
        println!("{}", args.get(MSG_ARG).unwrap());
    }
}
