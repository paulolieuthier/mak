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

    fn single_arg_call<'a>(&self, arg: &Value<'a>) -> Result<Option<Value<'a>>, String> {
        println!("{}", arg);
        Ok(None)
    }

    fn named_args_call<'a>(
        &self,
        args: BTreeMap<&str, Value<'a>>,
    ) -> Result<Option<Value<'a>>, String> {
        match args.get(MSG_ARG) {
            Some(arg) => self.single_arg_call(arg),
            None => Err(format!("print: argument 'msg' missing")),
        }
    }
}
