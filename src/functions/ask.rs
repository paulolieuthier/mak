use super::*;
use lazy_static::*;
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::io;

pub struct AskFunction;

const MSG_ARG: &'static str = "msg";
const DEFAULT_ARG: &'static str = "default";

lazy_static! {
    static ref ARGS: BTreeSet<&'static str> = {
        let mut set = BTreeSet::new();
        set.insert(MSG_ARG);
        set.insert(DEFAULT_ARG);
        set
    };
}

impl Function for AskFunction {
    fn name(&self) -> &'static str {
        "ask"
    }

    fn single_arg_call<'a>(&self, arg: &Value<'a>) -> Result<Option<Value<'a>>, String> {
        print!("{}", arg);
        self.flush_stdout()?;
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let without_newline = input[0 .. input.len() - 1].to_owned();
                Ok(Some(Value::from_text(Cow::from(without_newline))))
            }
            Err(_) => Err(format!("ask: failure to get user input")),
        }
    }

    fn named_args_call<'a>(&self, args: BTreeMap<&'a str, Value<'a>>) -> Result<Option<Value<'a>>, String> {
        let msg = match args.get(MSG_ARG) {
            Some(value) => value,
            None => return Err(format!("ask: argument 'msg' missing")),
        };

        let result_opt = self.single_arg_call(msg)?;
        if result_opt.is_some() {
            return Ok(result_opt);
        }

        match args.get(DEFAULT_ARG) {
            Some(default) => Ok(Some(default.clone())),
            None => Ok(None),
        }
    }
}
