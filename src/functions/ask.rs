use super::*;
use lazy_static::*;
use maplit::*;
use std::collections::{BTreeMap, BTreeSet};
use std::io;

pub struct AskFunction;

const MSG_ARG: &'static str = "msg";
const DEFAULT_ARG: &'static str = "default";

lazy_static! {
    static ref ARGS: BTreeSet<&'static str> = btreeset![MSG_ARG, DEFAULT_ARG];
}

impl Function for AskFunction {
    fn name(&self) -> &'static str {
        "ask"
    }

    fn single_arg_call<'a>(&self, arg: &Value<'a>) -> Result<Option<Value<'a>>, String> {
        print!("{}", arg);
        self.flush_stdout()?;
        let mut input = String::new();
        let without_newline = match io::stdin().read_line(&mut input) {
            Ok(_) => input[0..input.len() - 1].to_owned(),
            Err(_) => return Err(format!("ask: failure to get user input")),
        };

        Ok(Some(Value::from_text(without_newline)))
    }

    fn named_args_call<'a>(
        &self,
        args: BTreeMap<&'a str, Value<'a>>,
    ) -> Result<Option<Value<'a>>, String> {
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
