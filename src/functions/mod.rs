use crate::value::*;
use std::collections::BTreeMap;
use std::io;
use std::io::Write;

mod ask;
mod print;

pub trait Function {
    fn name(&self) -> &'static str;
    fn single_arg_call<'a>(&self, arg: &Value<'a>) -> Result<Option<Value<'a>>, String>;
    fn named_args_call<'a>(
        &self,
        args: BTreeMap<&'a str, Value<'a>>,
    ) -> Result<Option<Value<'a>>, String>;

    fn flush_stdout(&self) -> Result<(), String> {
        io::stdout()
            .flush()
            .map_err(|_| format!("{}: failure to write to stdout", self.name()))?;
        Ok(())
    }
}

const FUNCTIONS: &'static [&'static dyn Function] = &[&print::PrintFunction, &ask::AskFunction];

pub fn functions() -> &'static [&'static dyn Function] {
    FUNCTIONS
}
