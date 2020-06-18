mod print;

use crate::interpreter::*;
use std::collections::BTreeMap;

pub trait Function {
    fn name(&self) -> &'static str;
    fn single_arg_call(&self, arg: &Value);
    fn named_args_call(&self, args: BTreeMap<&str, Value>);
}

const FUNCTIONS: &'static [&'static dyn Function] = &[&print::PrintFunction];

pub fn functions() -> &'static [&'static dyn Function] {
    FUNCTIONS
}
