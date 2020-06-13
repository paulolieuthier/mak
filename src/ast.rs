use std::fmt::Debug;
use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'a>(pub Vec<TopLevelExpr<'a>>);

#[derive(Clone, Debug, PartialEq)]
pub enum TopLevelExpr<'a> {
    Import(&'a str),
    Constant(Constant<'a>),
    Task(Task<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Constant<'a> {
    pub name: Ident<'a>,
    pub value: Value<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Task<'a> {
    pub name: Ident<'a>,
    pub statements: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Assignment(Ident<'a>, Value<'a>),
    Call(Ident<'a>, Vec<Value<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident<'a>(pub &'a str);

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    Text(&'a str),
    Number(f32),
    Reference(Ident<'a>),
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Text(text) => write!(f, "\"{}\"", text),
            Value::Number(number) => write!(f, "{}", number),
            Value::Reference(Ident(ident)) => write!(f, "{}", ident),
        }
        
    }
}
