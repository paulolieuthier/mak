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
    pub value: RightHandSide<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Task<'a> {
    pub name: Ident<'a>,
    pub statements: Vec<Statement<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement<'a> {
    Assignment(Ident<'a>, RightHandSide<'a>),
    Call(Call<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Call<'a> {
    pub callee: Ident<'a>,
    pub args: Args<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Args<'a> {
    Simple(RightHandSide<'a>),
    Named(Vec<Arg<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Arg<'a> {
    pub name: Ident<'a>,
    pub value: RightHandSide<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ident<'a>(pub &'a str);

#[derive(Clone, Debug, PartialEq)]
pub enum RightHandSide<'a> {
    Text(&'a str),
    Number(f32),
    Reference(Ident<'a>),
    // Box: necessary to break struct-field recursion
    Call(Box<Call<'a>>),
}

impl<'a> Display for RightHandSide<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RightHandSide::Text(text) => write!(f, "\"{}\"", text),
            RightHandSide::Number(number) => write!(f, "{}", number),
            RightHandSide::Reference(Ident(ident)) => write!(f, "{}", ident),
            RightHandSide::Call(call) => write!(f, "{}({})", call.callee, call.args),
        }
    }
}

impl<'a> Display for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> Display for Args<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Args::Simple(arg) => write!(f, "{}", arg),
            Args::Named(args) => {
                args.iter().map(|arg| write!(f, "{}, ", arg)).collect()
            },
        }
    }
}

impl<'a> Display for Arg<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}
