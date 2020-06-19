use crate::ast;
use crate::functions::*;

use std::collections::BTreeMap;
use std::fmt::Display;

#[derive(Clone, Copy, Debug)]
pub enum Value<'a> {
    Text(&'a str),
    Number(f32),
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Text(text) => write!(f, "{}", text),
            Value::Number(number) => write!(f, "{}", number),
        }
    }
}

pub struct Interpreter<'a> {
    constants: BTreeMap<&'a str, Value<'a>>,
    tasks: BTreeMap<&'a str, &'a ast::Task<'a>>,
    functions: BTreeMap<&'a str, &'static dyn Function>,
}

struct ExecutionContext<'a> {
    task: &'a ast::Task<'a>,
    variables: BTreeMap<&'a str, Value<'a>>,
}

pub fn new<'a>(ast: &'a ast::Ast<'a>) -> Result<Interpreter<'a>, String> {
    Interpreter::new(ast)
}

impl<'a> Interpreter<'a> {
    pub fn new(prog: &'a ast::Ast<'a>) -> Result<Interpreter<'a>, String> {
        let mut interpreter = Interpreter {
            constants: BTreeMap::new(),
            tasks: BTreeMap::new(),
            functions: functions().iter().map(|f| (f.name(), *f)).collect(),
        };

        interpreter.load(prog)?;
        Ok(interpreter)
    }

    pub fn run(&self, task: &str) -> Result<(), String> {
        let task = self
            .tasks
            .get(&task[..])
            .ok_or(format!("task '{}' not found", task))?;
        self.run_task(ExecutionContext {
            task,
            variables: BTreeMap::new(),
        })
    }

    fn run_task(&self, mut context: ExecutionContext<'a>) -> Result<(), String> {
        for statement in &context.task.statements {
            match statement {
                ast::Statement::Assignment(ident, value) => {
                    self.run_task_assignment(&mut context, &ident, &value)?
                }
                ast::Statement::Call(ident, args) => self.run_task_call(&context, &ident, args)?,
            }
        }

        Ok(())
    }

    fn run_task_assignment(
        &self,
        context: &mut ExecutionContext<'a>,
        ast::Ident(name): &ast::Ident<'a>,
        value: &ast::Value<'a>,
    ) -> Result<(), String> {
        let value = self.load_value(Some(&context), value)?;
        context.variables.insert(name, value);
        Ok(())
    }

    fn run_task_call(
        &self,
        context: &ExecutionContext<'a>,
        ast::Ident(name): &ast::Ident<'a>,
        args: &ast::Args<'a>,
    ) -> Result<(), String> {
        let function: &dyn Function = *self
            .functions
            .get(name)
            .ok_or(format!("function '{}' not defined", name))?;

        match args {
            ast::Args::Simple(arg) => {
                let arg = self.load_value(Some(&context), arg)?;
                function.single_arg_call(&arg);
            }
            ast::Args::Named(args) => {
                let args = args
                    .iter()
                    .map(|ast::Arg { name, value }| {
                        self.load_value(Some(&context), value)
                            .map(|value| (name.0, value))
                    })
                    .collect::<Result<BTreeMap<&str, Value<'a>>, String>>()?;
                function.named_args_call(args);
            }
        }
        Ok(())
    }

    fn load(&mut self, ast::Ast(exprs): &'a ast::Ast<'a>) -> Result<(), String> {
        for expr in exprs {
            match expr {
                ast::TopLevelExpr::Import(_) => {
                    println!("import!");
                    ()
                }
                ast::TopLevelExpr::Constant(constant) => self.load_constant(&constant)?,
                ast::TopLevelExpr::Task(task) => self.load_task(&task)?,
            }
        }

        Ok(())
    }

    fn load_constant(
        &mut self,
        ast::Constant { name, value }: &ast::Constant<'a>,
    ) -> Result<(), String> {
        let ast::Ident(name) = name;
        let value = self.load_value(None, value)?;
        self.constants.insert(name, value);
        Ok(())
    }

    fn load_task(&mut self, task: &'a ast::Task<'a>) -> Result<(), String> {
        let ast::Ident(name) = task.name;
        self.tasks.insert(name, task);
        Ok(())
    }

    fn load_value(
        &self,
        context: Option<&ExecutionContext<'a>>,
        value: &ast::Value<'a>,
    ) -> Result<Value<'a>, String> {
        match value {
            ast::Value::Text(text) => Ok(Value::Text(text)),
            ast::Value::Number(number) => Ok(Value::Number(*number)),
            ast::Value::Reference(ast::Ident(ident)) => self
                .lookup_reference(context, ident)
                .ok_or(format!("Can't handle value: {}", value)),
        }
    }

    fn lookup_reference(
        &self,
        context: Option<&ExecutionContext<'a>>,
        ident: &'a str,
    ) -> Option<Value<'a>> {
        context
            .and_then(|c| c.variables.get(ident))
            .or_else(|| self.constants.get(ident))
            .map(|r| *r)
    }
}
