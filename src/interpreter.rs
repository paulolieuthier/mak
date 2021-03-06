use crate::ast;
use crate::functions::*;
use crate::value::*;

use std::collections::BTreeMap;

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
                    self.run_task_assignment(&mut context, &ident, &value)?;
                }
                ast::Statement::Call(call) => {
                    self.run_task_call(&context, &call.callee, &call.args)?;
                }
            }
        }

        Ok(())
    }

    fn run_task_assignment(
        &self,
        context: &mut ExecutionContext<'a>,
        ast::Ident(name): &ast::Ident<'a>,
        value: &ast::RightHandSide<'a>,
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
    ) -> Result<Option<Value<'a>>, String> {
        let function: &dyn Function = *self
            .functions
            .get(name)
            .ok_or(format!("function '{}' not defined", name))?;

        match args {
            ast::Args::Simple(arg) => {
                let arg = self.load_value(Some(&context), arg)?;
                function.single_arg_call(&arg)
            }
            ast::Args::Named(args) => {
                let args = args
                    .iter()
                    .map(|ast::Arg { name, value }| {
                        self.load_value(Some(&context), value)
                            .map(|value| (name.0, value))
                    })
                    .collect::<Result<BTreeMap<&str, Value<'a>>, String>>()?;
                function.named_args_call(args)
            }
        }
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
        value: &ast::RightHandSide<'a>,
    ) -> Result<Value<'a>, String> {
        match value {
            ast::RightHandSide::Text(text) => match &text {
                ast::Text::Simple(text) => Ok(Value::from_text(*text)),
                ast::Text::Complex(ast::Interpolation { content, vars }) => {
                    let var_and_values = vars
                        .iter()
                        .map(|var| {
                            let ast::Ident(ident) = var.name;
                            self.lookup_reference(context, ident)
                                .map(|value| (var, value))
                                .ok_or(format!("Can't handle value: {}", ident))
                        })
                        .collect::<Result<Vec<(&ast::InterpolationVar<'a>, Value<'a>)>, String>>(
                        )?;

                    // replace variables in string using ranges from parser
                    // as ranges appear in order and don't overlap, it's safe to use the accumulated delta got from replacing variables by values of different length
                    let mut content = content.clone();
                    let mut shift = 0i32;
                    for (var, value) in var_and_values {
                        let range_len = var.end as i32 - var.start as i32;
                        let value = &format!("{}", value)[..];
                        content.replace_range(
                            (var.start as i32 + shift) as usize..(var.end as i32 + shift) as usize,
                            value,
                        );
                        shift = shift + value.len() as i32 - range_len;
                    }
                    Ok(Value::from_text(content))
                }
            },
            ast::RightHandSide::Number(number) => Ok(Value::from_number(*number)),
            ast::RightHandSide::Reference(ast::Ident(ident)) => self
                .lookup_reference(context, ident)
                .ok_or(format!("Can't handle value: {}", value)),
            ast::RightHandSide::Call(call) => {
                self.run_task_call(
                    context.ok_or(format!("Function call now allowed here"))?,
                    &call.callee,
                    &call.args,
                )
                .and_then(|value| value.ok_or(format!("Attempted to use value from void function")))
            }
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
            .map(|v| v.clone())
    }
}
