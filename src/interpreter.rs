use crate::ast;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Clone, Copy)]
enum Value<'a> {
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

struct Task<'a>(&'a Vec<ast::Statement<'a>>);

trait Function {
    fn call(&self, args: Vec<Value>);
}

struct PrintFunction;
impl Function for PrintFunction {
    fn call(&self, args: Vec<Value>) {
        for arg in &args {
            println!("{}", arg);
        }
    }
}

pub struct Interpreter<'a> {
    constants: HashMap<&'a str, Value<'a>>,
    tasks: HashMap<&'a str, Task<'a>>,
    functions: HashMap<&'a str, Box<dyn Function>>,
}

struct ExecutionContext<'a> {
    task: &'a Task<'a>,
    variables: HashMap<&'a str, Value<'a>>,
}

pub fn new<'a>(ast: &'a ast::Ast<'a>) -> Result<RefCell<Interpreter<'a>>, String> {
    Interpreter::new(ast)
}

impl<'a> Interpreter<'a> {
    pub fn new(prog: &'a ast::Ast<'a>) -> Result<RefCell<Interpreter<'a>>, String> {
        let interpreter = RefCell::new(Interpreter {
            constants: HashMap::new(),
            tasks: HashMap::new(),
            functions: HashMap::new(),
        });

        interpreter.borrow_mut().load(prog)?;
        interpreter.borrow_mut().functions.insert("print", Box::new(PrintFunction));
        Ok(interpreter)
    }

    pub fn run(&self, task: String) -> Result<(), String> {
        let task = self.tasks.get(&task[..]).ok_or(format!("task '{}' not found", task))?;
        let context = RefCell::new(ExecutionContext { task, variables: HashMap::new() });
        self.run_task(context)
    }

    fn run_task(&self, context: RefCell<ExecutionContext<'a>>) -> Result<(), String> {
        let Task(statements) = context.borrow().task;
        for statement in *statements {
            let res = match statement {
                ast::Statement::Assignment(ident, value) => self.run_task_assignment(&context, &ident, &value),
                ast::Statement::Call(ident, args) => self.run_task_call(&context, &ident, &args)
            };

            if res.is_err() {
                return res;
            }
        }

        Ok(())
    }

    fn run_task_assignment(&self, context: &RefCell<ExecutionContext<'a>>, ast::Ident(name): &ast::Ident<'a>, value: &ast::Value<'a>) -> Result<(), String> {
        let value = self.load_value(Some(&context), value)?;
        context.borrow_mut().variables.insert(name, value);
        Ok(())
    }

    fn run_task_call(&self, context: &RefCell<ExecutionContext<'a>>, ast::Ident(name): &ast::Ident<'a>, args: &Vec<ast::Value<'a>>) -> Result<(), String> {
        let function: &Box<dyn Function> = self.functions.get(name).ok_or(format!("function '{}' not defined", name))?;
        let args: Result<Vec<_>, _> = args.iter().map(|arg| self.load_value(Some(context), arg)).collect();
        function.call(args?);
        Ok(())
    }

    fn load(&mut self, ast::Ast(exprs): &'a ast::Ast<'a>) -> Result<(), String> {
        for expr in exprs {
            let res = match expr {
                ast::TopLevelExpr::Import(_) => { println!("import!"); Ok(()) },
                ast::TopLevelExpr::Constant(constant) => self.load_constant(&constant),
                ast::TopLevelExpr::Task(task) => self.load_task(&task),
            };

            if res.is_err() {
                return res;
            }
        }

        Ok(())
    }

    fn load_constant(&mut self, ast::Constant { name, value }: &ast::Constant<'a>) -> Result<(), String> {
        let ast::Ident(name) = name;
        let value = self.load_value(None, value)?;
        self.constants.insert(name, value);
        Ok(())
    }

    fn load_task(&mut self, ast::Task { name, statements }: &'a ast::Task<'a>) -> Result<(), String> {
        let ast::Ident(name) = name;
        self.tasks.insert(name, Task(statements));
        Ok(())
    }

    fn load_value(&self, context: Option<&RefCell<ExecutionContext<'a>>>, value: &ast::Value<'a>) -> Result<Value<'a>, String> {
        match value {
            ast::Value::Text(text) => Ok(Value::Text(text)),
            ast::Value::Number(number) => Ok(Value::Number(*number)),
            ast::Value::Reference(ast::Ident(ident)) => self.lookup_reference(context, ident).ok_or(format!("Can't handle value: {}", value)),
        }
    }

    fn lookup_reference(&self, context: Option<&RefCell<ExecutionContext<'a>>>, ident: &'a str) -> Option<Value<'a>> {
        // less magic here, please
        context.and_then(|c| Some(*c.borrow().variables.get(ident)?))
            .or_else(|| Some(*self.constants.get(ident)?))
    }
}
