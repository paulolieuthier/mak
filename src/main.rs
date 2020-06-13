pub mod ast;
pub mod parser;
pub mod interpreter;

use std::env;
use std::fs;

const FILE_NAME: &str = "makfile";

fn main() -> Result<(), String> {
    if env::args().len() < 2 {
        panic!("No task to run".to_string())
    }

    let contents = fs::read_to_string(FILE_NAME).expect("makfile not found");
    let ast = parser::parse(&contents[..])?;
    let interpreter = interpreter::new(&ast)?;
    for task in env::args().skip(1) {
        interpreter.borrow().run(task)?;
    }

    Ok(())
}
