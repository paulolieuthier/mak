use crate::ast;

use nom::{
    branch::alt,
    bytes::complete::*,
    character::complete::*,
    combinator::*,
    error::*,
    multi::*,
    number::complete::*,
    sequence::*,
    IResult,
};

pub fn parse(i: &str) -> Result<ast::Ast, String> {
    parse_proper(i)
        .map(|(_, prog)| prog)
        .map_err(|e| {
            match e {
                nom::Err::Error(e) | nom::Err::Failure(e) => convert_error(i, e),
                _ => unreachable!(),
            }
        })
}

type ParserResult<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

fn parse_proper(i: &str) -> ParserResult<ast::Ast> {
    terminated(map(many0(delimited(any_space, parse_toplevel, any_space)), ast::Ast), eof)(i)
}

#[test]
fn test_parse_proper() {
    assert_eq!(parse(""), Ok(ast::Ast(vec![])));
    assert_eq!(parse(
r#"
# imports
import "xalala"
import "xalalo"

# constant
# another comment line
x = "value"

# task
task task1:
    print(x)
    yo = 123
    print(yo)

task task2:
    print("bye")
"#),
Ok(ast::Ast(vec![
    ast::TopLevelExpr::Import("xalala"),
    ast::TopLevelExpr::Import("xalalo"),
    ast::TopLevelExpr::Constant(ast::Constant { name: ast::Ident("x"), value: ast::Value::Text("value") }),
    ast::TopLevelExpr::Task(ast::Task {
        name: ast::Ident("task1"),
        statements: vec![
            ast::Statement::Call(ast::Ident("print"), vec![ast::Value::Reference(ast::Ident("x"))]),
            ast::Statement::Assignment(ast::Ident("yo"), ast::Value::Number(123f32)),
            ast::Statement::Call(ast::Ident("print"), vec![ast::Value::Reference(ast::Ident("yo"))]),
        ]
    }),
    ast::TopLevelExpr::Task(ast::Task {
        name: ast::Ident("task2"),
        statements: vec![ast::Statement::Call(ast::Ident("print"), vec![ast::Value::Text("bye")])]
    }),
])));
}

fn parse_toplevel(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, _) = parse_toplevel_comments(i)?;
    let (i, _) = space(i)?;
    let (i, toplevel) = alt((parse_toplevel_import, parse_toplevel_constant, parse_toplevel_task))(i)?;
    let (i, _) = space(i)?;
    Ok((i, toplevel))
}

#[test]
fn test_parse_toplevel() {
    assert_eq!(parse_toplevel("import \"xalala\""), Ok(("", ast::TopLevelExpr::Import("xalala"))));
    assert_eq!(parse_toplevel("# comment 1\nimport \"xalala\""), Ok(("", ast::TopLevelExpr::Import("xalala"))));
}

fn parse_toplevel_comments(i: &str) -> ParserResult<&str> {
    let (i, _) = opt(many0(preceded(tag("#"), terminated(not_line_ending, newline_or_eof))))(i)?;
    Ok((i, ""))
}

#[test]
fn test_parse_toplevel_comments() {
    assert_eq!(parse_toplevel_comments("# comment 1\n"), Ok(("", "")));
    assert_eq!(parse_toplevel_comments("# comment 1\n\n\n# comment 2"), Ok(("", "")));
}

fn parse_toplevel_import(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, _) = tag("import")(i)?;
    let (i, _) = space(i)?;
    let (i, import) = string(i)?;
    Ok((i, ast::TopLevelExpr::Import(import)))
}

#[test]
fn test_parse_toplevel_import() {
    assert_eq!(parse_toplevel_import("import \"xalala\""), Ok(("", ast::TopLevelExpr::Import("xalala"))));
    assert_eq!(parse_toplevel_import("import \t \"xalala\""), Ok(("", ast::TopLevelExpr::Import("xalala"))));
    assert_eq!(parse_toplevel_import("import xalala"), Err(nom::Err::Error(VerboseError::from_error_kind("xalala", ErrorKind::Tag))));
}

fn parse_toplevel_constant(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, name) = ident(i)?;
    let (i, _) = space(i)?;
    let (i, _) = char('=')(i)?;
    let (i, _) = space(i)?;
    let (i, value) = ident_or_value(i)?;
    Ok((i, ast::TopLevelExpr::Constant(ast::Constant { name, value })))
}

#[test]
fn test_parse_toplevel_constant() {
    assert_eq!(
        parse_toplevel_constant("xx = 42"),
        Ok(("", ast::TopLevelExpr::Constant(ast::Constant { name: ast::Ident("xx"), value: ast::Value::Number(42f32) }))));
    assert_eq!(
        parse_toplevel_constant("xx = \"42\""),
        Ok(("", ast::TopLevelExpr::Constant(ast::Constant { name: ast::Ident("xx"), value: ast::Value::Text("42") }))));
    assert_eq!(
        parse_toplevel_constant("xx = yy"),
        Ok(("", ast::TopLevelExpr::Constant(ast::Constant { name: ast::Ident("xx"), value: ast::Value::Reference(ast::Ident("yy")) }))));
}

fn parse_toplevel_task(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, name) = preceded(tag("task "), terminated(ident, tag(":")))(i)?;
    let (i, _) = newline(i)?;
    let (_, indent) = peek_indent(i)?;
    let (i, statements) = many1(|i| parse_task_statement(i, indent))(i)?;
    Ok((i, ast::TopLevelExpr::Task(ast::Task { name, statements })))
}

#[test]
fn test_parse_toplevel_task() {
    assert_eq!(
        parse_toplevel_task("task xalala:\n  x = \"y\"\n  print(\"xalalo\")"),
        Ok(("", ast::TopLevelExpr::Task(ast::Task {
            name: ast::Ident("xalala"),
            statements: vec![
                ast::Statement::Assignment(ast::Ident("x"), ast::Value::Text("y")),
                ast::Statement::Call(ast::Ident("print"), vec![ast::Value::Text("xalalo")])]
        }))));
}

fn parse_task_statement<'a>(i: &'a str, indent: &'_ str) -> ParserResult<'a, ast::Statement<'a>> {
    let (i, _) = tag(indent)(i)?;
    let (i, statement) = alt((parse_task_statement_assignment, parse_task_statement_call))(i)?;
    let (i, _) = newline_or_eof(i)?;
    Ok((i, statement))
}

#[test]
fn test_parse_task_statement() {
    assert_eq!(parse_task_statement("  x = \"y\"", "  "), Ok(("", ast::Statement::Assignment(ast::Ident("x"), ast::Value::Text("y")))));
    assert_eq!(parse_task_statement("\tx = \"y\"", "\t"), Ok(("", ast::Statement::Assignment(ast::Ident("x"), ast::Value::Text("y")))));
    assert_eq!(parse_task_statement("\tx = \"y\"\n", "\t"), Ok(("", ast::Statement::Assignment(ast::Ident("x"), ast::Value::Text("y")))));
}

fn parse_task_statement_assignment(i: &str) -> ParserResult<ast::Statement> {
    let (i, name) = ident(i)?;
    let (i, _) = space(i)?;
    let (i, _) = char('=')(i)?;
    let (i, _) = space(i)?;
    let (i, value) = ident_or_value(i)?;
    Ok((i, ast::Statement::Assignment(name, value)))
}

#[test]
fn test_parse_task_statement_assignment() {
    assert_eq!(parse_task_statement_assignment("x = \"y\""), Ok(("", ast::Statement::Assignment(ast::Ident("x"), ast::Value::Text("y")))));
    assert_eq!(parse_task_statement_assignment("x  = \t \"y\""), Ok(("", ast::Statement::Assignment(ast::Ident("x"), ast::Value::Text("y")))));
    assert_eq!(parse_task_statement_assignment("x=\"y\""), Ok(("", ast::Statement::Assignment(ast::Ident("x"), ast::Value::Text("y")))));
}

fn parse_task_statement_call(i: &str) -> ParserResult<ast::Statement> {
    let (i, callee) = ident(i)?;
    let (i, args) = delimited(char('('), separated_list(delimited(opt(space), tag(","), opt(space)), ident_or_value), char(')'))(i)?;
    Ok((i, ast::Statement::Call(callee, args)))
}

#[test]
fn test_parse_task_statement_call() {
    assert_eq!(
        parse_task_statement_call("print(\"xalala\")"), Ok(("", ast::Statement::Call(ast::Ident("print"), vec![ast::Value::Text("xalala")]))));
    assert_eq!(parse_task_statement_call("print(42)"), Ok(("", ast::Statement::Call(ast::Ident("print"), vec![ast::Value::Number(42f32)]))));
    assert_eq!(
        parse_task_statement_call("print(42 , \"xalala\")"),
        Ok(("", ast::Statement::Call(ast::Ident("print"), vec![ast::Value::Number(42f32), ast::Value::Text("xalala")]))));
}

fn eof(i: &str) -> ParserResult<&str> {
    if i.len() == 0 {
        Ok((i, i))
    } else {
        Err(nom::Err::Error(VerboseError::from_error_kind(i, ErrorKind::Eof)))
    }
}

fn space(i: &str) -> ParserResult<&str> {
    let chars = " \t";
    take_while(move |c| chars.contains(c))(i)
}

fn newline(i: &str) -> ParserResult<&str> {
    let chars = "\n";
    take_while(move |c| chars.contains(c))(i)
}

fn newline_or_eof(i: &str) -> ParserResult<&str> {
    alt((newline, eof))(i)
}

fn any_space(i: &str) -> ParserResult<&str> {
    let chars = " \t\n";
    take_while(move |c| chars.contains(c))(i)
}

fn peek_indent(i: &str) -> ParserResult<&str> {
    peek(alt((take_while(move |c| c == ' '), take_while(move |c| c == '\t'))))(i)
}

fn ident(i: &str) -> ParserResult<ast::Ident> {
    verify(peek(take(1usize)), |s: &str| s.chars().nth(0).unwrap().is_alphabetic())(i)?;
    map(alphanumeric1, ast::Ident)(i)
}

#[test]
fn test_ident() {
    assert_eq!(ident("x"), Ok(("", ast::Ident("x"))));
    assert_eq!(ident("xalala"), Ok(("", ast::Ident("xalala"))));
    assert_eq!(ident("1x"), Err(nom::Err::Error(VerboseError::from_error_kind("1x", ErrorKind::Verify))));
}

fn string(i: &str) -> ParserResult<&str> {
    let esc = escaped(none_of("\\\""), '\\', tag("\""));
    let esc_or_empty = alt((esc, tag("")));
    delimited(tag("\""), esc_or_empty, tag("\""))(i)
}

#[test]
fn test_string() {
    assert_eq!(string("\"42\""), Ok(("", "42")));
    assert_eq!(string("\"xalala\""), Ok(("", "xalala")));
    assert_eq!(string("\"xa la la\""), Ok(("", "xa la la")));
}

fn number(i: &str) -> ParserResult<ast::Value> {
    map(float, ast::Value::Number)(i)
}

#[test]
fn test_number() {
    assert_eq!(number("42"), Ok(("", ast::Value::Number(42f32))));
    assert_eq!(number("-42"), Ok(("", ast::Value::Number(-42f32))));
    assert_eq!(number("42.42"), Ok(("", ast::Value::Number(42.42f32))));
}

fn value(i: &str) -> ParserResult<ast::Value> {
    alt((map(string, ast::Value::Text), number))(i)
}

fn ident_or_value(i: &str) -> ParserResult<ast::Value> {
    alt((map(ident, ast::Value::Reference), value))(i)
}
