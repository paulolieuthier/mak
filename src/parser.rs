use crate::ast;

use nom::{
    branch::alt, bytes::complete::*, character::complete::*, combinator::*, error::*, multi::*,
    number::complete::*, sequence::*, IResult,
};

pub fn parse(i: &str) -> Result<ast::Ast, String> {
    parse_proper(i).map(|(_, prog)| prog).map_err(|e| match e {
        nom::Err::Error(e) | nom::Err::Failure(e) => convert_error(i, e),
        _ => unreachable!(),
    })
}

type ParserResult<'a, O> = IResult<&'a str, O, VerboseError<&'a str>>;

fn parse_proper(i: &str) -> ParserResult<ast::Ast> {
    terminated(
        map(
            many0(delimited(any_space, parse_toplevel, any_space)),
            ast::Ast,
        ),
        eof,
    )(i)
}

#[test]
fn test_parse_proper() {
    assert_eq!(parse(""), Ok(ast::Ast(vec![])));
    assert_eq!(
        parse(
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
    print(msg: "named arg")
"#
        ),
        Ok(ast::Ast(vec![
            ast::TopLevelExpr::Import("xalala"),
            ast::TopLevelExpr::Import("xalalo"),
            ast::TopLevelExpr::Constant(ast::Constant {
                name: ast::Ident("x"),
                value: ast::RightHandSide::Text("value")
            }),
            ast::TopLevelExpr::Task(ast::Task {
                name: ast::Ident("task1"),
                statements: vec![
                    ast::Statement::Call(ast::Call {
                        callee: ast::Ident("print"),
                        args: ast::Args::Simple(ast::RightHandSide::Reference(ast::Ident("x")))
                    }),
                    ast::Statement::Assignment(ast::Ident("yo"), ast::RightHandSide::Number(123f32)),
                    ast::Statement::Call(ast::Call {
                        callee: ast::Ident("print"),
                        args: ast::Args::Simple(ast::RightHandSide::Reference(ast::Ident("yo")))
                    }),
                ]
            }),
            ast::TopLevelExpr::Task(ast::Task {
                name: ast::Ident("task2"),
                statements: vec![ast::Statement::Call(ast::Call {
                    callee: ast::Ident("print"),
                    args: ast::Args::Named(vec![ast::Arg {
                        name: ast::Ident("msg"),
                        value: ast::RightHandSide::Text("named arg")
                    }])
                })]
            }),
        ]))
    );
}

fn parse_toplevel(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, _) = opt(many0(parse_comments))(i)?;
    let (i, _) = space(i)?;
    let (i, toplevel) = alt((
        parse_toplevel_import,
        parse_toplevel_constant,
        parse_toplevel_task,
    ))(i)?;
    let (i, _) = space(i)?;
    let (i, _) = opt(parse_comments)(i)?;
    let (i, _) = space(i)?;
    Ok((i, toplevel))
}

#[test]
fn test_parse_toplevel() {
    assert_eq!(
        parse_toplevel("import \"xalala\""),
        Ok(("", ast::TopLevelExpr::Import("xalala")))
    );
    assert_eq!(
        parse_toplevel("# comment 1\n# comment 2\nimport \"xalala\""),
        Ok(("", ast::TopLevelExpr::Import("xalala")))
    );
    assert_eq!(
        parse_toplevel("import \"xalala\" # comment on import"),
        Ok(("", ast::TopLevelExpr::Import("xalala")))
    );
    assert_eq!(
        parse_toplevel("import \"xalala\" # comment on import\n"),
        Ok(("", ast::TopLevelExpr::Import("xalala")))
    );
}

fn parse_toplevel_import(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, _) = tag("import")(i)?;
    let (i, _) = space(i)?;
    let (i, import) = string(i)?;
    Ok((i, ast::TopLevelExpr::Import(import)))
}

#[test]
fn test_parse_toplevel_import() {
    assert_eq!(
        parse_toplevel_import("import \"xalala\""),
        Ok(("", ast::TopLevelExpr::Import("xalala")))
    );
    assert_eq!(
        parse_toplevel_import("import \t \"xalala\""),
        Ok(("", ast::TopLevelExpr::Import("xalala")))
    );
    assert_eq!(
        parse_toplevel_import("import xalala"),
        Err(nom::Err::Error(VerboseError::from_error_kind(
            "xalala",
            ErrorKind::Tag
        )))
    );
}

fn parse_toplevel_constant(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, name) = ident(i)?;
    let (i, _) = space(i)?;
    let (i, _) = char('=')(i)?;
    let (i, _) = space(i)?;
    let (i, value) = right_hand_side(i)?;
    Ok((
        i,
        ast::TopLevelExpr::Constant(ast::Constant { name, value }),
    ))
}

#[test]
fn test_parse_toplevel_constant() {
    assert_eq!(
        parse_toplevel_constant("xx = 42"),
        Ok((
            "",
            ast::TopLevelExpr::Constant(ast::Constant {
                name: ast::Ident("xx"),
                value: ast::RightHandSide::Number(42f32)
            })
        ))
    );
    assert_eq!(
        parse_toplevel_constant("xx = \"42\""),
        Ok((
            "",
            ast::TopLevelExpr::Constant(ast::Constant {
                name: ast::Ident("xx"),
                value: ast::RightHandSide::Text("42")
            })
        ))
    );
    assert_eq!(
        parse_toplevel_constant("xx = yy"),
        Ok((
            "",
            ast::TopLevelExpr::Constant(ast::Constant {
                name: ast::Ident("xx"),
                value: ast::RightHandSide::Reference(ast::Ident("yy"))
            })
        ))
    );
}

fn parse_toplevel_task(i: &str) -> ParserResult<ast::TopLevelExpr> {
    let (i, _) = tag("task")(i)?;
    let (i, _) = space(i)?;
    let (i, name) = ident(i)?;
    let (i, _) = space(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = space(i)?;
    let (i, _) = newline(i)?;
    let (_, indent) = peek_indent(i)?;
    let (i, statements) = many1(|i| parse_task_statement(i, indent))(i)?;
    Ok((i, ast::TopLevelExpr::Task(ast::Task { name, statements })))
}

#[test]
fn test_parse_toplevel_task() {
    assert_eq!(
        parse_toplevel_task("task xalala:\n  x = \"y\"\n  print(\"xalalo\")"),
        Ok((
            "",
            ast::TopLevelExpr::Task(ast::Task {
                name: ast::Ident("xalala"),
                statements: vec![
                    ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Text("y")),
                    ast::Statement::Call(ast::Call {
                        callee: ast::Ident("print"),
                        args: ast::Args::Simple(ast::RightHandSide::Text("xalalo"))
                    })
                ]
            })
        ))
    );
}

fn parse_task_statement<'a>(i: &'a str, indent: &'_ str) -> ParserResult<'a, ast::Statement<'a>> {
    let (i, _) = tag(indent)(i)?;
    let (i, statement) = alt((parse_task_statement_assignment, map(parse_call, ast::Statement::Call)))(i)?;
    let (i, _) = newline_or_eof(i)?;
    Ok((i, statement))
}

#[test]
fn test_parse_task_statement() {
    assert_eq!(
        parse_task_statement("  x = \"y\"", "  "),
        Ok((
            "",
            ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Text("y"))
        ))
    );
    assert_eq!(
        parse_task_statement("\tx = \"y\"", "\t"),
        Ok((
            "",
            ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Text("y"))
        ))
    );
    assert_eq!(
        parse_task_statement("\tx = \"y\"\n", "\t"),
        Ok((
            "",
            ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Text("y"))
        ))
    );
}

fn parse_task_statement_assignment(i: &str) -> ParserResult<ast::Statement> {
    let (i, name) = ident(i)?;
    let (i, _) = space(i)?;
    let (i, _) = char('=')(i)?;
    let (i, _) = space(i)?;
    let (i, value) = right_hand_side(i)?;
    Ok((i, ast::Statement::Assignment(name, value)))
}

#[test]
fn test_parse_task_statement_assignment() {
    assert_eq!(
        parse_task_statement_assignment("x = \"y\""),
        Ok((
            "",
            ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Text("y"))
        ))
    );
    assert_eq!(
        parse_task_statement_assignment("x  = \t \"y\""),
        Ok((
            "",
            ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Text("y"))
        ))
    );
    assert_eq!(
        parse_task_statement_assignment("x=\"y\""),
        Ok((
            "",
            ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Text("y"))
        ))
    );
    assert_eq!(
        parse_task_statement_assignment("x = fn(\"y\")"),
        Ok((
            "",
            ast::Statement::Assignment(ast::Ident("x"), ast::RightHandSide::Call(Box::new(ast::Call { callee: ast::Ident("fn"), args: ast::Args::Simple(ast::RightHandSide::Text("y")) })))
        ))
    );
}

fn parse_call(i: &str) -> ParserResult<ast::Call> {
    let (i, callee) = ident(i)?;
    let (i, _) = space(i)?;

    let simple_arg_parser = complete(delimited(
        char('('),
        delimited(space, parse_call_simple_args, space),
        char(')'),
    ));
    let named_args_parser = complete(delimited(
        char('('),
        delimited(space, parse_call_named_args, space),
        char(')'),
    ));
    let (i, args) = alt((named_args_parser, simple_arg_parser))(i)?;

    Ok((i, ast::Call { callee, args }))
}

#[test]
fn test_parse_call() {
    assert_eq!(
        parse_call("print(\"xalala\")"),
        Ok((
            "",
            ast::Call {
                callee: ast::Ident("print"),
                args: ast::Args::Simple(ast::RightHandSide::Text("xalala"))
            }
        ))
    );
    assert_eq!(
        parse_call("print(arg: \"xalala\")"),
        Ok((
            "",
            ast::Call {
                callee: ast::Ident("print"),
                args: ast::Args::Named(vec![ast::Arg {
                    name: ast::Ident("arg"),
                    value: ast::RightHandSide::Text("xalala")
                }])
            }
        ))
    );
    assert_eq!(
        parse_call("print(42)"),
        Ok((
            "",
            ast::Call {
                callee: ast::Ident("print"),
                args: ast::Args::Simple(ast::RightHandSide::Number(42f32))
            }
        ))
    );
    assert_eq!(
        parse_call("print(arg: 42)"),
        Ok((
            "",
            ast::Call {
                callee: ast::Ident("print"),
                args: ast::Args::Named(vec![ast::Arg {
                    name: ast::Ident("arg"),
                    value: ast::RightHandSide::Number(42f32)
                }])
            }
        ))
    );
    assert_eq!(
        parse_call("print(arg1: 42, arg2: \"xalala\")"),
        Ok((
            "",
            ast::Call {
                callee: ast::Ident("print"),
                args: ast::Args::Named(vec![
                    ast::Arg {
                        name: ast::Ident("arg1"),
                        value: ast::RightHandSide::Number(42f32)
                    },
                    ast::Arg {
                        name: ast::Ident("arg2"),
                        value: ast::RightHandSide::Text("xalala")
                    }
                ])
            }
        ))
    );
}

fn parse_call_simple_args(i: &str) -> ParserResult<ast::Args> {
    let (i, arg) = right_hand_side(i)?;
    Ok((i, ast::Args::Simple(arg)))
}

#[test]
fn test_parse_call_simple_args() {
    assert_eq!(
        parse_call_simple_args("42"),
        Ok(("", ast::Args::Simple(ast::RightHandSide::Number(42f32))))
    );
    assert_eq!(
        parse_call_simple_args("\"value\""),
        Ok(("", ast::Args::Simple(ast::RightHandSide::Text("value"))))
    );
}

fn parse_call_named_args(i: &str) -> ParserResult<ast::Args> {
    let (i, args) = complete(separated_list(
        delimited(space, tag(","), space),
        parse_call_named_args_arg,
    ))(i)?;
    Ok((i, ast::Args::Named(args)))
}

#[test]
fn test_parse_call_named_args() {
    assert_eq!(
        parse_call_named_args("arg:\"value\""),
        Ok((
            "",
            ast::Args::Named(vec![ast::Arg {
                name: ast::Ident("arg"),
                value: ast::RightHandSide::Text("value")
            }])
        ))
    );
    assert_eq!(
        parse_call_named_args("arg: \"value\""),
        Ok((
            "",
            ast::Args::Named(vec![ast::Arg {
                name: ast::Ident("arg"),
                value: ast::RightHandSide::Text("value")
            }])
        ))
    );
    assert_eq!(
        parse_call_named_args("arg1: \"value1\", arg2: \"value2\""),
        Ok((
            "",
            ast::Args::Named(vec![
                ast::Arg {
                    name: ast::Ident("arg1"),
                    value: ast::RightHandSide::Text("value1")
                },
                ast::Arg {
                    name: ast::Ident("arg2"),
                    value: ast::RightHandSide::Text("value2")
                }
            ])
        ))
    );
}

fn parse_call_named_args_arg(i: &str) -> ParserResult<ast::Arg> {
    let (i, name) = ident(i)?;
    let (i, _) = tag(":")(i)?;
    let (i, _) = space(i)?;
    let (i, value) = right_hand_side(i)?;
    Ok((i, ast::Arg { name, value }))
}

#[test]
fn test_parse_call_named_args_arg() {
    assert_eq!(
        parse_call_named_args_arg("arg:\"value\""),
        Ok((
            "",
            ast::Arg {
                name: ast::Ident("arg"),
                value: ast::RightHandSide::Text("value")
            }
        ))
    );
    assert_eq!(
        parse_call_named_args_arg("arg:  \"value\""),
        Ok((
            "",
            ast::Arg {
                name: ast::Ident("arg"),
                value: ast::RightHandSide::Text("value")
            }
        ))
    );
}

fn parse_comments(i: &str) -> ParserResult<&str> {
    let (i, _) = preceded(tag("#"), terminated(not_line_ending, newline_or_eof))(i)?;
    Ok((i, ""))
}

#[test]
fn test_parse_comments() {
    assert_eq!(parse_comments("# comment line"), Ok(("", "")));
    assert_eq!(parse_comments("# comment line\n"), Ok(("", "")));
}

fn eof(i: &str) -> ParserResult<&str> {
    if i.len() == 0 {
        Ok((i, i))
    } else {
        Err(nom::Err::Error(VerboseError::from_error_kind(
            i,
            ErrorKind::Eof,
        )))
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
    peek(alt((
        take_while(move |c| c == ' '),
        take_while(move |c| c == '\t'),
    )))(i)
}

fn ident(i: &str) -> ParserResult<ast::Ident> {
    verify(peek(take(1usize)), |s: &str| {
        s.chars().nth(0).unwrap().is_alphabetic()
    })(i)?;
    map(alphanumeric1, ast::Ident)(i)
}

#[test]
fn test_ident() {
    assert_eq!(ident("x"), Ok(("", ast::Ident("x"))));
    assert_eq!(ident("xalala"), Ok(("", ast::Ident("xalala"))));
    assert_eq!(
        ident("1x"),
        Err(nom::Err::Error(VerboseError::from_error_kind(
            "1x",
            ErrorKind::Verify
        )))
    );
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

fn number(i: &str) -> ParserResult<ast::RightHandSide> {
    map(float, ast::RightHandSide::Number)(i)
}

#[test]
fn test_number() {
    assert_eq!(number("42"), Ok(("", ast::RightHandSide::Number(42f32))));
    assert_eq!(number("-42"), Ok(("", ast::RightHandSide::Number(-42f32))));
    assert_eq!(number("42.42"), Ok(("", ast::RightHandSide::Number(42.42f32))));
}

fn value(i: &str) -> ParserResult<ast::RightHandSide> {
    let string_parser = map(string, ast::RightHandSide::Text);
    alt((complete(string_parser), complete(number)))(i)
}

fn right_hand_side(i: &str) -> ParserResult<ast::RightHandSide> {
    let ident_parser = map(ident, ast::RightHandSide::Reference);
    let call_parser = map(parse_call, |call| ast::RightHandSide::Call(Box::new(call)));
    alt((complete(call_parser), complete(ident_parser), complete(value)))(i)
}
