#![allow(clippy::upper_case_acronyms)]
#![warn(clippy::complexity)]

use crate::args::ARGS;
use crate::interpreter::scope::Scope;
use crate::interpreter::{Interpreter, Ref};
use crate::parser::Parser;
use crate::repl::Repl;
use crate::report::{ReportChannel, UnwrapReport};

mod args;
mod ast;
mod debug;
mod files;
mod interpreter;
mod lexer;
mod parser;
mod repl;
mod report;
mod span;
mod token;

fn run_file(
    file_name: &'static str,
    scope: Ref<Scope>,
    interpreter: &mut Interpreter,
    reporter: &ReportChannel,
) {
    let ast = {
        let mut parser = Parser::new(file_name, reporter.get_sender()).unwrap_report();
        parser.parse()
    };

    dprintln!("{ast}");

    reporter.check_reports();

    let val = interpreter.run_block(ast.as_ref(), scope).unwrap_report();
    dprintln!("{val:?}");
}

fn main() {
    let reporter = ReportChannel::new();
    let scope = Scope::new_hm(None);
    let mut interpreter = Interpreter::new();
    Interpreter::register_builtins_to_scope(scope.clone());
    if let Some(filename) = ARGS.input() {
        run_file(filename, scope.clone(), &mut interpreter, &reporter);
    }
    if ARGS.repl() || ARGS.input().is_none() {
        Repl::new(scope, &mut interpreter, &reporter).start_loop()
    }
}
