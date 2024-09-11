#![allow(clippy::upper_case_acronyms)]
#![warn(clippy::complexity)]

use crate::args::ARGS;
use crate::interpreter::scope::Scope;
use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::report::{ReportChannel, UnwrapReport};

mod args;
mod ast;
mod debug;
mod files;
mod interpreter;
mod lexer;
mod parser;
mod report;
mod span;
mod token;

fn main() {
    let reporter = ReportChannel::new();

    let ast = {
        let mut parser = Parser::new(ARGS.input(), reporter.get_sender()).unwrap_report();
        parser.parse()
    };

    dprintln!("{ast}");

    reporter.check_reports();

    let mut interpreter = Interpreter::new();
    let scope = Scope::new_hm(None);
    Interpreter::register_builtins_to_scope(scope.clone());
    let val = interpreter.run_block(ast.as_ref(), scope).unwrap_report();
    dprintln!("{val}");
}
