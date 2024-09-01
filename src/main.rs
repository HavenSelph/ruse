#![allow(clippy::upper_case_acronyms)]
#![warn(clippy::complexity)]

use crate::args::ARGS;
use crate::interpreter::{Interpreter, Scope};
use crate::parser::Parser;
use crate::report::ReportChannel;

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
        let mut parser = Parser::new(ARGS.input(), reporter.get_sender());
        parser.parse()
    };

    dprintln!("{ast}");

    reporter.check_reports();

    let mut interpreter = Interpreter::new();
    match interpreter.run_block(ast.as_ref(), Scope::new(None)) {
        Ok(val) => println!("{}", val),
        Err(err) => reporter.get_sender().report(err.finish().into()),
    }

    reporter.check_reports();
}
