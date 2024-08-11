#![allow(dead_code, unused)]

use crate::args::ARGS;
use crate::files::get_source;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::report::{ReportChannel, UnwrapReport};

mod args;
mod ast;
mod debug;
mod files;
mod lexer;
mod parser;
mod report;
mod span;
mod token;
mod unbox;

fn main() {
    let reporter = ReportChannel::new();
    let tokens = {
        let mut lexer = Lexer::new(
            ARGS.input,
            get_source(ARGS.input).unwrap_report().text(),
            reporter.get_sender(),
        );
        lexer.lex();
        lexer.tokens
    };

    dprintln!("Tokens [");
    tokens.iter().for_each(|t| dprintln!(" {t:#?}"));
    dprintln!("]");
    reporter.check_reports();

    let ast = {
        let mut parser = Parser::new(ARGS.input, tokens, reporter.get_sender());
        parser.parse()
    };

    dprintln!("{ast}");
    reporter.check_reports();
}
