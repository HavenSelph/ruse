use crate::args::ARGS;
use crate::parser::Parser;
use crate::report::ReportChannel;

mod args;
mod ast;
mod debug;
mod files;
mod lexer;
mod parser;
mod report;
mod span;
mod token;

fn main() {
    let reporter = ReportChannel::new();

    let ast = {
        let mut parser = Parser::new(ARGS.input, reporter.get_sender());
        parser.parse()
    };

    dprintln!("{ast}");
    reporter.check_reports();
}
