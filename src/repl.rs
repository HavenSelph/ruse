use crate::ast::Node;
use crate::dprintln;
use crate::files::push_source;
use crate::interpreter::scope::Scope;
use crate::interpreter::value::Value;
use crate::interpreter::{Interpreter, Ref};
use crate::parser::Parser;
use crate::report::{ReportChannel, ReportLevel};
use crate::span::Span;
use std::io::{stdin, stdout, Write};

const REPL_VERSION: &str = "0.0.1";

#[derive(PartialOrd, PartialEq)]
pub enum ParseResult {
    Ok,
    Incomplete,
    Error,
}

pub struct Repl<'r, 'i> {
    interpreter: &'i mut Interpreter,
    scope: Ref<Scope>,
    report_channel: &'r ReportChannel,
    stdin_id: usize,
}

impl<'r, 'i> Repl<'r, 'i> {
    pub fn new(
        scope: Ref<Scope>,
        interpreter: &'i mut Interpreter,
        report_channel: &'r ReportChannel,
    ) -> Self {
        Self {
            interpreter,
            scope,
            report_channel,
            stdin_id: 0,
        }
    }

    fn try_parse(&self, cache_source: &'static str) -> Result<Box<Node>, ParseResult> {
        let mut parser = match Parser::new(cache_source, self.report_channel.get_sender()) {
            Ok(parser) => parser,
            Err(report) => {
                ReportChannel::display_report(report.finish());
                return Err(ParseResult::Error);
            }
        };
        let ast = parser.parse();
        let mut parse_result = ParseResult::Ok;
        for report in self.report_channel.receiver.try_iter() {
            let this_result: ParseResult = if report.level < ReportLevel::Error {
                ParseResult::Ok
            } else if report.incomplete {
                ParseResult::Incomplete
            } else {
                ReportChannel::display_report(*report.clone());
                ParseResult::Error
            };
            if this_result > parse_result {
                parse_result = this_result;
            }
        }
        match parse_result {
            ParseResult::Ok => Ok(ast),
            _ => Err(parse_result),
        }
    }

    fn run(&mut self) -> Result<(), ()> {
        let mut input = String::new();
        let mut last = String::new();
        let name = Box::leak(format!("<stdin-{:0>3}>", self.stdin_id).into_boxed_str());
        self.stdin_id += 1;
        let ast = loop {
            let mut temp = String::new();
            print!("{}", if input.is_empty() { ">>> " } else { "... " });
            stdout().flush().expect("Failed to flush stdout");
            stdin().read_line(&mut temp).expect("Failed to read line");
            if temp.trim().is_empty() && last.trim().is_empty() {
                return Ok(());
            };
            last = temp.clone();
            input.push_str(&temp);
            push_source(name, input.clone());
            match self.try_parse(name) {
                Ok(ast) => break ast,
                Err(ParseResult::Incomplete) => continue,
                _ => return Err(()),
            }
        };
        dprintln!("{ast}");
        let val = self
            .interpreter
            .run_block(&ast, self.scope.clone())
            .map_err(|err| {
                ReportChannel::display_report(err.finish());
            })
            .unwrap_or(Value::None);
        match val {
            Value::None => {}
            _ => {
                println!("{val}");
                self.scope.borrow_mut().set("_", val, Span::empty());
            }
        }
        Ok(())
    }

    pub fn start_loop(&mut self) {
        println!("Ruse - Made by Haven Selph");
        println!(
            "press Ctrl-C to exit | Repl v{REPL_VERSION} | Lang v{}",
            env!("CARGO_PKG_VERSION")
        );
        loop {
            self.run().ok();
        }
    }
}
