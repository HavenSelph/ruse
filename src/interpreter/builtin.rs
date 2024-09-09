use crate::interpreter::trash::GC;
use crate::interpreter::value::{BuiltInFunction, Function, FunctionArg, FunctionRun, Value};
use crate::interpreter::{Ref, Scope};
use crate::ref_it;
use crate::report::Result;
use crate::span::Span;
use indexmap::IndexMap;
use std::sync::Arc;

macro_rules! get_args {
    ($scope:expr, $span:expr, $($key:ident),+$(,)?) => {
        (
            $(
            $scope.read().unwrap().get(stringify!($key), $span)?,
            )+
        )
    };
}

pub fn print() -> Ref<Function> {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (vals, sep, end) = get_args!(scope, span, vals, sep, end);
        let Value::Tuple(vals) = vals.read().unwrap().clone() else {
            unreachable!();
        };
        let (sep, end) = (
            sep.read().unwrap().as_string(),
            end.read().unwrap().as_string(),
        );

        for (i, val) in vals.read().unwrap().iter().enumerate() {
            if i != 0 {
                print!("{sep}");
            }
            print!("{val}");
        }
        print!("{end}");
        Ok(Value::None)
    }
    RuseBuiltIn::new("print")
        .with_arg(BuiltInFunctionArg::PositionalVariadic("vals"))
        .with_arg(BuiltInFunctionArg::Keyword(
            "sep",
            Value::String(Arc::from(" ")),
        ))
        .with_arg(BuiltInFunctionArg::Keyword(
            "end",
            Value::String(Arc::from("\n")),
        ))
        .finish(Box::new(execute))
}

pub fn debug() -> Ref<Function> {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        let val = val.read().unwrap();
        println!("[DEBUG {span}] {val}",);
        Ok(Value::clone(&val))
    }
    RuseBuiltIn::new("debug")
        .with_arg(BuiltInFunctionArg::Positional("val"))
        .finish(Box::new(execute))
}

struct RuseBuiltIn {
    name: String,
    args: Vec<BuiltInFunctionArg>,
}

#[allow(dead_code)]
enum BuiltInFunctionArg {
    Positional(&'static str),
    PositionalVariadic(&'static str),
    Keyword(&'static str, Value),
    KeywordVariadic(&'static str),
}

impl RuseBuiltIn {
    fn new<T: ToString>(name: T) -> Self {
        Self {
            name: name.to_string(),
            args: Vec::new(),
        }
    }

    fn with_arg(mut self, arg: BuiltInFunctionArg) -> Self {
        self.args.push(arg);
        self
    }

    fn finish(self, f: BuiltInFunction) -> Ref<Function> {
        let span = Span::empty();
        let run = FunctionRun::BuiltIn(f);
        let mut args = IndexMap::new();
        for arg in self.args {
            let (name, arg) = match arg {
                BuiltInFunctionArg::Positional(name) => (name, FunctionArg::Positional(span)),
                BuiltInFunctionArg::PositionalVariadic(name) => {
                    (name, FunctionArg::PositionalVariadic(span))
                }
                BuiltInFunctionArg::Keyword(name, default) => (
                    name,
                    FunctionArg::Keyword(span, GC.write().unwrap().monitor(default)),
                ),
                BuiltInFunctionArg::KeywordVariadic(name) => {
                    (name, FunctionArg::KeywordVariadic(()))
                }
            };
            args.insert(name.to_string(), arg);
        }
        ref_it!(Function {
            span,
            name: Some(self.name),
            args,
            scope: Scope::new(None),
            run,
        })
    }
}
