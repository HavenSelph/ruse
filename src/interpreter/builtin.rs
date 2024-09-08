use crate::interpreter::value::{Function, FunctionArg, FunctionRun, Value};
use crate::interpreter::{Ref, Scope};
use crate::ref_it;
use crate::report::Result;
use crate::span::Span;
use indexmap::IndexMap;
use std::rc::Rc;

macro_rules! get_args {
    ($scope:expr, $span:expr, $($key:ident),+$(,)?) => {
        (
            $(
            $scope.borrow().get(stringify!($key), $span)?,
            )+
        )
    };
}

pub fn print() -> Ref<Function> {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (Value::Tuple(vals), sep, end) = get_args!(scope, span, vals, sep, end) else {
            unreachable!();
        };
        let (sep, end) = (sep.as_string(), end.as_string());

        for (i, val) in vals.borrow().iter().enumerate() {
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
            Value::String(Rc::new(" ".to_string())),
        ))
        .with_arg(BuiltInFunctionArg::Keyword(
            "end",
            Value::String(Rc::new("\n".to_string())),
        ))
        .finish(&execute)
}

pub fn debug() -> Ref<Function> {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        println!("[DEBUG {span}] {val}");
        Ok(val)
    }
    RuseBuiltIn::new("debug")
        .with_arg(BuiltInFunctionArg::Positional("val"))
        .finish(&execute)
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

    fn finish(self, f: &'static dyn Fn(Ref<Scope>, Span) -> Result<Value>) -> Ref<Function> {
        let span = Span::empty();
        let run = FunctionRun::BuiltIn(f);
        let mut args = IndexMap::new();
        for arg in self.args {
            let (name, arg) = match arg {
                BuiltInFunctionArg::Positional(name) => (name, FunctionArg::Positional(span)),
                BuiltInFunctionArg::PositionalVariadic(name) => {
                    (name, FunctionArg::PositionalVariadic(span))
                }
                BuiltInFunctionArg::Keyword(name, default) => {
                    (name, FunctionArg::Keyword(span, default))
                }
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
