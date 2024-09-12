use crate::interpreter::scope::Scope;
use crate::interpreter::value::{
    BuiltInFunction, Function, FunctionArg, FunctionRun, IteratorValue, NumberIterator, Value,
};
use crate::interpreter::Ref;
use crate::ref_it;
use crate::report::{ReportKind, ReportLevel, Result};
use crate::span::Span;
use indexmap::IndexMap;
use std::rc::Rc;

struct BuiltinError(String);

impl ReportKind for BuiltinError {
    fn title(&self) -> String {
        format!("BuiltinError: {}", self.0)
    }

    fn level(&self) -> ReportLevel {
        ReportLevel::Error
    }
}

macro_rules! get_args {
    ($scope:expr, $span:expr, $($key:ident),+$(,)?) => {
        (
            $(
            $scope.borrow().get(stringify!($key), $span)?,
            )+
        )
    };
}

pub fn print() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (Value::Tuple(vals), sep, end) = get_args!(scope, span, vals, sep, end) else {
            unreachable!()
        };

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
            Value::String(Rc::from(" ")),
        ))
        .with_arg(BuiltInFunctionArg::Keyword(
            "end",
            Value::String(Rc::from("\n")),
        ))
        .finish(Box::new(execute))
}

pub fn range() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (Value::Integer(stop), Value::Integer(start), Value::Integer(increment)) =
            get_args!(scope, span, stop, start, step)
        else {
            return Err(BuiltinError("Incorrect argument types".to_string())
                .make(span)
                .into());
        };
        Ok(Value::Iterator(IteratorValue(ref_it!(NumberIterator {
            current: 0,
            stop,
            increment,
        }))))
    }
    RuseBuiltIn::new("range")
        .with_arg(BuiltInFunctionArg::Positional("stop"))
        .with_arg(BuiltInFunctionArg::Keyword("start", Value::Integer(0)))
        .with_arg(BuiltInFunctionArg::Keyword("step", Value::Integer(1)))
        .finish(Box::new(execute))
}

pub fn debug() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        println!("[DEBUG {span}] {val}");
        Ok(val)
    }
    RuseBuiltIn::new("debug")
        .with_arg(BuiltInFunctionArg::Positional("val"))
        .finish(Box::new(execute))
}

pub fn iter() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        val.as_iter(span)
    }
    RuseBuiltIn::new("iter")
        .with_arg(BuiltInFunctionArg::Positional("val"))
        .finish(Box::new(execute))
}

pub fn str() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        Ok(Value::String(Rc::from(val.to_string())))
    }
    RuseBuiltIn::new("str")
        .with_arg(BuiltInFunctionArg::Positional("val"))
        .finish(Box::new(execute))
}

pub fn array() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        let Value::Iterator(iter) = val.as_iter(span)? else {
            unreachable!()
        };
        Ok(Value::Array(ref_it!(iter.collect())))
    }
    RuseBuiltIn::new("array")
        .with_arg(BuiltInFunctionArg::PositionalVariadic("val"))
        .finish(Box::new(execute))
}
pub fn tuple() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        let Value::Iterator(iter) = val.as_iter(span)? else {
            unreachable!()
        };
        Ok(Value::Tuple(ref_it!(iter.collect())))
    }
    RuseBuiltIn::new("tuple")
        .with_arg(BuiltInFunctionArg::PositionalVariadic("val"))
        .finish(Box::new(execute))
}

pub fn len() -> Value {
    fn execute(scope: Ref<Scope>, span: Span) -> Result<Value> {
        let (val,) = get_args!(scope, span, val);
        Ok(Value::Integer(match val {
            Value::String(str) => str.chars().count() as isize,
            Value::Array(vals) | Value::Tuple(vals) => vals.borrow().len() as isize,
            Value::Iterator(iter) => iter.clone().count() as isize,
            _ => {
                return Err(
                    BuiltinError("Cannot get length of passed argument".to_string())
                        .make(span)
                        .into(),
                )
            }
        }))
    }
    RuseBuiltIn::new("len")
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

    fn finish(self, f: BuiltInFunction) -> Value {
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
        Value::Function(ref_it!(Function {
            span,
            name: Some(self.name),
            args,
            scope: None,
            run,
        }))
    }
}
