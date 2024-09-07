use crate::interpreter::value::{FunctionArg, Value};
use crate::interpreter::{Ref, Scope};
use crate::report::Result;
use crate::span::Span;
use indexmap::IndexMap;
use std::rc::Rc;

type BuiltInData = (Span, IndexMap<String, FunctionArg>);

macro_rules! make_args {
    ($($key:ident=$val:expr),+$(,)?) => {{
        let mut map = IndexMap::new();
        $(
        map.insert(stringify!($key).to_string(), $val);
        )+
        map
    }};
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

pub fn print_data() -> BuiltInData {
    let span = Span::empty();
    (
        span,
        make_args![
            vals = FunctionArg::PositionalVariadic(span),
            sep = FunctionArg::Keyword(span, Value::String(Rc::new(" ".to_string()))),
            end = FunctionArg::Keyword(span, Value::String(Rc::new("\n".to_string())))
        ],
    )
}
pub fn print_run(scope: Ref<Scope>, span: Span) -> Result<Value> {
    let (Value::Tuple(vals), sep, end) = get_args!(scope, span, vals, sep, end) else {
        unreachable!();
    };
    let (sep, end) = (sep.as_string(), end.as_string());

    for val in vals.borrow().iter() {
        print!("{val}{sep}");
    }
    print!("{end}");
    Ok(Value::None)
}

pub fn debug_data() -> BuiltInData {
    let span = Span::empty();
    (span, make_args![val = FunctionArg::Positional(span),])
}
pub fn debug_run(scope: Ref<Scope>, span: Span) -> Result<Value> {
    let (val,) = get_args!(scope, span, val);
    println!("[DEBUG {span}] {val}");
    Ok(val)
}
