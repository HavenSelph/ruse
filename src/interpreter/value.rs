use crate::interpreter::trash::GC;
use crate::interpreter::value::ValueReport::*;
use crate::interpreter::InterpreterReport::SyntaxError;
use crate::interpreter::{ControlFlow, Interpreter, Ref, Scope};
use crate::ref_it;
use crate::report::{ReportKind, ReportLevel, Result, SpanToLabel};
use crate::span::Span;
use ariadne::Color;
use indexmap::IndexMap;
use name_variant::NamedVariant;
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::ops::Neg;
use std::sync::Arc;

#[derive(NamedVariant)]
enum ValueReport {
    ValueError,
    UnexpectedArgument,
}

impl std::fmt::Display for ValueReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())
    }
}

impl From<ValueReport> for ReportLevel {
    fn from(value: ValueReport) -> Self {
        match value {
            ValueError => Self::Error,
            UnexpectedArgument => Self::Error,
        }
    }
}

impl ReportKind for ValueReport {}
pub type BuiltInFunction = Box<dyn Fn(Ref<Scope>, Span) -> Result<Value> + Send + Sync>;
#[allow(dead_code)]
pub enum FunctionRun {
    Program(Box<crate::ast::Node>),
    BuiltIn(BuiltInFunction),
}

pub enum FunctionArg {
    Positional(Span),
    PositionalVariadic(Span),
    Keyword(Span, Ref<Value>),
    KeywordVariadic(()),
}

pub struct Function {
    pub span: Span,
    pub name: Option<String>,
    pub args: IndexMap<String, FunctionArg>,
    pub scope: Ref<Scope>,
    pub run: FunctionRun,
}

#[derive(Clone)]
pub enum CallArg {
    Positional(Span, Value),
    Keyword(Span, String, Value),
}

#[derive(NamedVariant, Clone)]
pub enum Value {
    Function(Ref<Function>),
    Integer(isize),
    Float(f64),
    Boolean(bool),
    String(Arc<str>),
    Array(Ref<Vec<Value>>),
    Tuple(Ref<Vec<Value>>),
    None,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value(")?;
        match self {
            Value::Array(vals) => {
                write!(f, "[")?;
                for (i, val) in vals.read().unwrap().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val:?}")?;
                }
                write!(f, "]")?;
            }
            Value::Tuple(vals) => {
                write!(f, "(")?;
                for (i, val) in vals.read().unwrap().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val:?}")?;
                }
                write!(f, ")")?;
            }
            Value::Integer(val) => write!(f, "{val}")?,
            Value::Float(val) => write!(f, "{val}")?,
            Value::Boolean(val) => write!(f, "{val}")?,
            Value::String(val) => write!(f, "{val:?}")?,
            Value::None => write!(f, "None")?,
            Value::Function(function) => {
                let function = function.read().unwrap();
                let kind = match function.run {
                    FunctionRun::BuiltIn(_) => "BuiltIn-Function",
                    FunctionRun::Program(..) => "Function",
                };
                write!(
                    f,
                    "<{kind}: {} at {}>",
                    function
                        .name
                        .as_ref()
                        .map(|s| format!("{s:?}"))
                        .unwrap_or("<anonymous>".to_string()),
                    function.span
                )?;
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Array(vals) => {
                write!(f, "[")?;
                for (i, val) in vals.read().unwrap().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val}")?;
                }
                write!(f, "]")?;
            }
            Value::Tuple(vals) => {
                write!(f, "(")?;
                for (i, val) in vals.read().unwrap().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val}")?;
                }
                write!(f, ")")?;
            }
            Value::Integer(val) => write!(f, "{val}")?,
            Value::Float(val) => write!(f, "{val}")?,
            Value::Boolean(val) => write!(f, "{val}")?,
            Value::String(val) => write!(f, "{val}")?,
            Value::None => write!(f, "None")?,
            Value::Function { .. } => write!(f, "{self:?}")?,
        }
        Ok(())
    }
}

impl Value {
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        call_args: Vec<CallArg>,
        span: Span,
    ) -> Result<Value> {
        match self {
            Value::Function(function) => {
                let function = function.read().unwrap();
                let scope = Scope::new(Some(function.scope.clone()));
                let mut call_args = call_args.iter().peekable();
                let mut arg_map: IndexMap<&String, &FunctionArg> =
                    IndexMap::from_iter(function.args.iter().rev());
                '_lock_scope: {
                    let mut scope = scope.write().unwrap();
                    scope.in_function = true;
                    while let Some(CallArg::Positional(span, val)) = call_args.peek() {
                        call_args.next();
                        match arg_map.pop() {
                            Some((name, FunctionArg::Positional(_))) => {
                                scope.declare(
                                    name,
                                    GC.write().unwrap().monitor(val.clone()),
                                    *span,
                                )?;
                            }
                            Some((name, FunctionArg::PositionalVariadic(_))) => {
                                let mut values = vec![val.clone()];
                                while let Some(CallArg::Positional(_, val)) = call_args.peek() {
                                    call_args.next();
                                    values.push(val.clone());
                                }
                                scope.declare(
                                    name,
                                    GC.write().unwrap().monitor(Value::Tuple(ref_it!(values))),
                                    *span,
                                )?;
                            }
                            Some((name, FunctionArg::Keyword(..))) => {
                                scope.declare(
                                    name,
                                    GC.write().unwrap().monitor(val.clone()),
                                    *span,
                                )?;
                            }
                            _ => return Err(UnexpectedArgument.make(*span).into()),
                        }
                    }

                    while let Some(CallArg::Keyword(span, name, val)) = call_args.peek() {
                        call_args.next();
                        if let Some(CallArg::Positional(span, ..)) = call_args.peek() {
                            return Err(UnexpectedArgument
                                .make(*span)
                                .with_message(
                                    "Positional arguments must not follow keyword arguments",
                                )
                                .into());
                        }
                        match arg_map.shift_remove_entry(name) {
                            Some((_, FunctionArg::Positional(_))) => {
                                scope.declare(
                                    name,
                                    GC.write().unwrap().monitor(val.clone()),
                                    *span,
                                )?;
                            }
                            Some((_, FunctionArg::Keyword(..))) => {
                                scope.declare(
                                    name,
                                    GC.write().unwrap().monitor(val.clone()),
                                    *span,
                                )?;
                            }
                            _ => return Err(UnexpectedArgument.make(*span).into()),
                        }
                    }

                    while let Some((name, kind)) = arg_map.pop() {
                        match kind {
                            FunctionArg::Positional(a_span) => {
                                return Err(SyntaxError(format!(
                                    "Missing required argument {name:?}"
                                ))
                                .make(span)
                                .with_label(a_span.label().with_color(Color::Blue))
                                .into());
                            }
                            FunctionArg::PositionalVariadic(a_span) => {
                                let default = Value::Tuple(ref_it!(Vec::new()));
                                scope.declare(
                                    name,
                                    GC.write().unwrap().monitor(default),
                                    *a_span,
                                )?;
                            }
                            FunctionArg::Keyword(a_span, default) => {
                                scope.declare(name, default.clone(), *a_span)?;
                            }
                            _ => (),
                        }
                    }
                }
                match &function.run {
                    FunctionRun::Program(node) => {
                        let val = interpreter.run(node, scope)?;
                        if let ControlFlow::Return = interpreter.control_flow {
                            interpreter.control_flow = ControlFlow::None
                        }
                        Ok(val)
                    }
                    FunctionRun::BuiltIn(func) => func(scope, span),
                }
            }
            _ => Err(ValueError
                .make(span)
                .with_message(format!("Cannot call type {}", self.variant_name()))
                .into()),
        }
    }

    pub fn string_repr(&self) -> Self {
        Value::String(Arc::from(self.as_string_repr()))
    }

    pub fn string(&self) -> Self {
        Value::String(Arc::from(self.as_string()))
    }

    pub fn add(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (Value::Array(lhs), Value::Array(rhs)) => Value::Array(ref_it!(lhs
                .read()
                .unwrap()
                .iter()
                .chain(rhs.read().unwrap().iter())
                .fold(
                    Vec::with_capacity(lhs.read().unwrap().len() + rhs.read().unwrap().len()),
                    |mut vec, val| {
                        vec.push(val.clone());
                        vec
                    },
                ))),
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs + rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 + rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs + *rhs as f64),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
            (Value::String(lhs), Value::String(rhs)) => {
                Value::String(Arc::from([lhs.clone(), rhs.clone()].concat()))
            }
            (lhs, rhs) => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!(
                        "Cannot add type {} with {}",
                        lhs.variant_name(),
                        rhs.variant_name()
                    ))
                    .into())
            }
        })
    }

    pub fn subtract(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs - rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 - rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs - *rhs as f64),
            (lhs, rhs) => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!(
                        "Cannot subtract type {} with {}",
                        lhs.variant_name(),
                        rhs.variant_name()
                    ))
                    .into())
            }
        })
    }

    pub fn multiply(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs * rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 * rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs * (*rhs as f64)),
            (lhs, rhs) => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!(
                        "Cannot multiply type {} with {}",
                        lhs.variant_name(),
                        rhs.variant_name()
                    ))
                    .into())
            }
        })
    }

    pub fn divide(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Float(*lhs as f64 / *rhs as f64),
            (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 / *rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(*lhs / *rhs as f64),
            (lhs, rhs) => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!(
                        "Cannot divide type {} with {}",
                        lhs.variant_name(),
                        rhs.variant_name()
                    ))
                    .into())
            }
        })
    }

    pub fn modulo(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs % rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => Value::Float(*lhs as f64 % rhs),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs % rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs % (*rhs as f64)),
            (lhs, rhs) => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!(
                        "Cannot multiply type {} with {}",
                        lhs.variant_name(),
                        rhs.variant_name()
                    ))
                    .into())
            }
        })
    }

    pub fn power(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => {
                Value::Float((*lhs as f64).powi(*rhs as i32))
            }
            (Value::Integer(lhs), Value::Float(rhs)) => Value::Float((*lhs as f64).powf(*rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs.powf(*rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs.powi(*rhs as i32)),
            (lhs, rhs) => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!(
                        "Cannot power type {} with {}",
                        lhs.variant_name(),
                        rhs.variant_name()
                    ))
                    .into())
            }
        })
    }

    pub fn logical_or(&self, other: &Self, _span: Span) -> Self {
        Value::Boolean(self.or(other))
    }

    pub fn logical_and(&self, other: &Self, _span: Span) -> Self {
        Value::Boolean(self.and(other))
    }

    pub fn equals(&self, other: &Self, _span: Span) -> Self {
        Value::Boolean(self.eq(other))
    }

    pub fn not_equals(&self, other: &Self, _span: Span) -> Self {
        Value::Boolean(!self.eq(other))
    }

    pub fn less_than(&self, other: &Self, span: Span) -> Result<Self> {
        let ordering = self.cmp(other).ok_or_else(|| {
            Box::new(ValueError.make(span).with_message(format!(
                "Cannot compare equality with type {} with {}",
                self.variant_name(),
                other.variant_name()
            )))
        })?;
        Ok(Value::Boolean(ordering == Ordering::Less))
    }

    pub fn less_than_eq(&self, other: &Self, span: Span) -> Result<Self> {
        let ordering = self.cmp(other).ok_or_else(|| {
            Box::new(ValueError.make(span).with_message(format!(
                "Cannot compare equality with type {} with {}",
                self.variant_name(),
                other.variant_name()
            )))
        })?;
        Ok(Value::Boolean(matches!(
            ordering,
            Ordering::Less | Ordering::Equal
        )))
    }

    pub fn greater_than(&self, other: &Self, span: Span) -> Result<Self> {
        other.less_than(self, span)
    }

    pub fn greater_than_equal(&self, other: &Self, span: Span) -> Result<Self> {
        other.less_than_eq(self, span)
    }

    pub fn negate(&self, span: Span) -> Result<Self> {
        Ok(match self {
            Value::Integer(val) => Value::Integer(val.neg()),
            Value::Float(val) => Value::Float(val.neg()),
            _ => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!("Cannot negate type {}", self.variant_name()))
                    .into())
            }
        })
    }

    pub fn logical_negate(&self, _span: Span) -> Self {
        Value::Boolean(!self.as_bool())
    }

    fn or(&self, other: &Self) -> bool {
        self.as_bool() || other.as_bool()
    }

    fn and(&self, other: &Self) -> bool {
        self.as_bool() && other.as_bool()
    }

    fn eq(&self, other: &Self) -> bool {
        match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs.eq(rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).eq(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.eq(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => lhs.eq(&(*rhs as f64)),
            (Value::String(lhs), Value::String(rhs)) => lhs.eq(rhs),
            (Value::Array(lhs), Value::Array(rhs)) | (Value::Tuple(lhs), Value::Tuple(rhs)) => {
                let (left, right) = (lhs.read().unwrap(), rhs.read().unwrap());
                if left.len() != right.len() {
                    false
                } else {
                    left.iter().zip(right.iter()).all(|(a, b)| a.eq(b))
                }
            }
            (_, _) => false,
        }
    }

    fn cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Some(lhs.cmp(rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => (lhs).partial_cmp(&(*rhs as f64)),
            (_, _) => None,
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Tuple(vals) => !vals.read().unwrap().is_empty(),
            Value::Array(vals) => !vals.read().unwrap().is_empty(),
            Value::Boolean(val) => *val,
            Value::Integer(val) => *val != 0,
            Value::Float(val) => *val != 0f64,
            Value::String(val) => !val.is_empty(),
            Value::Function { .. } => true,
            Value::None => false,
        }
    }

    pub fn as_string(&self) -> String {
        format!("{self}")
    }

    pub fn as_string_repr(&self) -> String {
        format!("{self:?}")
    }
}
