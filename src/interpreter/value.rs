use crate::interpreter::scope::Scope;
use crate::interpreter::value::ValueReport::*;
use crate::interpreter::InterpreterReport::SyntaxError;
use crate::interpreter::{ControlFlow, Interpreter, Ref};
use crate::ref_it;
use crate::report::{ReportKind, ReportLevel, Result, SpanToLabel};
use crate::span::Span;
use ariadne::Color;
use indexmap::IndexMap;
use name_variant::NamedVariant;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::ops::{IndexMut, Neg};
use std::rc::{Rc, Weak};
use std::vec;

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

#[derive(Clone)]
pub struct IteratorValue(pub Ref<dyn Iterator<Item = Value>>);

impl Iterator for IteratorValue {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let Self(iter) = self;
        iter.borrow_mut().next()
    }
}

struct SequenceIterator {
    array: Ref<Vec<Value>>,
    current: usize,
}

impl SequenceIterator {
    pub fn new(array: Ref<Vec<Value>>) -> Self {
        Self { array, current: 0 }
    }
}

impl Iterator for SequenceIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.array.borrow().get(self.current).cloned();
        self.current += 1;
        val
    }
}

struct StringIterator {
    source: Rc<str>,
    index: usize,
}

impl StringIterator {
    pub fn new(source: Rc<str>) -> Self {
        Self { source, index: 0 }
    }
}

impl Iterator for StringIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        let slice = self.source.get(self.index..)?;
        let c = slice.chars().next()?;
        self.index += 1;
        Some(Value::String(Rc::from(c.to_string().as_str())))
    }
}

pub type BuiltInFunction = Box<dyn Fn(Ref<Scope>, Span) -> Result<Value> + Send + Sync>;
#[allow(dead_code)]
pub enum FunctionRun {
    Program(Box<crate::ast::Node>),
    BuiltIn(BuiltInFunction),
}

pub enum FunctionArg {
    Positional(Span),
    PositionalVariadic(Span),
    Keyword(Span, Value),
    KeywordVariadic(()),
}

pub struct Function {
    pub span: Span,
    pub name: Option<String>,
    pub args: IndexMap<String, FunctionArg>,
    pub scope: Option<Weak<RefCell<Scope>>>,
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
    String(Rc<str>),
    Array(Ref<Vec<Value>>),
    Tuple(Ref<Vec<Value>>),
    Iterator(IteratorValue),
    None,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value(")?;
        match self {
            Value::Array(vals) => {
                write!(f, "[")?;
                for (i, val) in vals.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val:?}")?;
                }
                write!(f, "]")?;
            }
            Value::Tuple(vals) => {
                write!(f, "(")?;
                for (i, val) in vals.borrow().iter().enumerate() {
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
                let function = function.borrow();
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
            Value::Iterator(_) => write!(f, "<iterator>")?,
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
                for (i, val) in vals.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{val}")?;
                }
                write!(f, "]")?;
            }
            Value::Tuple(vals) => {
                write!(f, "(")?;
                for (i, val) in vals.borrow().iter().enumerate() {
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
            Value::Iterator(_) => write!(f, "<iterator>")?,
        }
        Ok(())
    }
}

impl Value {
    pub fn subscript_mut<F: FnOnce(&mut Self) -> Result<Self>>(
        &self,
        slice: &Self,
        f: F,
        span: Span,
    ) -> Result<Self> {
        match (self, slice) {
            (Value::Array(vals) | Value::Tuple(vals), Value::Integer(idx)) => {
                if *idx as usize >= vals.borrow().len() {
                    Err(ValueError
                        .make(span)
                        .with_message(format!(
                            "Index {idx} out of bounds on {}",
                            self.variant_name()
                        ))
                        .into())
                } else {
                    f(vals.borrow_mut().index_mut(*idx as usize))
                }
            }
            _ => Err(ValueError
                .make(span)
                .with_message(format!("Invalid slice for type {}", self.variant_name(),))
                .into()),
        }
    }

    pub fn subscript(&self, slice: &Self, span: Span) -> Result<Self> {
        match (self, slice) {
            (Value::String(str), Value::Integer(idx)) => str
                .chars()
                .nth(*idx as usize)
                .map(|c| Value::String(Rc::from(c.to_string().as_str())))
                .ok_or(
                    ValueError
                        .make(span)
                        .with_message(format!(
                            "Index {idx} out of bounds on {}",
                            self.variant_name()
                        ))
                        .into(),
                ),
            _ => self.subscript_mut(slice, |val| Ok(val.clone()), span),
        }
    }

    pub fn unpack(&self, span: Span) -> Result<Ref<Vec<Self>>> {
        match self {
            Value::Array(vals) | Value::Tuple(vals) => Ok(vals.clone()),
            Value::Iterator(vals) => Ok(ref_it!(vals.clone().collect::<Vec<Value>>())),
            _ => Err(ValueError
                .make(span)
                .with_message(format!("{} cannot be unpacked", self.variant_name()))
                .into()),
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        call_args: Vec<CallArg>,
        span: Span,
    ) -> Result<Self> {
        match self {
            Value::Function(function) => {
                let function = function.borrow();
                let parent = function
                    .scope
                    .clone()
                    .map(|g_scope| g_scope.upgrade().unwrap());
                let scope = Scope::new(parent);
                let mut call_args = call_args.iter().peekable();
                let mut arg_map: IndexMap<&String, &FunctionArg> =
                    IndexMap::from_iter(function.args.iter().rev());
                {
                    let mut scope = scope.borrow_mut();
                    scope.in_function = true;
                    while let Some(CallArg::Positional(span, val)) = call_args.peek() {
                        call_args.next();
                        match arg_map.pop() {
                            Some((name, FunctionArg::Positional(_))) => {
                                scope.declare(name, val.clone(), *span)?;
                            }
                            Some((name, FunctionArg::PositionalVariadic(_))) => {
                                let mut values = vec![val.clone()];
                                while let Some(CallArg::Positional(_, val)) = call_args.peek() {
                                    call_args.next();
                                    values.push(val.clone());
                                }
                                scope.declare(name, Value::Tuple(ref_it!(values)), *span)?;
                            }
                            Some((name, FunctionArg::Keyword(..))) => {
                                scope.declare(name, val.clone(), *span)?;
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
                                scope.declare(name, val.clone(), *span)?;
                            }
                            Some((_, FunctionArg::Keyword(..))) => {
                                scope.declare(name, val.clone(), *span)?;
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
                                scope.declare(name, default, *a_span)?;
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

    pub fn add(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (Value::Array(lhs), Value::Array(rhs)) => Value::Array(ref_it!(lhs
                .borrow()
                .iter()
                .chain(rhs.borrow().iter())
                .fold(
                    Vec::with_capacity(lhs.borrow().len() + rhs.borrow().len()),
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
                let mut string = lhs.to_string();
                string.push_str(rhs);
                Value::String(Rc::from(string.as_str()))
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
            (Value::String(lhs), Value::Integer(rhs)) => {
                let mut string = lhs.to_string();
                for _ in 0..*rhs {
                    string.push_str(lhs);
                }
                Value::String(Rc::from(string.as_str()))
            }
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

    pub fn contains(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (&self, &other) {
            (lhs, Value::Array(vals) | Value::Tuple(vals)) => {
                Value::Boolean(vals.borrow().contains(lhs))
            }
            (lhs, Value::Iterator(iter)) => Value::Boolean(iter.clone().any(|v| v.eq(lhs))),
            (lhs, rhs) => {
                return Err(ValueError
                    .make(span)
                    .with_message(format!(
                        "Cannot check if {} in type {}",
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
        let ordering = self.partial_cmp(other).ok_or_else(|| {
            Box::new(ValueError.make(span).with_message(format!(
                "Cannot compare equality with type {} with {}",
                self.variant_name(),
                other.variant_name()
            )))
        })?;
        Ok(Value::Boolean(ordering == Ordering::Less))
    }

    pub fn less_than_eq(&self, other: &Self, span: Span) -> Result<Self> {
        let ordering = self.partial_cmp(other).ok_or_else(|| {
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

    pub fn as_iter(&self, span: Span) -> Result<Self> {
        match self {
            Value::String(str) => Ok(Value::Iterator(IteratorValue(ref_it!(
                StringIterator::new(str.clone())
            )))),
            Value::Array(vals) | Value::Tuple(vals) => Ok(Value::Iterator(IteratorValue(ref_it!(
                SequenceIterator::new(vals.clone())
            )))),
            Value::Iterator(_) => Ok(self.clone()),
            lhs => Err(Box::new(ValueError.make(span).with_message(format!(
                "Cannot turn {} into iterator",
                lhs.variant_name()
            )))),
        }
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

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Tuple(vals) => !vals.borrow().is_empty(),
            Value::Array(vals) => !vals.borrow().is_empty(),
            Value::Boolean(val) => *val,
            Value::Integer(val) => *val != 0,
            Value::Float(val) => *val != 0f64,
            Value::String(val) => !val.is_empty(),
            Value::Function { .. } => true,
            Value::None => false,
            Value::Iterator(_) => true,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Some(lhs.cmp(rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            (_, _) => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self, &other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => lhs.eq(rhs),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).eq(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.eq(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => lhs.eq(&(*rhs as f64)),
            (Value::String(lhs), Value::String(rhs)) => lhs.eq(rhs),
            (Value::Array(lhs), Value::Array(rhs)) | (Value::Tuple(lhs), Value::Tuple(rhs)) => {
                let (left, right) = (lhs.borrow(), rhs.borrow());
                if left.len() != right.len() {
                    false
                } else {
                    left.iter().zip(right.iter()).all(|(a, b)| a.eq(b))
                }
            }
            (_, _) => false,
        }
    }
}
