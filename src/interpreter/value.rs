use crate::interpreter::value::ValueReport::*;
use crate::interpreter::Ref;
use crate::ref_it;
use crate::report::{ReportKind, ReportLevel, Result};
use crate::span::Span;
use name_variant::NamedVariant;
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::ops::Neg;

#[derive(NamedVariant)]
enum ValueReport {
    ValueError,
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
        }
    }
}

impl ReportKind for ValueReport {}

#[derive(NamedVariant, Clone)]
pub enum Value {
    Integer(isize),
    Float(f64),
    Boolean(bool),
    String(String),
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
        }
        Ok(())
    }
}

impl Value {
    pub fn repr(&self) -> Self {
        Value::String(self.to_string())
    }

    pub fn add(&self, other: &Self, span: Span) -> Result<Self> {
        Ok(match (self, other) {
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
                Value::String([lhs.as_str(), rhs.as_str()].concat())
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
        Ok(match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Integer(lhs - rhs),
            (Self::Integer(lhs), Self::Float(rhs)) => Self::Float(*lhs as f64 - rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs - rhs),
            (Self::Float(lhs), Self::Integer(rhs)) => Self::Float(lhs - *rhs as f64),
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
        Ok(match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Integer(lhs * rhs),
            (Self::Integer(lhs), Self::Float(rhs)) => Self::Float(*lhs as f64 * rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs * rhs),
            (Self::Float(lhs), Self::Integer(rhs)) => Self::Float(lhs * (*rhs as f64)),
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
        Ok(match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Float(*lhs as f64 / *rhs as f64),
            (Self::Integer(lhs), Self::Float(rhs)) => Self::Float(*lhs as f64 / *rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs / rhs),
            (Self::Float(lhs), Self::Integer(rhs)) => Self::Float(*lhs / *rhs as f64),
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
        Ok(match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Integer(lhs % rhs),
            (Self::Integer(lhs), Self::Float(rhs)) => Self::Float(*lhs as f64 % rhs),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs % rhs),
            (Self::Float(lhs), Self::Integer(rhs)) => Self::Float(lhs % (*rhs as f64)),
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
        Ok(match (self, other) {
            (Self::Integer(lhs), Self::Integer(rhs)) => Self::Integer(lhs.pow(*rhs as u32)),
            (Self::Float(lhs), Self::Float(rhs)) => Self::Float(lhs.powf(*rhs)),
            (Self::Float(lhs), Self::Integer(rhs)) => Self::Float(lhs.powi(*rhs as i32)),
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

    pub fn logical_or(&self, other: &Self, _span: Span) -> Result<Self> {
        Ok(Value::Boolean(self.or(other)))
    }

    pub fn logical_and(&self, other: &Self, _span: Span) -> Result<Self> {
        Ok(Value::Boolean(self.and(other)))
    }

    pub fn equals(&self, other: &Self, span: Span) -> Result<Self> {
        self.eq(other).map(Value::Boolean).ok_or_else(|| {
            ValueError
                .make(span)
                .with_message(format!(
                    "Cannot compare equality with type {} with {}",
                    self.variant_name(),
                    other.variant_name()
                ))
                .into()
        })
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

    fn eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Some(lhs.eq(rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => Some((*lhs as f64).eq(rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Some(lhs.eq(rhs)),
            (Value::Float(lhs), Value::Integer(rhs)) => Some(lhs.eq(&(*rhs as f64))),
            (_, _) => None,
        }
    }

    fn cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Some(lhs.cmp(rhs)),
            (Value::Integer(lhs), Value::Float(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => (lhs).partial_cmp(rhs),
            (Value::Float(lhs), Value::Integer(rhs)) => (lhs).partial_cmp(&(*rhs as f64)),
            (_, _) => None,
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Tuple(vals) => !vals.borrow().is_empty(),
            Value::Array(vals) => !vals.borrow().is_empty(),
            Value::Boolean(val) => *val,
            Value::Integer(val) => *val != 0,
            Value::Float(val) => *val != 0f64,
            Value::String(val) => !val.is_empty(),
            Value::None => false,
        }
    }
}
