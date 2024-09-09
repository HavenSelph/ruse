#![allow(dead_code)]
use crate::interpreter::value::Value;
use crate::interpreter::Ref;
use crate::ref_it;
use crate::report::{ReportKind, ReportLevel, Result};
use crate::span::Span;
use name_variant::NamedVariant;
use rustc_hash::FxBuildHasher;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, RwLock};
use TrashReport::*;

#[derive(NamedVariant)]
enum TrashReport {
    VariableNotFound(String),
    VariableAlreadyExists(String),
}

impl Display for TrashReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            VariableNotFound(string) => write!(f, ": {string}")?,
            VariableAlreadyExists(string) => write!(f, ": {string}")?,
        }
        Ok(())
    }
}

impl From<TrashReport> for ReportLevel {
    fn from(value: TrashReport) -> Self {
        match value {
            VariableAlreadyExists(_) | VariableNotFound(_) => Self::Error,
        }
    }
}

impl ReportKind for TrashReport {}

pub static GC: RwLock<GarbageCollector> = RwLock::new(GarbageCollector::new());

pub struct GarbageCollector {
    next: usize,
    refs: Vec<Ref<Value>>,
}

impl GarbageCollector {
    const fn new() -> Self {
        Self {
            next: 100,
            refs: Vec::new(),
        }
    }

    pub fn monitor(&mut self, value: Value) -> Ref<Value> {
        let value = ref_it!(value);
        self.refs.push(value.clone());
        value
    }

    pub fn collect(&mut self) {
        if self.refs.len() < 2 * self.next {
            return;
        }
        self.refs = self
            .refs
            .iter()
            .fold(Vec::with_capacity(self.refs.len()), |mut vec, val| {
                if Arc::strong_count(val) > 1 {
                    vec.push(val.clone());
                }
                vec
            });
        self.next = self.refs.len();
    }
}

pub struct Scope {
    parent: Option<Ref<Scope>>,
    variables: HashMap<String, Ref<Value>, FxBuildHasher>,
    pub in_function: bool,
}

impl Scope {
    pub fn new(parent: Option<Ref<Self>>) -> Ref<Self> {
        let in_function = parent
            .clone()
            .map_or(false, |scope| scope.read().unwrap().in_function);
        ref_it!(Self {
            parent,
            variables: HashMap::with_hasher(FxBuildHasher),
            in_function
        })
    }

    fn _get(&self, key: &str) -> Option<Ref<Value>> {
        match self.variables.get(key) {
            Some(value) => Some(value.clone()),
            None => match self.parent {
                Some(ref parent) => parent.read().unwrap()._get(key),
                None => None,
            },
        }
    }

    pub fn get(&self, key: &str, span: Span) -> Result<Ref<Value>> {
        match self._get(key) {
            Some(value) => Ok(value),
            None => Err(VariableNotFound(key.to_string()).make(span).into()),
        }
    }

    pub fn declare(&mut self, key: &str, value: Ref<Value>, span: Span) -> Result<()> {
        if self.variables.contains_key(key) {
            Err(VariableAlreadyExists(key.to_string()).make(span).into())
        } else {
            self.variables.insert(key.to_string(), value);
            Ok(())
        }
    }

    pub fn assign<F: FnOnce(Ref<Value>) -> Result<Ref<Value>>>(
        &mut self,
        key: &str,
        f: F,
        span: Span,
    ) -> Result<Ref<Value>> {
        match self.variables.get_mut(key) {
            Some(value) => {
                *value = f(value.clone())?;
                Ok(value.clone())
            }
            None => match self.parent {
                Some(ref parent) => parent.write().unwrap().assign(key, f, span),
                None => Err(VariableNotFound(key.to_string()).make(span).into()),
            },
        }
    }
}
