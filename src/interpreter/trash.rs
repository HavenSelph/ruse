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
use std::sync::{Arc, RwLock, Weak};
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

pub static SC: RwLock<ScopeCollector> = RwLock::new(ScopeCollector::new());

pub struct ScopeCollector {
    next: usize,
    refs: Vec<Ref<Scope>>,
}

impl ScopeCollector {
    const fn new() -> Self {
        Self {
            next: 100,
            refs: Vec::new(),
        }
    }

    pub fn monitor(&mut self, scope: Scope) -> Ref<Scope> {
        let scope = ref_it!(scope);
        self.refs.push(scope.clone());
        scope
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
    fallback: Option<Weak<RwLock<Scope>>>,
    variables: HashMap<String, Value, FxBuildHasher>,
    pub in_function: bool,
}

impl Scope {
    pub fn new(parent: Option<Ref<Self>>, fallback: Option<Weak<RwLock<Self>>>) -> Ref<Self> {
        let in_function = parent
            .clone()
            .map_or(false, |scope| scope.read().unwrap().in_function);
        SC.write().unwrap().monitor(Self {
            parent,
            fallback,
            variables: HashMap::with_hasher(FxBuildHasher),
            in_function,
        })
    }

    fn try_get(&self, key: &str) -> Option<Value> {
        self.variables.get(key).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.read().unwrap().try_get(key))
                .or_else(|| {
                    self.fallback
                        .as_ref()
                        .and_then(|fallback| fallback.upgrade()?.read().unwrap().try_get(key))
                })
        })
    }

    pub fn get(&self, key: &str, span: Span) -> Result<Value> {
        match self.try_get(key) {
            Some(value) => Ok(value),
            None => Err(VariableNotFound(key.to_string()).make(span).into()),
        }
    }

    pub fn declare(&mut self, key: &str, value: Value, span: Span) -> Result<()> {
        if self.variables.contains_key(key) {
            Err(VariableAlreadyExists(key.to_string()).make(span).into())
        } else {
            self.variables.insert(key.to_string(), value);
            Ok(())
        }
    }

    pub fn assign<F: FnOnce(&Value) -> Result<Value>>(
        &mut self,
        key: &str,
        f: F,
        span: Span,
    ) -> Result<Value> {
        match self.variables.get_mut(key) {
            Some(value) => {
                *value = f(value)?;
                Ok(value.clone())
            }
            None => match self.parent {
                Some(ref parent) => parent.write().unwrap().assign(key, f, span),
                None => Err(VariableNotFound(key.to_string()).make(span).into()),
            },
        }
    }
}
