use crate::interpreter::scope::ScopeReport::*;
use crate::interpreter::value::Value;
use crate::interpreter::Ref;
use crate::ref_it;
use crate::report::{ReportKind, ReportLevel, Result};
use crate::span::Span;
use name_variant::NamedVariant;
use rustc_hash::FxBuildHasher;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(NamedVariant)]
enum ScopeReport {
    VariableNotFound(String),
    VariableAlreadyExists(String),
}

impl Display for ScopeReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            VariableNotFound(string) => write!(f, ": {string}")?,
            VariableAlreadyExists(string) => write!(f, ": {string}")?,
        }
        Ok(())
    }
}

impl ReportKind for ScopeReport {
    fn title(&self) -> String {
        format!("{}", self)
    }

    fn level(&self) -> ReportLevel {
        match self {
            VariableAlreadyExists(_) | VariableNotFound(_) => ReportLevel::Error,
        }
    }
}

pub enum VariableStorage {
    Vector(Vec<(String, Value)>),
    HashMap(HashMap<String, Value, FxBuildHasher>),
}

impl VariableStorage {
    fn contains_key(&self, key: &str) -> bool {
        match self {
            VariableStorage::Vector(vars) => vars.iter().any(|(k, _)| k == key),
            VariableStorage::HashMap(vars) => vars.contains_key(key),
        }
    }
    fn try_get(&self, key: &str) -> Option<Value> {
        match self {
            VariableStorage::Vector(vars) => vars
                .iter()
                .find(|(inner_key, _)| inner_key == key)
                .map(|(_, v)| v.clone()),
            VariableStorage::HashMap(vars) => vars.get(key).cloned(),
        }
    }

    // fn len(&self) -> usize {
    //     match self {
    //         VariableStorage::Vector(vars) => vars.len(),
    //         VariableStorage::HashMap(vars) => vars.len(),
    //     }
    // }

    // fn vec_to_hm(&mut self) {
    //     let VariableStorage::Vector(vars) = self else {
    //         return;
    //     };
    //     let hm = HashMap::from_iter(vars.drain(..vars.len()));
    //     *self = VariableStorage::HashMap(hm);
    // }

    fn insert(&mut self, key: String, value: Value) {
        match self {
            VariableStorage::Vector(vars) => vars.push((key.clone(), value)),
            VariableStorage::HashMap(vars) => {
                vars.insert(key, value);
            }
        }
    }

    fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        match self {
            VariableStorage::Vector(vars) => vars
                .iter_mut()
                .find(|(inner_key, _)| key == inner_key)
                .map(|(_, val)| val),
            VariableStorage::HashMap(vars) => vars.get_mut(key),
        }
    }
}

pub struct Scope {
    parent: Option<Ref<Scope>>,
    variables: VariableStorage,
    pub in_function: bool,
}

impl Scope {
    pub fn new(parent: Option<Ref<Self>>) -> Ref<Self> {
        let in_function = parent
            .clone()
            .map_or(false, |scope| scope.borrow().in_function);
        ref_it!(Self {
            parent,
            variables: VariableStorage::Vector(Vec::new()),
            in_function,
        })
    }

    pub fn new_hm(parent: Option<Ref<Self>>) -> Ref<Self> {
        let in_function = parent
            .clone()
            .map_or(false, |scope| scope.borrow().in_function);
        ref_it!(Self {
            parent,
            variables: VariableStorage::HashMap(HashMap::default()),
            in_function,
        })
    }

    fn try_get(&self, key: &str) -> Option<Value> {
        self.variables
            .try_get(key)
            .or_else(|| self.parent.as_ref().and_then(|p| p.borrow().try_get(key)))
    }

    pub fn get(&self, key: &str, span: Span) -> Result<Value> {
        match self.try_get(key) {
            Some(value) => Ok(value),
            None => Err(VariableNotFound(key.to_string()).make(span).into()),
        }
    }

    pub fn set(&mut self, key: &str, value: Value, span: Span) {
        if self.variables.contains_key(key) {
            self.assign(key, |_| Ok(value), span)
                .expect("Failed to assign key that should exist");
        } else {
            self.variables.insert(key.to_string(), value);
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
                Some(ref parent) => parent.borrow_mut().assign(key, f, span),
                None => Err(VariableNotFound(key.to_string()).make(span).into()),
            },
        }
    }
}
