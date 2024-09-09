use crate::ast::{BinaryOp, CallArg, FunctionArg, Node, NodeKind, UnaryOp};
pub use crate::interpreter::trash::Scope;
use crate::interpreter::trash::GC;
use crate::interpreter::value::{Function, FunctionRun, Value};
use crate::report::{ReportKind, ReportLevel, Result};
use crate::span::Span;
use indexmap::IndexMap;
use name_variant::NamedVariant;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, RwLock};
use InterpreterReport::*;

mod builtin;
mod trash;
mod value;

#[derive(NamedVariant)]
enum InterpreterReport {
    SyntaxError(String),
}

impl Display for InterpreterReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            SyntaxError(msg) => write!(f, ": {}", msg)?,
        }
        Ok(())
    }
}

impl From<InterpreterReport> for ReportLevel {
    fn from(value: InterpreterReport) -> Self {
        match value {
            SyntaxError(_) => Self::Error,
        }
    }
}

impl ReportKind for InterpreterReport {}

type Ref<T> = Arc<RwLock<T>>;

#[macro_export]
macro_rules! ref_it {
    ($val:expr) => {
        std::sync::Arc::new(std::sync::RwLock::new($val))
    };
}

pub struct Interpreter {
    control_flow: ControlFlow,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            control_flow: ControlFlow::None,
        }
    }

    pub fn register_builtins_to_scope(scope: Ref<Scope>) -> Ref<Scope> {
        macro_rules! register_builtins {
            ($($func_path:path),+$(,)?) => {
                $(
                let func = $func_path();
                let name = func.read().unwrap().name.clone().unwrap();
                scope
                    .write()
                    .unwrap()
                    .declare(&name, GC.write().unwrap().monitor(Value::Function(func)), Span::empty())
                    .ok();
                )+
            };
        }
        register_builtins!(builtin::print, builtin::debug);
        scope
    }

    #[allow(unused)]
    pub fn run_and_return_scope(&mut self, node: &Node) -> Result<Ref<Scope>> {
        let scope = Scope::new(None);
        self.run_block(node, scope.clone()).map(|_| scope)
    }

    #[allow(unused)]
    pub fn execute(&mut self, node: &Node) -> Result<Ref<Scope>> {
        let scope = Scope::new(None);
        self.run(node, scope.clone()).map(|_| scope)
    }

    pub fn run_block(&mut self, node: &Node, scope: Ref<Scope>) -> Result<Value> {
        match &node.kind {
            NodeKind::Block(stmts) => {
                let mut last = None;
                for stmt in stmts {
                    last = Some(self.run(stmt, scope.clone())?);
                    match self.control_flow {
                        ControlFlow::None => {}
                        _ => break,
                    }
                }
                Ok(last.unwrap_or(Value::None))
            }
            _ => unreachable!("run_block called on non-block"),
        }
    }

    pub fn run(&mut self, node: &Node, scope: Ref<Scope>) -> Result<Value> {
        let span = node.span;

        macro_rules! dispatch_op {
            ($op:path, $left:expr, $right:expr) => {{
                let left = self.run($left, scope.clone())?;
                let right = self.run($right, scope.clone())?;
                $op(&left, &right, span)
            }};

            ($op:path, $val:expr) => {{
                let val = self.run($val, scope.clone())?;
                $op(&val, span)
            }};
        }

        match &node.kind {
            NodeKind::For(init, cond, loop_expr, body) => {
                let scope = Scope::new(Some(scope.clone()));
                if let Some(init) = init {
                    self.run(init, scope.clone())?;
                }
                loop {
                    if let Some(cond) = cond {
                        if !self.run(cond, scope.clone())?.as_bool() {
                            break;
                        }
                    }
                    self.run(body, scope.clone())?;
                    match self.control_flow {
                        ControlFlow::Break => {
                            self.control_flow = ControlFlow::None;
                            break;
                        }
                        ControlFlow::Continue => {
                            self.control_flow = ControlFlow::None;
                        }
                        _ => {}
                    }
                    if let Some(loop_expr) = loop_expr {
                        self.run(loop_expr, scope.clone())?;
                    }
                }
                Ok(Value::None)
            }
            NodeKind::While(condition, body) => {
                loop {
                    if !self.run(condition, scope.clone())?.as_bool() {
                        break;
                    }
                    self.run(body, scope.clone())?;
                    match self.control_flow {
                        ControlFlow::Break => {
                            self.control_flow = ControlFlow::None;
                            break;
                        }
                        ControlFlow::Continue => {
                            self.control_flow = ControlFlow::None;
                        }
                        _ => {}
                    }
                }
                Ok(Value::None)
            }
            NodeKind::Subscript { .. } => todo!("Subscript"),
            NodeKind::ArrayLiteral(stmts) => {
                let mut vals = Vec::with_capacity(stmts.len());
                for stmt in stmts {
                    vals.push(self.run(stmt, scope.clone())?);
                }
                Ok(Value::Array(ref_it!(vals)))
            }
            NodeKind::TupleLiteral(stmts) => {
                let mut vals = Vec::with_capacity(stmts.len());
                for stmt in stmts {
                    vals.push(self.run(stmt, scope.clone())?);
                }
                Ok(Value::Tuple(ref_it!(vals)))
            }
            NodeKind::IfStatement(condition, truthy, falsy) => {
                let condition = self.run(condition, scope.clone())?;
                if condition.as_bool() {
                    self.run(truthy, scope)
                } else if let Some(falsy) = falsy {
                    self.run(falsy, scope)
                } else {
                    Ok(Value::None)
                }
            }
            NodeKind::VariableAccess(key) => {
                let val = scope.read().unwrap().get(key.as_str(), span)?;
                Ok(Value::clone(&val.clone().read().unwrap()))
            }
            NodeKind::VariableDeclaration(key, expr) => {
                let value = self.run(expr, scope.clone())?;
                scope
                    .write()
                    .unwrap()
                    .declare(key.as_str(), GC.write().unwrap().monitor(value), span)
                    .map(|_| Value::None)
            }
            NodeKind::Assignment(key, expr) => {
                let value = self.run(expr, scope.clone())?;
                self.simple_assign(key, GC.write().unwrap().monitor(value), scope, span)
            }
            NodeKind::CompoundAssignment(op, lhs, rhs) => {
                let rhs = self.run(rhs, scope.clone())?;
                self.handle_assign(
                    lhs,
                    |lhs| {
                        let lhs = lhs.read().unwrap();
                        let val = match op {
                            BinaryOp::Add => lhs.add(&rhs, span),
                            BinaryOp::Subtract => lhs.subtract(&rhs, span),
                            BinaryOp::Multiply => lhs.multiply(&rhs, span),
                            BinaryOp::Divide => lhs.divide(&rhs, span),
                            BinaryOp::Modulus => lhs.modulo(&rhs, span),
                            BinaryOp::Power => lhs.power(&rhs, span),
                            _ => unreachable!(),
                        }?;
                        Ok(GC.write().unwrap().monitor(val))
                    },
                    scope.clone(),
                    span,
                )
            }
            NodeKind::NoneLiteral => Ok(Value::None),
            NodeKind::StringLiteral(string) => Ok(Value::String(Arc::from(string.clone()))),
            NodeKind::FloatLiteral(val) => Ok(Value::Float(*val)),
            NodeKind::IntegerLiteral(val) => Ok(Value::Integer(*val as isize)),
            NodeKind::BooleanLiteral(val) => Ok(Value::Boolean(*val)),
            NodeKind::BinaryOperation(op, lhs, rhs) => match op {
                BinaryOp::Add => dispatch_op!(Value::add, lhs, rhs),
                BinaryOp::Subtract => dispatch_op!(Value::subtract, lhs, rhs),
                BinaryOp::Multiply => dispatch_op!(Value::multiply, lhs, rhs),
                BinaryOp::Divide => dispatch_op!(Value::divide, lhs, rhs),
                BinaryOp::Modulus => dispatch_op!(Value::modulo, lhs, rhs),
                BinaryOp::Power => dispatch_op!(Value::power, lhs, rhs),
                BinaryOp::LogicalOr => Ok(dispatch_op!(Value::logical_or, lhs, rhs)),
                BinaryOp::LogicalAnd => Ok(dispatch_op!(Value::logical_and, lhs, rhs)),
                BinaryOp::CompareEq => Ok(dispatch_op!(Value::equals, lhs, rhs)),
                BinaryOp::CompareNotEq => Ok(dispatch_op!(Value::not_equals, lhs, rhs)),
                BinaryOp::CompareGreaterThan => dispatch_op!(Value::greater_than, lhs, rhs),
                BinaryOp::CompareGreaterThanEq => {
                    dispatch_op!(Value::greater_than_equal, lhs, rhs)
                }
                BinaryOp::CompareLessThan => dispatch_op!(Value::less_than, lhs, rhs),
                BinaryOp::CompareLessThanEq => dispatch_op!(Value::less_than_eq, lhs, rhs),
            },
            NodeKind::UnaryOperation(op, expr) => match op {
                UnaryOp::LogicalNot => Ok(dispatch_op!(Value::logical_negate, expr)),
                UnaryOp::Negate => dispatch_op!(Value::negate, expr),
            },
            NodeKind::Block(_) => {
                let scope = Scope::new(Some(scope));
                let val = self.run_block(node, scope);
                GC.write().unwrap().collect();
                val
            }
            NodeKind::Function {
                name,
                body,
                args: raw_args,
                captures,
            } => {
                let mut args = IndexMap::new();
                for argument in raw_args.clone() {
                    match argument {
                        FunctionArg::Positional(span, name) => {
                            args.insert(name.clone(), value::FunctionArg::Positional(span));
                        }
                        FunctionArg::PositionalVariadic(span, name) => {
                            args.insert(name.clone(), value::FunctionArg::PositionalVariadic(span));
                        }
                        FunctionArg::Keyword(span, name, default) => {
                            let value = self.run(&default, scope.clone())?;
                            args.insert(
                                name.clone(),
                                value::FunctionArg::Keyword(
                                    span,
                                    GC.write().unwrap().monitor(value),
                                ),
                            );
                        }
                        FunctionArg::KeywordVariadic(_span, name) => {
                            args.insert(name.clone(), value::FunctionArg::KeywordVariadic(()));
                        }
                    }
                }
                let function_scope = Scope::new(None);
                for (capture, span) in captures {
                    let val = scope.clone().read().unwrap().get(capture, *span)?;
                    function_scope
                        .write()
                        .unwrap()
                        .declare(capture, val, *span)?;
                }
                let function = Value::Function(ref_it!(Function {
                    span,
                    name: name.clone(),
                    args,
                    scope: function_scope,
                    run: FunctionRun::Program(body.clone()),
                }));
                if let Some(name) = name {
                    scope.write().unwrap().declare(
                        name,
                        GC.write().unwrap().monitor(function),
                        span,
                    )?;
                    Ok(Value::None)
                } else {
                    Ok(function)
                }
            }
            NodeKind::Return(expr) => {
                if !scope.read().unwrap().in_function {
                    return Err(SyntaxError("Return used outside of function".to_string())
                        .make(span)
                        .into());
                }
                let value = if let Some(expr) = expr {
                    self.run(expr, scope.clone())?
                } else {
                    Value::None
                };
                self.control_flow = ControlFlow::Return;
                Ok(value)
            }
            NodeKind::Call(expr, args) => self.handle_call(expr, args, scope, span),
            NodeKind::Continue => {
                self.control_flow = ControlFlow::Continue;
                Ok(Value::None)
            }
            NodeKind::Break => {
                self.control_flow = ControlFlow::Break;
                Ok(Value::None)
            }
        }
    }

    pub fn handle_call(
        &mut self,
        node: &Node,
        call_args: &[CallArg],
        scope: Ref<Scope>,
        span: Span,
    ) -> Result<Value> {
        let callee = self.run(node, scope.clone())?;
        let mut args = Vec::with_capacity(call_args.len());
        for arg in call_args.iter() {
            let value_arg = match arg {
                CallArg::Positional(span, node) => {
                    value::CallArg::Positional(*span, self.run(node, scope.clone())?)
                }
                CallArg::Keyword(span, string, node) => {
                    value::CallArg::Keyword(*span, string.clone(), self.run(node, scope.clone())?)
                }
            };
            args.push(value_arg);
        }
        callee.call(self, args, span)
    }

    pub fn handle_assign<F: FnOnce(Ref<Value>) -> Result<Ref<Value>>>(
        &self,
        key: &Node,
        f: F,
        scope: Ref<Scope>,
        span: Span,
    ) -> Result<Value> {
        match &key.kind {
            NodeKind::VariableAccess(key) => {
                let val = scope.write().unwrap().assign(key, f, span)?;
                Ok(Value::clone(&val.clone().read().unwrap()))
            }
            _ => Err(SyntaxError("Invalid assignment target".to_string())
                .make(span)
                .into()),
        }
    }

    pub fn simple_assign(
        &self,
        key: &Node,
        value: Ref<Value>,
        scope: Ref<Scope>,
        span: Span,
    ) -> Result<Value> {
        self.handle_assign(key, |_| Ok(value), scope, span)
    }
}

#[allow(dead_code)]
#[derive(Clone)]
enum ControlFlow {
    None,
    Continue,
    Break,
    Return,
}
