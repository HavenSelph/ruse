use crate::span::Span;
use name_variant::NamedVariant;
use std::fmt::{Debug, Display, Formatter};

#[derive(NamedVariant)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Power,
    LogicalOr,
    LogicalAnd,
    CompareEq,
    CompareNotEq,
    CompareGreaterThan,
    CompareGreaterThanEq,
    CompareLessThan,
    CompareLessThanEq,
}

#[derive(NamedVariant)]
pub enum UnaryOp {
    LogicalNot,
    Negate,
}

#[derive(NamedVariant)]
pub enum NodeKind {
    NoneLiteral,
    // Function {
    //     positional: Vec<Node>,
    // },
    Subscript {
        lhs: Box<Node>,
        start: Option<Box<Node>>,
        inclusive: bool,
        stop: Option<Box<Node>>,
    },
    ArrayLiteral(Vec<Node>),
    TupleLiteral(Vec<Node>),
    IfStatement(Box<Node>, Box<Node>, Option<Box<Node>>),
    VariableAccess(String),
    VariableDeclaration(String, Box<Node>),
    Assignment(Box<Node>, Box<Node>),
    CompoundAssignment(BinaryOp, Box<Node>, Box<Node>),
    StringLiteral(String),
    FloatLiteral(f64),
    IntegerLiteral(usize),
    BooleanLiteral(bool),
    BinaryOperation(BinaryOp, Box<Node>, Box<Node>),
    UnaryOperation(UnaryOp, Box<Node>),
    Block(Vec<Node>),
}

impl NodeKind {
    pub fn make(self, span: Span) -> Node {
        Node { kind: self, span }
    }
}

pub struct Node {
    pub kind: NodeKind,
    pub span: Span,
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            NodeFormatter {
                node: self,
                indent: 0,
            }
        )
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

struct Indent<F> {
    f: F,
    indent: usize,
    stored_space: usize,
}

impl<F: std::fmt::Write> Indent<F> {
    pub fn new(f: F, indent: usize) -> Self {
        Self {
            f,
            indent,
            stored_space: indent,
        }
    }

    pub fn indent(&mut self, indent: usize) {
        self.indent += indent;
        self.stored_space = self.indent;
    }
    pub fn dedent(&mut self, indent: usize) {
        self.indent = self.indent.saturating_sub(indent);
        self.stored_space = self.indent;
    }
}

impl<F: std::fmt::Write> std::fmt::Write for Indent<F> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() {
            self.write_char(c)?;
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        match c {
            '\n' => {
                self.f.write_char('\n')?;
                self.stored_space = self.indent;
            }
            ' ' => {
                self.stored_space += 1;
            }
            _ if c.is_whitespace() => {
                unimplemented!("unusual space characters aren't allowed");
            }
            _ => {
                for _ in 0..std::mem::take(&mut self.stored_space) {
                    self.f.write_char(' ')?;
                }
                self.f.write_char(c)?;
            }
        }
        Ok(())
    }
}

impl<F: std::fmt::Write> Indent<F> {
    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> std::fmt::Result {
        std::fmt::Write::write_fmt(self, args)
    }
}

struct NodeFormatter<'n> {
    node: &'n Node,
    indent: usize,
}

impl<'n> NodeFormatter<'n> {
    fn child(&self, node: &'n Node) -> Self {
        Self { node, indent: 2 }
    }
}

impl<'a> Display for NodeFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut f = Indent::new(f, self.indent);
        let node = self.node;
        write!(f, "{}", node.kind.variant_name())?;
        match &node.kind {
            NodeKind::NoneLiteral => (),
            NodeKind::Subscript {
                lhs,
                start,
                inclusive,
                stop,
            } => {
                writeln!(
                    f,
                    "({}) {{\n{},",
                    inclusive.then(|| "Inclusive").unwrap_or("Exclusive"),
                    self.child(lhs),
                )?;
                if let Some(start) = start {
                    writeln!(f, "{},", self.child(start))?;
                }
                if let Some(stop) = stop {
                    writeln!(f, "{},", self.child(stop))?;
                }
                write!(f, "}}")?;
            }
            NodeKind::CompoundAssignment(op, lhs, rhs) => {
                write!(
                    f,
                    "({}) {{\n{}\n{}\n}}",
                    op.variant_name(),
                    self.child(lhs),
                    self.child(rhs)
                )?;
            }
            NodeKind::UnaryOperation(op, expr) => {
                write!(f, "({}) {{\n{}\n}}", op.variant_name(), self.child(expr),)?;
            }
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                write!(
                    f,
                    "({}) {{\n{}\n{}\n}}",
                    op.variant_name(),
                    self.child(lhs),
                    self.child(rhs)
                )?;
            }
            NodeKind::VariableAccess(name) => write!(f, "({name:?})")?,
            NodeKind::Assignment(lhs, expr) => {
                write!(f, " {{\n{}\n{}\n}}", self.child(lhs), self.child(expr))?
            }
            NodeKind::VariableDeclaration(name, expr) => {
                write!(f, "({name:?}) {{\n{}\n}}", self.child(expr))?
            }
            NodeKind::StringLiteral(val) => write!(f, "({val:?})")?,
            NodeKind::FloatLiteral(val) => write!(f, "({val})")?,
            NodeKind::IntegerLiteral(val) => write!(f, "({val})")?,
            NodeKind::BooleanLiteral(val) => write!(f, "({val})")?,
            NodeKind::TupleLiteral(stmts) => {
                writeln!(f, "({} items) (", stmts.len())?;
                for stmt in stmts {
                    writeln!(f, "{}", self.child(stmt))?;
                }
                write!(f, ")")?;
            }
            NodeKind::ArrayLiteral(stmts) => {
                writeln!(f, "({} items) [", stmts.len())?;
                for stmt in stmts {
                    writeln!(f, "{}", self.child(stmt))?;
                }
                write!(f, "]")?;
            }
            NodeKind::Block(stmts) => {
                writeln!(f, "({} statements) {{", stmts.len())?;
                for stmt in stmts {
                    writeln!(f, "{}", self.child(stmt))?;
                }
                write!(f, "}}")?;
            }
            NodeKind::IfStatement(condition, truthy, falsy) => {
                writeln!(f, " {{\n{}\n{}", self.child(condition), self.child(truthy),)?;
                if let Some(falsy) = falsy {
                    f.indent(2);
                    writeln!(f, "Some(\n{}\n)", self.child(falsy))?;
                    f.dedent(2);
                } else {
                    f.indent(2);
                    writeln!(f, "None")?;
                    f.dedent(2);
                }
                write!(f, "}}")?;
            }
        }
        write!(f, "[{:?}]", self.node.span)?;
        Ok(())
    }
}
