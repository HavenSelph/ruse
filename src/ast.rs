use std::fmt::{Display, Formatter};

use name_variant::NamedVariant;

use crate::span::Span;

#[derive(NamedVariant)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
}

#[derive(NamedVariant)]
pub enum NodeKind {
    VariableAccess(String),
    VariableDeclaration(String, Box<Node>),
    Assignment(Box<Node>, Box<Node>),
    StringLiteral(String),
    FloatLiteral(f64),
    IntegerLiteral(usize),
    BooleanLiteral(bool),
    BinaryOperation(BinaryOp, Box<Node>, Box<Node>),
    Block(Vec<Box<Node>>),
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

impl Display for Box<Node> {
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

struct Indent<F> {
    f: F,
    indent: usize,
    stored_space: usize,
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
    node: &'n Box<Node>,
    indent: usize,
}

impl<'n> NodeFormatter<'n> {
    fn child(&self, node: &'n Box<Node>) -> Self {
        Self { node, indent: 2 }
    }
}

impl<'a> Display for NodeFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut f = Indent {
            f,
            indent: self.indent,
            stored_space: self.indent,
        };
        let node = self.node;
        write!(f, "{}", node.kind.variant_name())?;
        match &node.kind {
            NodeKind::BinaryOperation(op, lhs, rhs) => {
                write!(
                    f,
                    "({}) {{\n{},\n{}\n}}",
                    op.variant_name(),
                    self.child(lhs),
                    self.child(rhs)
                )?;
            }
            NodeKind::VariableAccess(name) => write!(f, "({name:?})")?,
            NodeKind::Assignment(lhs, expr) => {
                write!(f, " {{{},\n{}\n}}", self.child(lhs), self.child(expr))?
            }
            NodeKind::VariableDeclaration(name, expr) => {
                write!(f, " {{{name:?},\n{}\n}}", self.child(expr))?
            }
            NodeKind::StringLiteral(val) => write!(f, "({val:?})")?,
            NodeKind::FloatLiteral(val) => write!(f, "({val})")?,
            NodeKind::IntegerLiteral(val) => write!(f, "({val})")?,
            NodeKind::BooleanLiteral(val) => write!(f, "({val})")?,
            NodeKind::Block(stmts) => {
                writeln!(f, " {{")?;
                for stmt in stmts {
                    writeln!(f, "{}", self.child(stmt))?;
                }
                write!(f, "}}")?;
            }
        }
        write!(f, "[{:?}]", self.node.span)?;
        Ok(())
    }
}
