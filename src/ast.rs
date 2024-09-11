use crate::span::Span;
use name_variant::NamedVariant;
use std::fmt::{Debug, Display, Formatter};

#[derive(NamedVariant, Clone)]
pub enum BinaryOp {
    NotIn,
    Not,
    In,
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

#[derive(NamedVariant, Clone)]
pub enum UnaryOp {
    LogicalNot,
    Negate,
}

#[derive(Clone)]
pub enum FunctionArg {
    Positional(Span, String),
    PositionalVariadic(Span, String),
    Keyword(Span, String, Box<Node>),
    KeywordVariadic(Span, String),
}

#[derive(Clone)]
pub enum CallArg {
    Positional(Span, Box<Node>),
    Keyword(Span, String, Box<Node>),
}

#[derive(NamedVariant, Clone)]
pub enum NodeKind {
    Break,
    Continue,
    NoneLiteral,
    While(Box<Node>, Box<Node>),
    For(
        Option<Box<Node>>,
        Option<Box<Node>>,
        Option<Box<Node>>,
        Box<Node>,
    ),
    Return(Option<Box<Node>>),
    Function {
        name: Option<String>,
        args: Vec<FunctionArg>,
        body: Box<Node>,
    },
    Subscript(Box<Node>, Box<Node>),
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
    StarExpression(Box<Node>),
    StarStarExpression(Box<Node>),
    Block(Vec<Node>),
    Call(Box<Node>, Vec<CallArg>),
}

impl NodeKind {
    pub fn make(self, span: Span) -> Node {
        Node { kind: self, span }
    }
}

#[derive(Clone)]
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
            '\r' => {
                self.stored_space = 0;
            }
            '\t' => {
                self.indent(2);
            }
            '\0' => {
                self.dedent(2);
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
            NodeKind::For(init_expr, condition_expr, loop_expr, body) => {
                writeln!(f, " {{\t\nInitializer {{")?;
                match init_expr {
                    Some(expr) => writeln!(f, "{}", self.child(expr))?,
                    None => writeln!(f, "None")?,
                }
                write!(f, "}}\nCondition {{")?;
                match condition_expr {
                    Some(expr) => writeln!(f, "{}", self.child(expr))?,
                    None => writeln!(f, "None")?,
                }
                writeln!(f, "}}Loop {{")?;
                match loop_expr {
                    Some(expr) => writeln!(f, "{}", self.child(expr))?,
                    None => writeln!(f, "None")?,
                }
                write!(f, "}}\n{}", self.child(body))?;
                write!(f, "\0\n}}")?;
            }
            NodeKind::While(condition, body) => {
                write!(
                    f,
                    " {{\n{}\n{}\n}}",
                    self.child(condition),
                    self.child(body)
                )?;
            }
            NodeKind::Return(expr) => {
                if let Some(expr) = expr {
                    write!(f, " {{\n{}\n}}", self.child(expr))?;
                }
            }
            NodeKind::NoneLiteral => (),
            NodeKind::Subscript(lhs, idx) => {
                write!(f, " {{\n{}\n{}\n}}", self.child(lhs), self.child(idx))?;
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
            NodeKind::Function { name, args, body } => {
                if let Some(name) = name {
                    write!(f, "({name})")?;
                } else {
                    write!(f, "(<anonymous>)")?;
                }
                writeln!(f, " {{")?;
                writeln!(f, "{{\n\tArguments (\t")?;
                for arg in args {
                    match arg {
                        FunctionArg::Positional(_, name) => writeln!(f, "{name},")?,
                        FunctionArg::PositionalVariadic(_, name) => writeln!(f, "*{name},")?,
                        FunctionArg::KeywordVariadic(_, name) => writeln!(f, "**{name},")?,
                        FunctionArg::Keyword(_, name, default) => {
                            writeln!(f, "{name}={{\n{}\n}},", self.child(default))?;
                        }
                    }
                }
                writeln!(f, "\0)\n\0{}\n}}", self.child(body))?;
            }
            NodeKind::Call(expr, args) => {
                writeln!(f, " {{")?;
                writeln!(f, "{}", self.child(expr))?;
                f.indent(2);
                writeln!(f, "Arguments (")?;
                f.indent(2);
                for arg in args {
                    match arg {
                        CallArg::Positional(_, expr) => writeln!(f, "{},", self.child(expr))?,
                        CallArg::Keyword(_, name, expr) => {
                            f.indent(2);
                            writeln!(f, "{name} = {{\n{}\n}},", self.child(expr))?;
                            f.dedent(2);
                        }
                    }
                }
                f.dedent(2);
                writeln!(f, ")")?;
                f.dedent(2);
                write!(f, "}}")?;
            }
            NodeKind::Break => (),
            NodeKind::Continue => (),
            NodeKind::StarStarExpression(expr) | NodeKind::StarExpression(expr) => {
                write!(f, " {{\n{}\n}}", self.child(expr))?;
            }
        }
        write!(f, "[{:?}]", self.node.span)?;
        Ok(())
    }
}
