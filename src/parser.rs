use std::fmt::{Display, Formatter};

use name_variant::NamedVariant;

use ParserReport::*;

use crate::ast::{BinaryOp, Node, NodeKind};
use crate::lexer::{Base, Lexer, LexerIterator};
use crate::report::{Report, ReportKind, ReportLevel, ReportSender, Result};
use crate::span::Span;
use crate::token::{Token, TokenKind};

#[derive(NamedVariant)]
enum ParserReport {
    SyntaxError(String),
    UnexpectedToken(TokenKind),
    UnexpectedEOF,
}

impl Display for ParserReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            SyntaxError(title) => write!(f, ": {title}")?,
            UnexpectedToken(kind) => write!(f, ": {kind}")?,
            _ => (),
        }
        Ok(())
    }
}

impl From<ParserReport> for ReportLevel {
    fn from(value: ParserReport) -> Self {
        match value {
            SyntaxError(_) | UnexpectedToken(_) | UnexpectedEOF => Self::Error,
        }
    }
}

impl ReportKind for ParserReport {}

pub struct Parser<'contents> {
    lexer: std::iter::Peekable<LexerIterator<'contents>>,
    current: Token<'contents>,
    reporter: ReportSender,
}

impl<'contents> Parser<'contents> {
    pub fn new(filename: &'static str, reporter: ReportSender) -> Self {
        let mut lexer = Lexer::new(filename).into_iter().peekable();
        let current = loop {
            match lexer.next() {
                Some(Err(report)) => reporter.report(report.finish().into()),
                Some(Ok(token)) => break token,
                _ => unreachable!(),
            }
        };
        Self {
            current,
            lexer,
            reporter,
        }
    }

    fn report(&self, report: Box<Report>) {
        self.reporter.report(report);
    }

    fn advance(&mut self) {
        self.current = loop {
            match self.lexer.next().expect("Advanced past EOF") {
                Err(report) => self.report(report.finish().into()),
                Ok(token) => break token,
            }
        }
    }

    fn skip_until<F: Fn(TokenKind) -> bool>(&mut self, predicate: F) -> Option<Token> {
        loop {
            match self.current.kind {
                kind if predicate(kind) => break Some(self.current),
                TokenKind::EOF => break None,
                _ => self.advance(),
            }
        }
    }

    fn sync<F: Fn(TokenKind) -> bool>(&mut self, predicate: F) {
        self.skip_until(
            |kind| /* Is it a new statement? */matches!(kind, TokenKind::Let) || predicate(kind),
        );
    }

    fn consume<F: FnOnce(TokenKind) -> bool, T: Display>(
        &mut self,
        predicate: F,
        message: T,
    ) -> Result<Token<'contents>> {
        let token = self.current;
        match token.kind {
            kind if predicate(kind) => {
                if kind != TokenKind::EOF {
                    self.advance();
                }
                Ok(token)
            }
            TokenKind::EOF => Err(UnexpectedEOF.make(token.span).with_message(message).into()),
            kind => Err(UnexpectedToken(kind)
                .make(token.span)
                .with_message(message)
                .into()),
        }
    }

    pub fn consume_one(&mut self, expect: TokenKind) -> Result<Token<'contents>> {
        self.consume(|kind| kind == expect, format!("Expected {expect}"))
    }

    pub fn consume_line(&mut self) -> Result<()> {
        match self.current {
            Token {
                kind: TokenKind::SemiColon,
                ..
            } => self.advance(),
            Token {
                kind: TokenKind::EOF,
                ..
            } => (),
            token if token.newline_before => (),
            token => {
                return Err(UnexpectedToken(token.kind)
                    .make(token.span)
                    .with_message("Expected end of statement")
                    .into())
            }
        }
        Ok(())
    }

    pub fn parse(&mut self) -> Box<Node> {
        self.parse_global()
    }

    fn parse_global(&mut self) -> Box<Node> {
        let mut stmts = Vec::new();
        let sync = |s: &mut Parser| s.sync(|kind| kind == TokenKind::SemiColon);

        let start = self.current.span;
        while self.current.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(stmt) => match self.consume_line() {
                    Ok(_) => stmts.push(stmt),
                    Err(e) => {
                        self.report(e.finish().into());
                        sync(self);
                        self.skip_until(|kind| kind != TokenKind::SemiColon);
                    }
                },
                Err(e) => {
                    self.report(e.finish().into());
                    sync(self);
                    self.skip_until(|kind| kind != TokenKind::SemiColon);
                }
            }
        }
        let end = self.current.span;

        NodeKind::Block(stmts).make(start.extend(end)).into()
    }

    fn parse_block(&mut self, start: Span, closer: TokenKind) -> Result<Box<Node>> {
        let mut stmts = Vec::new();
        let sync = |s: &mut Parser| s.sync(|kind| kind == closer || kind == TokenKind::SemiColon);

        while self.current.kind != closer && self.current.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(stmt) => match self.consume_line() {
                    Ok(_) => stmts.push(stmt),
                    Err(e) => {
                        self.report(e.finish().into());
                        sync(self);
                        self.skip_until(|kind| kind != TokenKind::SemiColon);
                    }
                },
                Err(e) => {
                    self.report(e.finish().into());
                    sync(self);
                    self.skip_until(|kind| kind != TokenKind::SemiColon);
                }
            }
        }
        let end = self.consume_one(closer)?.span;

        Ok(NodeKind::Block(stmts).make(start.extend(end)).into())
    }

    fn parse_statement(&mut self) -> Result<Box<Node>> {
        let stmt = match self.current {
            Token {
                kind: TokenKind::Let,
                span,
                ..
            } => {
                self.advance();
                let ident = self.consume_one(TokenKind::Identifier)?.text.to_string();
                self.consume_one(TokenKind::Equals)?;
                let expr = self.parse_expression()?;
                self.consume_line()?;
                Ok(NodeKind::VariableDeclaration(ident, expr).make(span).into())
            }
            _ => self.parse_expression(),
        };
        stmt
    }

    fn parse_expression(&mut self) -> Result<Box<Node>> {
        self.parse_assignment()
    }

    // fn parse_lambda(&mut self) -> Result<Box<Node>> {}

    fn parse_assignment(&mut self) -> Result<Box<Node>> {
        let left = self.parse_if()?;
        match self.current.kind {
            TokenKind::Equals => {
                self.advance();
                let right = self.parse_expression()?;
                let span = left.span.extend(right.span);
                Ok(NodeKind::Assignment(left, right).make(span).into())
            }
            _ => Ok(left),
        }
    }

    fn parse_if(&mut self) -> Result<Box<Node>> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> Result<Box<Node>> {
        self.parse_logical_and()
    }

    fn parse_logical_and(&mut self) -> Result<Box<Node>> {
        self.parse_logical_not()
    }

    fn parse_logical_not(&mut self) -> Result<Box<Node>> {
        self.parse_comparison()
    }

    fn parse_comparison(&mut self) -> Result<Box<Node>> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_multiplicative()?;
        match self.current.kind {
            TokenKind::Plus | TokenKind::Minus => {
                let op = match self.current.kind {
                    TokenKind::Plus => BinaryOp::Add,
                    TokenKind::Minus => BinaryOp::Subtract,
                    _ => unreachable!(),
                };
                self.advance();
                let rhs = self.parse_additive()?;
                let span = rhs.span;
                Ok(NodeKind::BinaryOperation(op, lhs, rhs).make(span).into())
            }
            _ => Ok(lhs),
        }
    }
    fn parse_multiplicative(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_sign()?;
        match self.current.kind {
            TokenKind::Star | TokenKind::Slash => {
                let op = match self.current.kind {
                    TokenKind::Star => BinaryOp::Multiply,
                    TokenKind::Slash => BinaryOp::Divide,
                    _ => unreachable!(),
                };
                self.advance();
                let rhs = self.parse_multiplicative()?;
                let span = rhs.span;
                Ok(NodeKind::BinaryOperation(op, lhs, rhs).make(span).into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_sign(&mut self) -> Result<Box<Node>> {
        self.parse_exponential()
    }

    fn parse_exponential(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_prefix()?;
        match self.current.kind {
            TokenKind::StarStar => {
                self.advance();
                let rhs = self.parse_exponential()?;
                let span = rhs.span;
                Ok(NodeKind::BinaryOperation(BinaryOp::Power, lhs, rhs)
                    .make(span)
                    .into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_prefix(&mut self) -> Result<Box<Node>> {
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Box<Node>> {
        self.parse_atom()
    }

    fn parse_atom(&mut self) -> Result<Box<Node>> {
        let Token {
            kind, text, span, ..
        } = self.current;
        match kind {
            TokenKind::LeftParen => {
                self.advance();
                let mut exp = self.parse_expression()?;
                let end = self.consume_one(TokenKind::RightParen)?.span;
                exp.span = span.extend(end);
                Ok(exp)
            }
            TokenKind::LeftBrace => {
                self.advance();
                self.parse_block(span, TokenKind::RightBrace)
            }
            TokenKind::Identifier => {
                self.advance();
                Ok(NodeKind::VariableAccess(text.to_string()).make(span).into())
            }
            TokenKind::StringLiteral => {
                self.advance();
                // todo: make this actually parse the string properly
                Ok(NodeKind::StringLiteral(text.to_string()).make(span).into())
            }
            TokenKind::BooleanLiteral => {
                self.advance();
                Ok(NodeKind::BooleanLiteral(text.eq("true")).make(span).into())
            }
            TokenKind::FloatLiteral => {
                self.advance();
                let val = text.parse().map_err(|err| {
                    Box::new(
                        SyntaxError("Invalid Float".to_string())
                            .make(span)
                            .with_message(err),
                    )
                })?;
                Ok(NodeKind::FloatLiteral(val).make(span).into())
            }
            TokenKind::IntegerLiteralBin
            | TokenKind::IntegerLiteralDec
            | TokenKind::IntegerLiteralHex
            | TokenKind::IntegerLiteralOct => {
                let Token { kind, .. } = self.current;
                self.advance();
                let (base, radix) = match kind {
                    TokenKind::IntegerLiteralBin => (Base::Binary, 2),
                    TokenKind::IntegerLiteralOct => (Base::Octal, 8),
                    TokenKind::IntegerLiteralDec => (Base::Decimal, 10),
                    TokenKind::IntegerLiteralHex => (Base::Hexadecimal, 16),
                    _ => unreachable!(),
                };
                let val = usize::from_str_radix(text, radix).map_err(|err| {
                    Box::new(
                        SyntaxError(format!("Invalid {base:?} Integer"))
                            .make(span)
                            .with_message(err),
                    )
                })?;
                Ok(NodeKind::IntegerLiteral(val).make(span).into())
            }
            _ => Err(UnexpectedToken(kind).make(span).into()),
        }
    }
}
