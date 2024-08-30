use std::fmt::{Display, Formatter};

use name_variant::NamedVariant;

use ParserReport::*;

use crate::ast::{Node, NodeKind};
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
    ) -> Result<Token> {
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

    pub fn consume_one(&mut self, expect: TokenKind) -> Result<Token> {
        self.consume(|kind| kind == expect, format!("Expected {expect}"))
    }

    pub fn consume_line(&mut self) -> Result<Token> {
        self.consume_one(TokenKind::SemiColon)
    }

    pub fn skip_line(&mut self) {
        self.skip_until(|kind| kind != TokenKind::SemiColon);
    }

    pub fn peek(&mut self) -> Option<&Result<Token>> {
        self.lexer.peek()
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
                    }
                },
                Err(e) => {
                    self.report(e.finish().into());
                    sync(self);
                }
            }
            self.skip_line();
        }
        let end = self.current.span;

        NodeKind::Block(stmts).make(start.extend(end)).into()
    }

    fn parse_block(&mut self, start: Span, closer: TokenKind) -> Result<Box<Node>> {
        let mut stmts = Vec::new();
        let sync = |s: &mut Parser| s.sync(|kind| kind == closer || kind == TokenKind::SemiColon);

        while self.current.kind != closer && self.current.kind != TokenKind::EOF {
            // errors with errors not handling properly
            match self.parse_statement() {
                Ok(stmt) => match self.consume_line() {
                    Ok(_) => stmts.push(stmt),
                    Err(e) => {
                        self.report(e.finish().into());
                        sync(self);
                    }
                },
                Err(e) => {
                    self.report(e.finish().into());
                    sync(self);
                }
            }
            self.skip_line();
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

    fn parse_assignment(&mut self) -> Result<Box<Node>> {
        let left = self.parse_atom()?;
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

    fn parse_atom(&mut self) -> Result<Box<Node>> {
        match self.current {
            Token {
                kind: TokenKind::StringLiteral,
                text,
                span,
            } => {
                self.advance();
                // todo: make this actually parse the string properly
                Ok(NodeKind::StringLiteral(text.to_string()).make(span).into())
            }
            Token {
                kind: TokenKind::BooleanLiteral,
                text,
                span,
            } => {
                self.advance();
                Ok(NodeKind::BooleanLiteral(text.eq("true")).make(span).into())
            }
            Token {
                kind: TokenKind::FloatLiteral,
                text,
                span,
            } => {
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
            Token {
                kind:
                    TokenKind::IntegerLiteralBin
                    | TokenKind::IntegerLiteralDec
                    | TokenKind::IntegerLiteralHex
                    | TokenKind::IntegerLiteralOct,
                text,
                span,
            } => {
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
            token => Err(UnexpectedToken(token.kind).make(token.span).into()),
        }
    }
}
