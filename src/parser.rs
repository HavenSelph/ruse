use std::fmt::Display;

use crate::ast::{Node, NodeKind};
use crate::lexer::Base;
use crate::report::{Report, ReportKind, ReportSender, Result};
use crate::span::Span;
use crate::token::{Token, TokenKind};

pub struct Parser<'contents> {
    filename: &'static str,
    tokens: std::iter::Peekable<std::vec::IntoIter<Token<'contents>>>,
    current: Token<'contents>,
    reporter: ReportSender,
}

impl<'contents> Parser<'contents> {
    pub fn new(
        filename: &'static str,
        tokens: Vec<Token<'contents>>,
        reporter: ReportSender,
    ) -> Self {
        let mut tokens = tokens.into_iter().peekable();
        Self {
            filename,
            current: tokens.next().unwrap(),
            tokens,
            reporter,
        }
    }

    fn report(&self, report: Box<Report>) {
        self.reporter.report(report);
    }

    fn advance(&mut self) {
        match self.tokens.next() {
            Some(t) => self.current = t,
            None => {
                panic!("Advanced past end of iterator")
            }
        }
    }

    fn skip_until<F: Fn(TokenKind) -> bool>(&mut self, predicate: F) {
        loop {
            match self.current.kind {
                kind if predicate(kind) => break,
                TokenKind::EOF => break,
                _ => self.advance(),
            }
        }
    }

    fn sync<F: Fn(TokenKind) -> bool>(&mut self, predicate: F) {
        self.skip_until(|kind| kind.likely_statement_start() || predicate(kind))
    }

    fn consume<F: FnOnce(TokenKind) -> bool, T: ToString>(
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
            TokenKind::EOF => Err(ReportKind::UnexpectedEOF
                .make(token.span)
                .with_message(message)
                .into()),
            kind => Err(ReportKind::UnexpectedToken(kind)
                .make(token.span)
                .with_message(message)
                .into()),
        }
    }
    pub fn consume_line(&mut self) -> Result<Token> {
        self.consume_one(TokenKind::SemiColon)
    }

    pub fn consume_one(&mut self, expect: TokenKind) -> Result<Token> {
        self.consume(|kind| kind == expect, format!("Expected {expect}"))
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    pub fn parse(&mut self) -> Box<Node> {
        match self.parse_block(self.current.span, TokenKind::EOF) {
            Ok(program) => program,
            Err(_) => unreachable!("Global block will always parse."),
        }
    }

    fn parse_block(&mut self, start: Span, closer: TokenKind) -> Result<Box<Node>> {
        let mut stmts = Vec::new();

        while self.current.kind != TokenKind::EOF && self.current.kind != closer {
            let stmt = match self.parse_statement() {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.report(e.finish().into());
                    self.sync(|kind| kind == closer || kind == TokenKind::SemiColon);
                    match self.current.kind {
                        TokenKind::SemiColon => {}
                        _ => continue,
                    }
                }
            };
        }

        let end = if closer == TokenKind::EOF {
            self.current.span
        } else {
            self.consume_one(closer)?.span
        };

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
                Ok(NodeKind::VariableDeclaration(ident, expr).make(span).into())
            }
            _ => self.parse_expression(),
        };
        self.consume_line()?;
        stmt
    }

    fn parse_expression(&mut self) -> Result<Box<Node>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Box<Node>> {
        let left = self.parse_scope()?;
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

    fn parse_scope(&mut self) -> Result<Box<Node>> {
        match self.current {
            Token {
                kind: TokenKind::LeftBrace,
                span,
                ..
            } => {
                self.advance();
                self.parse_block(span, TokenKind::RightBrace)
            }
            _ => self.parse_atom(),
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
                    Box::new(ReportKind::InvalidFloatLiteral.make(span).with_message(err))
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
                        ReportKind::InvalidIntegerLiteral(base)
                            .make(span)
                            .with_message(err),
                    )
                })?;
                Ok(NodeKind::IntegerLiteral(val).make(span).into())
            }
            token => Err(ReportKind::UnexpectedToken(token.kind)
                .make(token.span)
                .into()),
        }
    }
}
