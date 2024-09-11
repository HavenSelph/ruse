use crate::ast::{BinaryOp, CallArg, FunctionArg, Node, NodeKind, UnaryOp};
use crate::lexer::{Base, Lexer, LexerIterator};
use crate::report::{Report, ReportKind, ReportLevel, ReportSender, Result, SpanToLabel};
use crate::span::Span;
use crate::token::{Token, TokenKind};
use ariadne::Color;
use name_variant::NamedVariant;
use std::fmt::{Display, Formatter};
use ParserReport::*;

#[derive(NamedVariant)]
enum ParserReport {
    SyntaxError(String),
    InvalidFloat,
    InvalidInteger(Base),
    UnexpectedToken(TokenKind),
    UnexpectedEOF,
}

impl Display for ParserReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            SyntaxError(msg) => write!(f, ": {}", msg)?,
            InvalidInteger(base) => write!(f, ": of base {base:?}")?,
            UnexpectedToken(kind) => write!(f, ": {kind}")?,
            _ => (),
        }
        Ok(())
    }
}

impl From<ParserReport> for ReportLevel {
    fn from(value: ParserReport) -> Self {
        match value {
            InvalidFloat | InvalidInteger(_) | SyntaxError(_) | UnexpectedToken(_)
            | UnexpectedEOF => Self::Error,
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
    pub fn new(filename: &'static str, reporter: ReportSender) -> Result<Self> {
        let mut lexer = Lexer::new(filename)?.into_iter().peekable();
        let current = loop {
            match lexer.next() {
                Some(Err(report)) => reporter.report(report.finish().into()),
                Some(Ok(token)) => break token,
                _ => unreachable!(),
            }
        };
        Ok(Self {
            current,
            lexer,
            reporter,
        })
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

    fn skip_until<F: Fn(Token) -> bool>(&mut self, predicate: F) -> Option<Token> {
        loop {
            match self.current {
                token if predicate(token) => break Some(self.current),
                Token {
                    kind: TokenKind::EOF,
                    ..
                } => break None,
                _ => self.advance(),
            }
        }
    }

    fn sync<F: Fn(Token) -> bool>(&mut self, predicate: F) {
        self.skip_until(|token| /* Is it a new statement? */matches!(token.kind,
                TokenKind::SemiColon
                | TokenKind::Let
                | TokenKind::If
                | TokenKind::Else
                | TokenKind::Fn
                | TokenKind::For
                | TokenKind::At
            ) || token.newline_before || predicate(token));
        if self.current.kind == TokenKind::SemiColon {
            self.advance();
        }
    }

    fn peek_is(&mut self, kind: TokenKind) -> bool {
        self.lexer
            .peek()
            .is_some_and(|result| result.as_ref().is_ok_and(|token| token.kind == kind))
    }

    fn consume<F: FnOnce(&Token) -> bool, T: Display>(
        &mut self,
        predicate: F,
        message: T,
    ) -> Result<Token<'contents>> {
        match self.current {
            token if predicate(&token) => {
                if token.kind != TokenKind::EOF {
                    self.advance();
                }
                Ok(token)
            }
            token if token.kind == TokenKind::EOF => {
                Err(UnexpectedEOF.make(token.span).with_message(message).into())
            }
            token => Err(UnexpectedToken(token.kind)
                .make(token.span)
                .with_message(message)
                .into()),
        }
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

    pub fn consume_line_or(&mut self, expect: TokenKind) -> Result<()> {
        match self.current {
            Token {
                kind: TokenKind::SemiColon,
                ..
            } => self.advance(),
            Token {
                kind: TokenKind::EOF,
                ..
            } => (),
            token if token.newline_before || token.kind == expect => (),
            token => {
                return Err(UnexpectedToken(token.kind)
                    .make(token.span)
                    .with_message(format!("Expected end of statement or {:?}", expect))
                    .into())
            }
        }
        Ok(())
    }

    // fn consume_line_or(&mut self, expect: TokenKind) -> Result<Token<'contents>> {
    //     match self.current {
    //         token if token.kind == expect => {
    //             self.advance();
    //             Ok(token)
    //         }
    //         token if token.kind == TokenKind::EOF || token.newline_before => Ok(token),
    //         token => Err(UnexpectedToken(token.kind)
    //             .make(token.span)
    //             .with_message(format!("Expected end of line or {expect:?}"))
    //             .into()),
    //     }
    // }

    fn consume_one(&mut self, expect: TokenKind) -> Result<Token<'contents>> {
        self.consume(|token| token.kind == expect, format!("Expected {expect}"))
    }

    pub fn parse(&mut self) -> Box<Node> {
        self.parse_global()
    }

    fn parse_global(&mut self) -> Box<Node> {
        match self.parse_block(self.current.span, TokenKind::EOF) {
            Ok(val) => val,
            _ => panic!("Failed to parse global block."),
        }
    }

    fn parse_block(&mut self, start: Span, closer: TokenKind) -> Result<Box<Node>> {
        let mut stmts = Vec::new();
        let sync = |s: &mut Parser| s.sync(|token| token.kind == closer);

        while self.current.kind != closer && self.current.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(stmt) => match self.consume_line_or(closer) {
                    Ok(_) => stmts.push(*stmt),
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
        }
        let end = self.consume_one(closer)?.span;

        Ok(NodeKind::Block(stmts).make(start.extend(end)).into())
    }

    fn parse_statement(&mut self) -> Result<Box<Node>> {
        match self.current {
            Token {
                kind: TokenKind::For,
                span,
                ..
            } => {
                self.advance();
                self.consume_one(TokenKind::LeftParen)?;
                let init_expr = match self.current.kind {
                    TokenKind::SemiColon => {
                        self.advance();
                        None
                    }
                    _ => {
                        let expr = self.parse_statement()?;
                        self.consume_one(TokenKind::SemiColon)?;
                        Some(expr)
                    }
                };
                let condition_expr = match self.current.kind {
                    TokenKind::SemiColon => {
                        self.advance();
                        None
                    }
                    _ => {
                        let expr = self.parse_expression()?;
                        self.consume_one(TokenKind::SemiColon)?;
                        Some(expr)
                    }
                };
                let loop_expr = match self.current.kind {
                    TokenKind::RightParen => None,
                    _ => Some(self.parse_expression()?),
                };
                self.consume_one(TokenKind::RightParen)?;
                let body = self.consume_one(TokenKind::LeftBrace)?;
                let body = self.parse_block(body.span, TokenKind::RightBrace)?;
                let span = span.extend(body.span);
                Ok(NodeKind::For(init_expr, condition_expr, loop_expr, body)
                    .make(span)
                    .into())
            }
            Token {
                kind: TokenKind::Break,
                span,
                ..
            } => {
                self.advance();
                self.consume_line()?;
                Ok(NodeKind::Break.make(span).into())
            }
            Token {
                kind: TokenKind::Continue,
                span,
                ..
            } => {
                self.advance();
                self.consume_line()?;
                Ok(NodeKind::Continue.make(span).into())
            }
            Token {
                kind: TokenKind::While,
                span,
                ..
            } => {
                self.advance();
                let condition = self.parse_expression()?;
                let body = self.consume_one(TokenKind::LeftBrace)?;
                let body = self.parse_block(body.span, TokenKind::RightBrace)?;
                self.consume_line()?;
                let span = span.extend(body.span);
                Ok(NodeKind::While(condition, body).make(span).into())
            }
            Token {
                kind: TokenKind::Let,
                span,
                ..
            } => {
                self.advance();
                let ident = self.consume_one(TokenKind::Identifier)?.text.to_string();
                self.consume_one(TokenKind::Equals)?;
                let expr = self.parse_expression()?;
                let span = span.extend(expr.span);
                Ok(NodeKind::VariableDeclaration(ident, expr).make(span).into())
            }
            Token {
                kind: TokenKind::Return,
                span,
                ..
            } => {
                self.advance();
                let (span, expr) = match self.current.kind {
                    TokenKind::SemiColon => (span, None),
                    _ => {
                        let expr = self.parse_expression()?;
                        (span.extend(expr.span), Some(expr))
                    }
                };
                self.consume_line()?;
                Ok(NodeKind::Return(expr).make(span).into())
            }
            Token {
                kind: TokenKind::Fn,
                span,
                ..
            } => {
                self.advance();
                let name = self.consume_one(TokenKind::Identifier)?.text.to_string();
                self.consume_one(TokenKind::LeftParen)?;
                let args = self.parse_signature(TokenKind::RightParen)?;
                self.consume_one(TokenKind::RightParen)?;
                let body = self.consume(
                    |t| matches!(t.kind, TokenKind::LeftBrace | TokenKind::FatArrow),
                    "expected function body",
                )?;
                let body = match body.kind {
                    TokenKind::LeftBrace => self.parse_block(body.span, TokenKind::RightBrace)?,
                    TokenKind::FatArrow => {
                        let expr = self.parse_expression()?;
                        self.consume_line()?;
                        expr
                    }
                    _ => unreachable!(),
                };
                let span = span.extend(body.span);
                Ok(NodeKind::Function {
                    name: Some(name),
                    args,
                    body,
                }
                .make(span)
                .into())
            }
            _ => self.parse_expression(),
        }
    }

    fn parse_expression(&mut self) -> Result<Box<Node>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Box<Node>> {
        let left = self.parse_lambda()?;
        match self.current.kind {
            TokenKind::Equals => {
                self.advance();
                let right = self.parse_assignment()?;
                let span = left.span.extend(right.span);
                Ok(NodeKind::Assignment(left, right).make(span).into())
            }
            TokenKind::PlusEquals
            | TokenKind::MinusEquals
            | TokenKind::StarEquals
            | TokenKind::SlashEquals
            | TokenKind::PercentEquals
            | TokenKind::StarStarEquals => {
                let op = match self.current.kind {
                    TokenKind::PlusEquals => BinaryOp::Add,
                    TokenKind::MinusEquals => BinaryOp::Subtract,
                    TokenKind::StarEquals => BinaryOp::Multiply,
                    TokenKind::SlashEquals => BinaryOp::Divide,
                    TokenKind::PercentEquals => BinaryOp::Modulus,
                    TokenKind::StarStarEquals => BinaryOp::Power,
                    _ => unreachable!(),
                };
                self.advance();
                let expr = self.parse_assignment()?;
                let span = left.span.extend(expr.span);
                Ok(NodeKind::CompoundAssignment(op, left, expr)
                    .make(span)
                    .into())
            }
            _ => Ok(left),
        }
    }

    fn parse_lambda(&mut self) -> Result<Box<Node>> {
        match self.current {
            Token {
                kind: TokenKind::Pipe,
                span,
                ..
            } => {
                self.advance();
                let args = self.parse_signature(TokenKind::Pipe)?;
                self.consume_one(TokenKind::Pipe)?;
                let body = self.parse_lambda()?;
                let span = span.extend(body.span);
                Ok(NodeKind::Function {
                    name: None,
                    args,
                    body,
                }
                .make(span)
                .into())
            }
            Token {
                kind: TokenKind::PipePipe,
                span,
                ..
            } => {
                self.advance();
                let body = self.parse_lambda()?;
                let span = span.extend(body.span);
                Ok(NodeKind::Function {
                    name: None,
                    args: Vec::default(),
                    body,
                }
                .make(span)
                .into())
            }
            _ => self.parse_if_expression(),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Box<Node>> {
        let condition = self.parse_if()?;
        match self.current.kind {
            TokenKind::QuestionMark => {
                self.advance();
                let true_expression = self.parse_if_expression()?;
                self.consume_one(TokenKind::Colon)?;
                let false_expression = self.parse_if_expression()?;
                let span = condition.span.extend(false_expression.span);
                Ok(
                    NodeKind::IfStatement(condition, true_expression, Some(false_expression))
                        .make(span)
                        .into(),
                )
            }
            _ => Ok(condition),
        }
    }

    fn parse_if(&mut self) -> Result<Box<Node>> {
        match self.current {
            Token {
                kind: TokenKind::If,
                span,
                ..
            } => {
                self.advance();
                let condition = self.parse_if()?;
                let true_span = self.consume_one(TokenKind::LeftBrace)?.span;
                let true_block = self.parse_block(true_span, TokenKind::RightBrace)?;
                let (end, else_block) = if self.current.kind == TokenKind::Else {
                    self.advance();
                    let else_block = if self.current.kind == TokenKind::If {
                        self.parse_if()?
                    } else {
                        let else_span = self.consume_one(TokenKind::LeftBrace)?.span;
                        self.parse_block(else_span, TokenKind::RightBrace)?
                    };
                    (else_block.span, Some(else_block))
                } else {
                    (true_block.span, None)
                };
                Ok(NodeKind::IfStatement(condition, true_block, else_block)
                    .make(span.extend(end))
                    .into())
            }
            _ => self.parse_logical_or(),
        }
    }

    fn parse_logical_or(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_logical_and()?;
        match self.current.kind {
            TokenKind::PipePipe => {
                self.advance();
                let rhs = self.parse_logical_or()?;
                let span = lhs.span.extend(rhs.span);
                Ok(NodeKind::BinaryOperation(BinaryOp::LogicalOr, lhs, rhs)
                    .make(span)
                    .into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_logical_and(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_comparison()?;
        match self.current.kind {
            TokenKind::AmpersandAmpersand => {
                self.advance();
                let rhs = self.parse_logical_and()?;
                let span = lhs.span.extend(rhs.span);
                Ok(NodeKind::BinaryOperation(BinaryOp::LogicalAnd, lhs, rhs)
                    .make(span)
                    .into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_comparison(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_additive()?;
        match self.current.kind {
            TokenKind::Not => {
                self.advance();
                let op = match self.current.kind {
                    TokenKind::In => {
                        self.advance();
                        BinaryOp::NotIn
                    }
                    _ => BinaryOp::Not,
                };
                let rhs = self.parse_comparison()?;
                let span = lhs.span.extend(rhs.span);
                Ok(NodeKind::BinaryOperation(op, lhs, rhs).make(span).into())
            }
            TokenKind::BangEquals
            | TokenKind::In
            | TokenKind::EqualsEquals
            | TokenKind::GreaterThan
            | TokenKind::GreaterThanEquals
            | TokenKind::LessThan
            | TokenKind::LessThanEquals => {
                let op = match self.current.kind {
                    TokenKind::EqualsEquals => BinaryOp::CompareEq,
                    TokenKind::BangEquals => BinaryOp::CompareNotEq,
                    TokenKind::GreaterThan => BinaryOp::CompareGreaterThan,
                    TokenKind::GreaterThanEquals => BinaryOp::CompareGreaterThanEq,
                    TokenKind::LessThan => BinaryOp::CompareLessThan,
                    TokenKind::LessThanEquals => BinaryOp::CompareLessThanEq,
                    TokenKind::In => BinaryOp::In,
                    _ => unreachable!(),
                };
                self.advance();
                let rhs = self.parse_comparison()?;
                let span = lhs.span.extend(rhs.span);
                Ok(NodeKind::BinaryOperation(op, lhs, rhs).make(span).into())
            }
            _ => Ok(lhs),
        }
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
                let span = lhs.span.extend(rhs.span);
                Ok(NodeKind::BinaryOperation(op, lhs, rhs).make(span).into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_multiplicative(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_exponential()?;
        match self.current.kind {
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
                let op = match self.current.kind {
                    TokenKind::Star => BinaryOp::Multiply,
                    TokenKind::Slash => BinaryOp::Divide,
                    TokenKind::Percent => BinaryOp::Modulus,
                    _ => unreachable!(),
                };
                self.advance();
                let rhs = self.parse_multiplicative()?;
                let span = lhs.span.extend(rhs.span);
                Ok(NodeKind::BinaryOperation(op, lhs, rhs).make(span).into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_exponential(&mut self) -> Result<Box<Node>> {
        let lhs = self.parse_prefix()?;
        match self.current.kind {
            TokenKind::StarStar => {
                self.advance();
                let rhs = self.parse_exponential()?;
                let span = lhs.span.extend(rhs.span);
                Ok(NodeKind::BinaryOperation(BinaryOp::Power, lhs, rhs)
                    .make(span)
                    .into())
            }
            _ => Ok(lhs),
        }
    }

    fn parse_prefix(&mut self) -> Result<Box<Node>> {
        match self.current {
            Token {
                kind: TokenKind::Star,
                span,
                ..
            } => {
                self.advance();
                let expr = self.parse_prefix()?;
                let end = expr.span;
                Ok(NodeKind::StarExpression(expr).make(span.extend(end)).into())
            }
            Token {
                kind: TokenKind::StarStar,
                span,
                ..
            } => {
                self.advance();
                let expr = self.parse_prefix()?;
                let end = expr.span;
                Ok(NodeKind::StarStarExpression(expr)
                    .make(span.extend(end))
                    .into())
            }
            Token {
                kind: TokenKind::Bang,
                span,
                ..
            } => {
                self.advance();
                let expr = self.parse_prefix()?;
                let end = expr.span;
                Ok(NodeKind::UnaryOperation(UnaryOp::LogicalNot, expr)
                    .make(span.extend(end))
                    .into())
            }
            Token {
                kind: TokenKind::Plus,
                span,
                ..
            } => {
                self.skip_until(|token| token.kind != TokenKind::Plus);
                let mut expr = self.parse_prefix()?;
                expr.span = span.extend(expr.span);
                Ok(expr)
            }
            Token {
                kind: TokenKind::Minus,
                span,
                ..
            } => {
                self.advance();
                let expr = self.parse_prefix()?;
                let end = expr.span;
                Ok(NodeKind::UnaryOperation(UnaryOp::Negate, expr)
                    .make(span.extend(end))
                    .into())
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Box<Node>> {
        let mut lhs = self.parse_atom()?;
        loop {
            match self.current.kind {
                TokenKind::LeftBracket => {
                    self.advance();
                    let idx = self.parse_expression()?;
                    let end = self.consume_one(TokenKind::RightBracket)?.span;
                    let span = lhs.span.extend(end);
                    lhs = NodeKind::Subscript(lhs, idx).make(span).into()
                }
                TokenKind::LeftParen => {
                    self.advance();
                    let mut args = Vec::new();
                    while self.current.kind != TokenKind::RightParen {
                        if !args.is_empty() {
                            self.consume_one(TokenKind::Comma)?;
                        }
                        if self.current.kind == TokenKind::Identifier
                            && self.peek_is(TokenKind::Equals)
                        {
                            let ident = self.consume_one(TokenKind::Identifier)?;
                            self.consume_one(TokenKind::Equals)?;
                            let expr = self.parse_expression()?;
                            args.push(CallArg::Keyword(
                                ident.span.extend(expr.span),
                                ident.text.to_string(),
                                expr,
                            ))
                        } else {
                            let expr = self.parse_expression()?;
                            args.push(CallArg::Positional(expr.span, expr))
                        }
                    }
                    let end = self.consume_one(TokenKind::RightParen)?.span;
                    let span = lhs.span.extend(end);
                    lhs = NodeKind::Call(lhs, args).make(span).into();
                }
                _ => break Ok(lhs),
            }
        }
    }

    fn parse_atom(&mut self) -> Result<Box<Node>> {
        let Token {
            kind, text, span, ..
        } = self.current;
        match kind {
            TokenKind::LeftBracket => {
                self.advance();
                let mut stmts = Vec::new();
                while self.current.kind != TokenKind::RightBracket {
                    if !stmts.is_empty() {
                        self.consume_one(TokenKind::Comma)?;
                    }
                    if self.current.kind == TokenKind::RightBracket {
                        break;
                    }
                    stmts.push(*self.parse_expression()?);
                }
                if self.current.kind == TokenKind::Comma {
                    self.advance();
                }
                let end = self.consume_one(TokenKind::RightBracket)?.span;
                Ok(NodeKind::ArrayLiteral(stmts).make(span.extend(end)).into())
            }
            TokenKind::LeftParen => {
                self.advance();
                let mut stmts = Vec::new();
                let mut tuple = false;
                while self.current.kind != TokenKind::RightParen {
                    if !stmts.is_empty() {
                        tuple = true;
                        self.consume_one(TokenKind::Comma)?;
                    }
                    if self.current.kind == TokenKind::RightParen {
                        break;
                    }
                    stmts.push(*self.parse_expression()?);
                }
                if self.current.kind == TokenKind::Comma {
                    self.advance();
                }
                let end = self.consume_one(TokenKind::RightParen)?.span;
                if tuple || stmts.is_empty() {
                    Ok(NodeKind::TupleLiteral(stmts).make(span.extend(end)).into())
                } else {
                    let mut expr = stmts.pop().unwrap();
                    expr.span = span.extend(end);
                    Ok(expr.into())
                }
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
                let text = StringParser::new(text, span).parse()?;
                Ok(NodeKind::StringLiteral(text).make(span).into())
            }
            TokenKind::BooleanLiteral => {
                self.advance();
                Ok(NodeKind::BooleanLiteral(text.eq("True")).make(span).into())
            }
            TokenKind::FloatLiteral => {
                self.advance();
                let val = text
                    .parse()
                    .map_err(|err| InvalidFloat.make(span).with_message(err))?;
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
                let val = usize::from_str_radix(text, radix)
                    .map_err(|err| Box::new(InvalidInteger(base).make(span).with_message(err)))?;
                Ok(NodeKind::IntegerLiteral(val).make(span).into())
            }
            TokenKind::None => {
                self.advance();
                Ok(NodeKind::NoneLiteral.make(span).into())
            }
            _ => {
                self.advance();
                Err(UnexpectedToken(kind).make(span).into())
            }
        }
    }

    fn parse_signature(&mut self, closer: TokenKind) -> Result<Vec<FunctionArg>> {
        let mut arguments = Vec::new();
        let (mut pos_v, mut key_v) = (false, false);
        while self.current.kind != closer {
            if !arguments.is_empty() {
                self.consume_one(TokenKind::Comma)?;
            }
            let start = self.current.span;
            match self.current.kind {
                TokenKind::Star => {
                    self.advance();
                    let ident = self.consume_one(TokenKind::Identifier)?;
                    let span = start.extend(ident.span);
                    if pos_v {
                        return Err(SyntaxError(
                            "* argument must be the final argument".to_string(),
                        )
                        .make(span)
                        .into());
                    }
                    pos_v = true;
                    arguments.push(FunctionArg::PositionalVariadic(
                        span,
                        ident.text.to_string(),
                    ));
                }
                TokenKind::StarStar => {
                    self.advance();
                    let ident = self.consume_one(TokenKind::Identifier)?;
                    let span = start.extend(ident.span);
                    if key_v {
                        return Err(SyntaxError(
                            "** argument must be the final argument".to_string(),
                        )
                        .make(span)
                        .into());
                    }
                    key_v = true;
                    arguments.push(FunctionArg::KeywordVariadic(span, ident.text.to_string()));
                }
                _ => {
                    let ident = self.consume_one(TokenKind::Identifier)?;
                    match self.current.kind {
                        TokenKind::Equals => {
                            self.advance();
                            let expr = self.parse_expression()?;
                            let span = start.extend(expr.span);
                            arguments.push(FunctionArg::Keyword(
                                span,
                                ident.text.to_string(),
                                expr,
                            ));
                        }
                        _ => {
                            let span = start.extend(ident.span);
                            arguments.push(FunctionArg::Positional(span, ident.text.to_string()));
                        }
                    }
                }
            };
        }
        Ok(arguments)
    }
}

struct StringParser<'contents> {
    span: Span,
    source: &'contents str,
    char_indices: std::iter::Peekable<std::str::CharIndices<'contents>>,
    current_char: Option<char>,
    current_index: usize,
}

impl<'contents> StringParser<'contents> {
    pub fn new(source: &'contents str, span: Span) -> Self {
        let mut parser = Self {
            span,
            source,
            char_indices: source.char_indices().peekable(),
            current_char: None,
            current_index: 0,
        };
        parser.advance();
        parser
    }
    fn advance(&mut self) {
        let current = self.char_indices.next();
        self.current_char = current.map(|(_, c)| c);
        self.current_index = current.map(|(i, _)| i).unwrap_or(self.current_index + 1);
    }
    fn span_at(&mut self, i: usize) -> Span {
        Span::at(self.span.filename, self.span.start + i)
    }

    pub fn parse(&mut self) -> Result<String> {
        let mut buf = String::with_capacity(self.source.len());
        while let Some(char) = self.current_char {
            match char {
                '\\' => {
                    let start = self.span_at(self.current_index);
                    self.advance();
                    let escaped = self.current_char.expect("Lexer left a hanging escape");
                    self.advance();
                    match escaped {
                        '\\' | '\'' | '"' => buf.push(escaped),
                        'n' => buf.push('\n'),
                        'r' => buf.push('\r'),
                        't' => buf.push('\t'),
                        'b' => buf.push('\u{0008}'),
                        'f' => buf.push('\u{000C}'),
                        '0' => buf.push('\0'),
                        'u' => {
                            let text_start = self.current_index;
                            for _ in 0..4 {
                                self.advance();
                            }
                            let text_end = self.current_index;
                            let text_span = start.extend(self.span_at(text_end));
                            let text = &self.source[text_start..text_end];
                            let val = u16::from_str_radix(text, 16).map_err(|e| {
                                SyntaxError(format!("Invalid Unicode Escape Sequence: {text}"))
                                    .make(text_span)
                                    .with_message(e)
                                    .with_label(self.span.label().with_color(Color::Blue))
                            })?;
                            let u_char = char::decode_utf16(vec![val])
                                .next()
                                .expect("Got None from unicode decoder")
                                .map_err(|e| {
                                    SyntaxError(format!("Invalid Unicode Escape Sequence: {text}"))
                                        .make(text_span)
                                        .with_message(e)
                                        .with_label(self.span.label().with_color(Color::Blue))
                                })?;
                            buf.push(u_char);
                        }
                        unexpected => {
                            return Err(SyntaxError(format!(
                                "Invalid Escape Character: {unexpected}"
                            ))
                            .make(start.extend(self.span_at(self.current_index)))
                            .with_label(self.span.label().with_color(Color::Blue))
                            .into())
                        }
                    }
                }
                c => {
                    self.advance();
                    buf.push(c);
                }
            }
        }
        Ok(buf)
    }
}
