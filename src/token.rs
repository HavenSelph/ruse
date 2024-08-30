use std::fmt::{Debug, Display, Formatter};

use crate::span::Span;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
    EOF,
    Equals,
    FloatLiteral,
    Identifier,
    BooleanLiteral,
    IntegerLiteralBin,
    IntegerLiteralDec,
    IntegerLiteralHex,
    IntegerLiteralOct,
    Let,
    Minus,
    MinusEquals,
    Period,
    Plus,
    PlusEquals,
    SemiColon,
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    StarStar,
    StarStarEquals,
    StringLiteral,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Copy, Clone)]
pub struct Token<'source> {
    pub kind: TokenKind,
    pub span: Span,
    pub text: &'source str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, span: Span, text: &'a str) -> Self {
        Token { kind, span, text }
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{{} {:?}}}", self.kind, self.span, self.text)
    }
}

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}{{{:?} {:?}}}", self.kind, self.span, self.text)
    }
}
