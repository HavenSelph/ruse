use crate::span::Span;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
    Ampersand,
    AmpersandAmpersand,
    At,
    Bang,
    BangEquals,
    BooleanLiteral,
    Break,
    Colon,
    Comma,
    Continue,
    EOF,
    Else,
    Equals,
    EqualsEquals,
    FatArrow,
    FloatLiteral,
    Fn,
    For,
    GreaterThan,
    GreaterThanEquals,
    Identifier,
    If,
    In,
    IntegerLiteralBin,
    IntegerLiteralDec,
    IntegerLiteralHex,
    IntegerLiteralOct,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessThan,
    LessThanEquals,
    Let,
    Minus,
    MinusEquals,
    None,
    Percent,
    PercentEquals,
    Period,
    PeriodPeriod,
    Pipe,
    PipePipe,
    Plus,
    PlusEquals,
    QuestionMark,
    Return,
    RightBrace,
    RightBracket,
    RightParen,
    SemiColon,
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    StarStar,
    StarStarEquals,
    StringLiteral,
    While,
    Not,
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
    pub newline_before: bool,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, span: Span, text: &'a str) -> Self {
        Token {
            kind,
            span,
            text,
            newline_before: false,
        }
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
