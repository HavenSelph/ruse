use std::fmt::{Display, Formatter};
use std::iter::FusedIterator;

use ariadne::Color;
use name_variant::NamedVariant;

use LexerReport::*;

use crate::files::get_source;
use crate::report::{ReportKind, ReportLevel, Result, SpanToLabel, UnwrapReport};
use crate::span::Span;
use crate::token::{Token, TokenKind};

#[derive(NamedVariant)]
enum LexerReport {
    SyntaxError,
    UnterminatedString,
    UnterminatedComment,
    UnexpectedCharacter(char),
}

impl Display for LexerReport {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.variant_name())?;
        match self {
            UnexpectedCharacter(c) => write!(f, ": {c}")?,
            _ => (),
        }
        Ok(())
    }
}

impl From<LexerReport> for ReportLevel {
    fn from(value: LexerReport) -> Self {
        match value {
            SyntaxError | UnterminatedString | UnterminatedComment | UnexpectedCharacter(_) => {
                Self::Error
            }
        }
    }
}

impl ReportKind for LexerReport {}

pub struct Lexer<'contents> {
    filename: &'static str,
    source: &'contents str,
    char_indices: std::iter::Peekable<std::str::CharIndices<'contents>>,
    current_char: Option<char>,
    current_index: usize,
}

impl<'contents> Lexer<'contents> {
    pub fn new(filename: &'static str) -> Self {
        let source = get_source(filename).unwrap_report().text();
        let mut lexer = Self {
            filename,
            source,
            char_indices: source.char_indices().peekable(),
            current_char: None,
            current_index: 0,
        };
        lexer.advance();
        lexer
    }

    fn advance(&mut self) {
        let current = self.char_indices.next();
        self.current_char = current.map(|(_, c)| c);
        self.current_index = current.map(|(i, _)| i).unwrap_or(self.current_index + 1);
    }
    fn peek(&mut self) -> Option<&(usize, char)> {
        self.char_indices.peek()
    }
    fn peek_char(&mut self) -> Option<&char> {
        self.peek().map(|(_, c)| c)
    }
    fn make(
        &mut self,
        start: usize,
        end: usize,
        token_kind: TokenKind,
        text: &'contents str,
    ) -> Result<Token<'contents>> {
        Ok(Token::new(token_kind, self.span(start, end), text))
    }
    fn make_simple(&mut self, start: usize, token_kind: TokenKind) -> Result<Token<'contents>> {
        self.make(
            start,
            self.current_index,
            token_kind,
            self.slice(start, self.current_index),
        )
    }
    fn make_advance(
        &mut self,
        start: usize,
        i: usize,
        token_kind: TokenKind,
    ) -> Result<Token<'contents>> {
        for _ in 0..i {
            self.advance();
        }
        self.make_simple(start, token_kind)
    }

    fn span(&self, start: usize, end: usize) -> Span {
        Span {
            filename: self.filename,
            start,
            end,
        }
    }
    fn span_from(&self, start: usize) -> Span {
        self.span(start, self.current_index)
    }

    fn span_at(&self, start: usize) -> Span {
        Span::at(self.filename, start)
    }
    fn slice(&self, start: usize, end: usize) -> &'contents str {
        &self.source[start..end]
    }

    pub fn lex_token(&mut self) -> Result<Token<'contents>> {
        loop {
            let Some(char) = self.current_char else {
                return Ok(Token::new(
                    TokenKind::EOF,
                    self.span_from(self.current_index),
                    "",
                ));
            };
            let start = self.current_index;
            return match char {
                c if c.is_whitespace() => {
                    self.advance();
                    continue;
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    loop {
                        match self.current_char {
                            Some('a'..='z' | 'A'..='Z' | '_') => self.advance(),
                            _ => break,
                        }
                    }
                    let kind = match self.slice(start, self.current_index) {
                        "let" => TokenKind::Let,
                        "True" | "False" => TokenKind::BooleanLiteral,
                        _ => TokenKind::Identifier,
                    };
                    self.make_simple(start, kind)
                }
                '0' if self.peek_char().is_some_and(|c| "box".contains(*c)) => {
                    self.advance();
                    let base = match self.current_char.unwrap() {
                        'b' => Base::Binary,
                        'o' => Base::Octal,
                        'x' => Base::Hexadecimal,
                        _ => unreachable!(),
                    };
                    self.advance();
                    self.lex_integer(start, base)?;
                    self.make_simple(start + 2, base.into())
                }
                '0'..='9' => {
                    self.lex_integer(start, Base::Decimal)?;
                    if let Some('.') = self.current_char {
                        self.advance();
                        self.lex_integer(start, Base::Decimal)?;
                        if let Some('.') = self.current_char {
                            return Err(SyntaxError
                                .make(self.span_at(self.current_index))
                                .with_message("Second decimal place in float")
                                .with_label(
                                    self.span_from(start).label().with_color(Color::BrightBlue),
                                )
                                .into());
                        }
                    }
                    let kind = if self.slice(start, self.current_index).contains('.') {
                        TokenKind::FloatLiteral
                    } else {
                        TokenKind::IntegerLiteralDec
                    };
                    self.make_simple(start, kind)
                }
                '"' | '\'' => {
                    self.lex_quoted_literal(start, char)?;
                    self.make(
                        start,
                        self.current_index,
                        TokenKind::StringLiteral,
                        self.slice(start + 1, self.current_index - 1),
                    )
                }
                '=' => self.make_advance(start, 1, TokenKind::Equals),
                '-' => match self.peek_char() {
                    Some('=') => self.make_advance(start, 2, TokenKind::MinusEquals),
                    _ => self.make_advance(start, 1, TokenKind::Minus),
                },
                '.' => self.make_advance(start, 1, TokenKind::Period),
                '+' => match self.peek_char() {
                    Some('=') => self.make_advance(start, 2, TokenKind::PlusEquals),
                    _ => self.make_advance(start, 1, TokenKind::Plus),
                },
                ';' => self.make_advance(start, 1, TokenKind::SemiColon),
                '/' => match self.peek_char() {
                    Some('/') => {
                        while self.current_char.is_some_and(|c| c != '\n') {
                            self.advance();
                        }
                        continue;
                    }
                    Some('*') => {
                        let mut depth = 0;
                        while let Some(c) = self.current_char {
                            match c {
                                '/' if self.peek_char().is_some_and(|c| *c == '*') => {
                                    depth += 1;
                                    self.advance();
                                }
                                '*' if self.peek_char().is_some_and(|c| *c == '/') => {
                                    depth -= 1;
                                    self.advance();
                                }
                                _ => self.advance(),
                            }
                            if depth == 0 {
                                break;
                            }
                        }
                        if depth > 0 {
                            return Err(if depth > 1 {
                                UnterminatedComment
                                    .make(self.span(start, start + 2))
                                    .with_message(format!("Opened {} more time(s)", depth - 1))
                            } else {
                                UnterminatedComment.make(self.span(start, start + 2))
                            }
                            .into());
                        }
                        continue;
                    }
                    Some('=') => self.make_advance(start, 2, TokenKind::SlashEquals),
                    _ => self.make_advance(start, 1, TokenKind::Slash),
                },
                '*' => match self.peek_char() {
                    Some('=') => self.make_advance(start, 2, TokenKind::StarEquals),
                    Some('*') => {
                        self.advance();
                        match self.peek_char() {
                            Some('=') => self.make_advance(start, 2, TokenKind::StarStarEquals),
                            _ => self.make_advance(start, 1, TokenKind::StarStar),
                        }
                    }
                    _ => self.make_advance(start, 1, TokenKind::Star),
                },
                '(' => self.make_advance(start, 1, TokenKind::LeftParen),
                ')' => self.make_advance(start, 1, TokenKind::RightParen),
                '[' => self.make_advance(start, 1, TokenKind::LeftBracket),
                ']' => self.make_advance(start, 1, TokenKind::RightBracket),
                '{' => self.make_advance(start, 1, TokenKind::LeftBrace),
                '}' => self.make_advance(start, 1, TokenKind::RightBrace),
                _ => {
                    self.advance();
                    return Err(UnexpectedCharacter(char).make(self.span_at(start)).into());
                }
            };
        }
    }

    fn lex_quoted_literal(&mut self, start: usize, closer: char) -> Result<()> {
        self.advance();
        while let Some(char) = self.current_char {
            match char {
                c if c == closer => break,
                '\\' if self.peek_char().is_some_and(|c| *c == closer) => {
                    self.advance();
                    self.advance();
                }
                '\n' => break,
                _ => self.advance(),
            }
        }
        if !self.current_char.is_some_and(|c| c == closer) {
            return Err(UnterminatedString.make(self.span_from(start)).into());
        }
        self.advance();
        Ok(())
    }

    fn lex_integer(&mut self, start: usize, base: Base) -> Result<()> {
        while let Some(char) = self.current_char {
            match (base, char.to_ascii_lowercase()) {
                (Base::Binary, '0'..='1')
                | (Base::Octal, '0'..='7')
                | (Base::Decimal, '0'..='9')
                | (Base::Hexadecimal, '0'..='9' | 'a'..='f') => {
                    self.advance();
                }
                (_, '0'..='9' | 'a'..='z') => {
                    return Err(SyntaxError
                        .make(self.span_at(self.current_index))
                        .with_message(format!("{char:?} invalid for base {base:?}"))
                        .with_label(
                            self.span(start, self.current_index)
                                .label()
                                .with_color(Color::BrightBlue),
                        )
                        .into())
                }
                (_, '_') => self.advance(),
                _ => break,
            }
        }
        Ok(())
    }
}

pub struct LexerIterator<'contents> {
    exhausted: bool,
    lexer: Lexer<'contents>,
}

impl<'contents> Iterator for LexerIterator<'contents> {
    type Item = Result<Token<'contents>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.exhausted {
            return None;
        }
        let token = self.lexer.lex_token();
        Some(match token {
            Ok(t) => {
                if t.kind == TokenKind::EOF {
                    self.exhausted = true;
                }
                Ok(t)
            }
            Err(e) => Err(e),
        })
    }
}

impl<'contents> FusedIterator for LexerIterator<'contents> {}

impl<'contents> IntoIterator for Lexer<'contents> {
    type Item = Result<Token<'contents>>;
    type IntoIter = LexerIterator<'contents>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIterator {
            exhausted: false,
            lexer: self,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Base {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl From<Base> for TokenKind {
    fn from(value: Base) -> Self {
        match value {
            Base::Binary => Self::IntegerLiteralBin,
            Base::Octal => Self::IntegerLiteralOct,
            Base::Decimal => Self::IntegerLiteralDec,
            Base::Hexadecimal => Self::IntegerLiteralHex,
        }
    }
}
