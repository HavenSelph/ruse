use ariadne::Color;

use crate::report::{Report, ReportKind, ReportSender, Result, SpanToLabel};
use crate::span::Span;
use crate::token::{Token, TokenKind};

pub struct Lexer<'contents> {
    filename: &'static str,
    source: &'contents str,
    char_indices: std::iter::Peekable<std::str::CharIndices<'contents>>,
    current_char: Option<char>,
    current_index: usize,
    pub tokens: Vec<Token<'contents>>,
    reporter: ReportSender,
}

impl<'contents> Lexer<'contents> {
    pub fn new(filename: &'static str, source: &'contents str, reporter: ReportSender) -> Self {
        let mut lexer = Self {
            filename,
            source,
            char_indices: source.char_indices().peekable(),
            current_char: None,
            current_index: 0,
            tokens: Vec::new(),
            reporter,
        };
        lexer.advance();
        lexer
    }

    fn report(&mut self, report: Box<Report>) {
        self.reporter.report(report);
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
    fn push(&mut self, start: usize, end: usize, token_kind: TokenKind, text: &'contents str) {
        self.tokens
            .push(Token::new(token_kind, self.span_from(start), text));
    }
    fn push_simple(&mut self, start: usize, token_kind: TokenKind) {
        self.push(
            start,
            self.current_index,
            token_kind,
            self.slice(start, self.current_index),
        );
    }
    fn push_advance(&mut self, start: usize, i: usize, token_kind: TokenKind) {
        for _ in 0..i {
            self.advance();
        }
        self.push_simple(start, token_kind);
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

    pub fn lex(&mut self) {
        macro_rules! report {
            ($result:expr) => {
                match $result {
                    Ok(v) => v,
                    Err(e) => {
                        self.report(e.finish().into());
                        continue;
                    }
                }
            };
        }

        while let Some(char) = self.current_char {
            let start = self.current_index;
            match char {
                c if c.is_whitespace() => self.advance(),
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
                    self.push_simple(start, kind);
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
                    report!(self.lex_integer(start, base));
                    self.push_simple(start + 2, base.into());
                }
                '0'..='9' => {
                    report!(self.lex_integer(start, Base::Decimal));
                    if let Some('.') = self.current_char {
                        self.advance();
                        report!(self.lex_integer(start, Base::Decimal));
                        if let Some('.') = self.current_char {
                            self.report(
                                ReportKind::InvalidFloatLiteral
                                    .make(self.span_at(self.current_index))
                                    .with_message("Second decimal point")
                                    .finish()
                                    .into(),
                            );
                            continue;
                        }
                    }
                    let kind = if self.slice(start, self.current_index).contains('.') {
                        TokenKind::FloatLiteral
                    } else {
                        TokenKind::IntegerLiteralDec
                    };
                    self.push_simple(start, kind);
                }
                '"' | '\'' => {
                    report!(self.lex_quoted_literal(start, char));
                    self.push(
                        start,
                        self.current_index,
                        TokenKind::StringLiteral,
                        self.slice(start + 1, self.current_index - 1),
                    )
                }
                '=' => self.push_advance(start, 1, TokenKind::Equals),
                '-' => match self.peek_char() {
                    Some('=') => self.push_advance(start, 2, TokenKind::MinusEquals),
                    _ => self.push_advance(start, 1, TokenKind::Minus),
                },
                '.' => self.push_advance(start, 1, TokenKind::Period),
                '+' => match self.peek_char() {
                    Some('=') => self.push_advance(start, 2, TokenKind::PlusEquals),
                    _ => self.push_advance(start, 1, TokenKind::Plus),
                },
                ';' => self.push_advance(start, 1, TokenKind::SemiColon),
                '/' => match self.peek_char() {
                    Some('/') => {
                        while self.current_char.is_some_and(|c| c != '\n') {
                            self.advance();
                        }
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
                            self.report(
                                if depth > 1 {
                                    ReportKind::UnterminatedMultiLineComment
                                        .make(self.span_from(start))
                                        .with_message(format!("Opened {} more time(s)", depth - 1))
                                } else {
                                    ReportKind::UnterminatedMultiLineComment
                                        .make(self.span_from(start))
                                }
                                .finish()
                                .into(),
                            );
                        }
                    }
                    Some('=') => self.push_advance(start, 2, TokenKind::Equals),
                    _ => self.push_advance(start, 1, TokenKind::Slash),
                },
                '*' => match self.peek_char() {
                    Some('=') => self.push_advance(start, 2, TokenKind::StarEquals),
                    Some('*') => {
                        self.advance();
                        match self.peek_char() {
                            Some('=') => self.push_advance(start, 2, TokenKind::StarStarEquals),
                            _ => self.push_advance(start, 1, TokenKind::StarStar),
                        }
                    }
                    _ => self.push_advance(start, 1, TokenKind::Star),
                },
                '(' => self.push_advance(start, 1, TokenKind::LeftParen),
                ')' => self.push_advance(start, 1, TokenKind::RightParen),
                '[' => self.push_advance(start, 1, TokenKind::LeftBracket),
                ']' => self.push_advance(start, 1, TokenKind::RightBracket),
                '{' => self.push_advance(start, 1, TokenKind::LeftBrace),
                '}' => self.push_advance(start, 1, TokenKind::RightBrace),
                _ => {
                    self.report(
                        ReportKind::UnexpectedCharacter(char)
                            .make(self.span_at(start))
                            .finish()
                            .into(),
                    );
                    self.advance();
                }
            };
        }
        self.tokens.push(Token::new(
            TokenKind::EOF,
            self.span_from(self.current_index),
            "",
        ));
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
            return Err(ReportKind::UnterminatedStringLiteral
                .make(self.span_from(start))
                .into());
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
                    return Err(ReportKind::InvalidIntegerLiteral(base)
                        .make(self.span_from(start))
                        .with_label(
                            self.span(start, self.current_index - 1)
                                .label()
                                .with_color(Color::Blue),
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
