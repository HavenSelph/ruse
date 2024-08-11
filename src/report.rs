use std::fmt::{Display, Formatter};
use std::fmt::Write;
use std::sync::mpsc::{Receiver, Sender};

use ariadne::{Color, Config};
use name_variant::NamedVariant;
use owo_colors::OwoColorize;

use crate::args::ARGS;
use crate::dprintln;
use crate::files::ScannerCache;
use crate::span::Span;

pub type Result<T> = std::result::Result<T, Box<ReportBuilder>>;
pub type ResultFinal<T> = std::result::Result<T, Box<Report>>;
pub type ResultErrorless<T> = std::result::Result<T, ()>;

pub type Label = ariadne::Label<Span>;

pub trait SpanToLabel<T: ariadne::Span>: ariadne::Span {
    fn label(&self) -> ariadne::Label<T>;

    fn labeled<M: Display>(&self, message: M) -> ariadne::Label<T> {
        self.label().with_message(message)
    }
}

impl SpanToLabel<Span> for Span {
    fn label(&self) -> ariadne::Label<Span> {
        Label::new(self.clone())
    }
}

pub trait UnwrapReport<T>
where
    Self: Sized,
{
    fn unwrap_report(self) -> T;
}

impl<T> UnwrapReport<T> for Result<T> {
    fn unwrap_report(self) -> T {
        match self {
            Ok(val) => val,
            Err(err) => {
                err.finish().eprint(ARGS.config());
                std::process::exit(1);
            }
        }
    }
}

impl<T> UnwrapReport<T> for ResultFinal<T> {
    fn unwrap_report(self) -> T {
        match self {
            Ok(val) => val,
            Err(err) => {
                err.eprint(ARGS.config());
                std::process::exit(1);
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, PartialEq)]
pub enum ReportLevel {
    Silent,
    Error,
    Warn,
    Advice,
}

impl From<ReportLevel> for ariadne::ReportKind<'_> {
    fn from(value: ReportLevel) -> Self {
        match value {
            ReportLevel::Advice => Self::Advice,
            ReportLevel::Warn => Self::Warning,
            ReportLevel::Error => Self::Error,
            ReportLevel::Silent => panic!("Turned SILENT into ReportKind"),
        }
    }
}

impl From<ReportLevel> for Color {
    fn from(value: ReportLevel) -> Self {
        match value {
            ReportLevel::Advice => Color::BrightBlue,
            ReportLevel::Warn => Color::Yellow,
            ReportLevel::Error => Color::Red,
            ReportLevel::Silent => panic!("Turned SILENT into color"),
        }
    }
}

#[derive(NamedVariant)]
pub enum ReportKind {
    ArgParser(String),
    UnterminatedMultiLineComment,
    InvalidIntegerLiteral(crate::lexer::Base),
    InvalidFloatLiteral,
    UnterminatedStringLiteral,
    UnexpectedCharacter(char),
    UnexpectedToken(crate::token::TokenKind),
    Custom { level: ReportLevel, title: String },
    InvalidFile(&'static str),
    UnexpectedEOF,
}

impl ReportKind {
    fn as_level(&self) -> ReportLevel {
        match self {
            ReportKind::UnterminatedMultiLineComment
            | ReportKind::InvalidIntegerLiteral(..)
            | ReportKind::UnterminatedStringLiteral
            | ReportKind::ArgParser(..)
            | ReportKind::UnexpectedCharacter(..)
            | ReportKind::UnexpectedToken { .. }
            | ReportKind::InvalidFloatLiteral
            | ReportKind::InvalidFile(..)
            | ReportKind::UnexpectedEOF => ReportLevel::Error,
            ReportKind::Custom { level, .. } => level.clone(),
        }
    }

    pub fn custom(level: ReportLevel, title: String) -> Self {
        Self::Custom { level, title }
    }
    pub fn make(self, span: Span) -> ReportBuilder {
        ReportBuilder {
            title: format!("{self}"),
            span,
            level: self.as_level(),
            message: None,
            labels: Vec::new(),
            kind: self,
            help: None,
            note: None,
        }
    }
}

impl Display for ReportKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let ReportKind::Custom { title, .. } = self {
            return write!(f, "{title}");
        }
        write!(f, "[{}]", self.variant_name())?;
        match self {
            ReportKind::ArgParser(message) => write!(f, " {message}")?,
            ReportKind::UnexpectedCharacter(char) => write!(f, " {char}")?,
            ReportKind::UnexpectedToken(token) => write!(f, " {token}")?,
            ReportKind::InvalidIntegerLiteral(base) => write!(f, " of base {base:?}")?,
            ReportKind::UnterminatedMultiLineComment => {}
            ReportKind::UnterminatedStringLiteral => {}
            ReportKind::UnexpectedEOF => {}
            ReportKind::InvalidFloatLiteral => {}
            ReportKind::InvalidFile(file) => write!(f, " {file:?}")?,
            ReportKind::Custom { .. } => unreachable!(),
        }
        Ok(())
    }
}

pub struct ReportBuilder {
    kind: ReportKind,
    title: String,
    span: Span,
    level: ReportLevel,
    message: Option<String>,
    labels: Vec<Label>,
    help: Option<String>,
    note: Option<String>,
}

impl ReportBuilder {
    pub fn set_help<T: ToString>(&mut self, help: T) -> &mut Self {
        self.help = Some(help.to_string());
        self
    }

    pub fn with_help<T: ToString>(mut self, help: T) -> Self {
        self.set_help(help);
        self
    }

    pub fn set_note<T: ToString>(&mut self, note: T) -> &mut Self {
        self.note = Some(note.to_string());
        self
    }

    pub fn with_note<T: ToString>(mut self, note: T) -> Self {
        self.set_note(note);
        self
    }

    pub fn set_message<T: ToString>(&mut self, message: T) -> &mut Self {
        self.message = Some(message.to_string());
        self
    }

    pub fn with_message<T: ToString>(mut self, message: T) -> Self {
        self.set_message(message);
        self
    }

    pub fn push_label(&mut self, label: Label) -> &mut Self {
        self.labels.push(label);
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.push_label(label);
        self
    }

    fn labels(&self) -> Vec<Label> {
        let mut labels = Vec::new();
        labels.push(
            if let Some(message) = &self.message {
                self.span.labeled(message)
            } else {
                self.span.label()
            }
            .with_color(self.level.into()),
        );
        labels.extend_from_slice(&self.labels);
        labels
    }

    pub fn finish(self) -> Report {
        Report {
            labels: self.labels(),
            help: self.help,
            note: self.note,
            title: self.title,
            span: self.span,
            level: self.level,
        }
    }
}

pub struct Report {
    title: String,
    span: Span,
    level: ReportLevel,
    labels: Vec<Label>,
    help: Option<String>,
    note: Option<String>,
}

#[derive(Copy, Clone)]
pub struct ReportConfig {
    pub code_context: bool,
    pub compact: bool,
}

impl Default for ReportConfig {
    fn default() -> Self {
        ReportConfig {
            code_context: true,
            compact: false,
        }
    }
}

impl Report {
    fn into_ariadne_report(self, config: ReportConfig) -> ariadne::Report<'static, Span> {
        let level = if config.compact || !config.code_context {
            ariadne::ReportKind::Custom(
                format!("[{}] {:?}", self.span, self.level).leak(),
                self.level.into(),
            )
        } else {
            self.level.into()
        };
        let mut builder = ariadne::Report::build(level, self.span.filename, self.span.start)
            .with_message(self.title)
            .with_config(Config::default().with_compact(true));
        if config.compact {
            return builder.finish();
        }
        if config.code_context {
            builder.add_labels(self.labels);
        }
        if let Some(help) = self.help {
            builder.set_help(help);
        }
        if let Some(note) = self.note {
            builder.set_note(note);
        }
        builder.finish()
    }

    pub fn eprint(self, config: ReportConfig) {
        let (empty, help, note) = (self.labels.is_empty(), self.help.clone(), self.note.clone());
        self.into_ariadne_report(config)
            .eprint(ScannerCache {})
            .expect("Failed to print error.");
        if config.compact {
            return;
        }
        if let Some(help) = help {
            eprintln!("{}", help);
        }
        if let Some(note) = note {
            eprintln!("{}", note)
        }
    }
}

pub struct ReportChannel {
    sender: Sender<Box<Report>>,
    receiver: Receiver<Box<Report>>,
}

pub struct ReportSender {
    sender: Sender<Box<Report>>,
}

impl ReportSender {
    pub fn report(&self, report: Box<Report>) {
        self.sender.send(report).expect("Failed to send report");
    }
}

impl ReportChannel {
    pub fn new() -> ReportChannel {
        let (sender, receiver) = std::sync::mpsc::channel();
        ReportChannel { sender, receiver }
    }

    pub fn get_sender(&self) -> ReportSender {
        ReportSender {
            sender: self.sender.clone(),
        }
    }

    pub fn check_reports(&self) {
        let mut errors = 0usize;
        for report in self.receiver.try_iter() {
            if report.level == ReportLevel::Error {
                errors += 1;
            }
            dprintln!("Report({},{:?})", report.title, report.span);
            if ARGS.report_level.to_value() < report.level {
                continue;
            }
            report.eprint(ARGS.config())
        }
        if errors > 0 {
            if ARGS.report_level.to_value() != ReportLevel::Silent {
                eprint!(
                    "{}",
                    format_args!("Failed with {errors} errors emitted.").red()
                );
            }
            std::process::exit(1);
        }
    }
}
