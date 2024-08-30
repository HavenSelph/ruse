#![allow(unused)]

use std::fmt::Display;
use std::sync::mpsc::{Receiver, Sender};

use ariadne::{Color, Config};
use owo_colors::OwoColorize;

use crate::args::ARGS;
use crate::dprintln;
use crate::files::ScannerCache;
use crate::span::Span;

pub type Result<T> = std::result::Result<T, Box<ReportBuilder>>;
pub type ResultFinal<T> = std::result::Result<T, Box<Report>>;
pub type ResultErrorless<T> = std::result::Result<T, ()>;

pub struct Label {
    span: Span,
    message: Option<String>,
    color: Option<Color>,
}

impl Label {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            message: None,
            color: None,
        }
    }
    pub fn set_message<T: Display>(&mut self, message: T) -> &mut Self {
        self.message = Some(message.to_string());
        self
    }

    pub fn with_message<T: Display>(mut self, message: T) -> Self {
        self.set_message(message);
        self
    }
    pub fn set_color(&mut self, color: Color) -> &mut Self {
        self.color = Some(color);
        self
    }

    pub fn with_color(mut self, color: Color) -> Self {
        self.set_color(color);
        self
    }

    fn as_ariadne_label(&self, level: ReportLevel) -> ariadne::Label<Span> {
        let mut label =
            ariadne::Label::new(self.span).with_color(if let Some(color) = self.color {
                color
            } else {
                level.into()
            });
        if let Some(text) = self.message.clone() {
            label = label.with_message(text);
        }
        label
    }
}
pub trait SpanToLabel<T: ariadne::Span>: ariadne::Span {
    fn label(&self) -> Label;

    fn labeled<M: Display>(&self, message: M) -> Label {
        self.label().with_message(message)
    }
}

impl SpanToLabel<Span> for Span {
    fn label(&self) -> Label {
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

pub trait ReportKind: Into<ReportLevel> + Display {
    fn make(self, span: Span) -> ReportBuilder {
        ReportBuilder {
            title: self.to_string(),
            level: self.into(),
            span,
            message: None,
            help: None,
            note: None,
            labels: Vec::new(),
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
            ReportLevel::Error => Self::Error,
            ReportLevel::Warn => Self::Warning,
            ReportLevel::Advice => Self::Advice,
            ReportLevel::Silent => panic!("Turned SILENT into report kind"),
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

#[must_use]
pub struct ReportBuilder {
    level: ReportLevel,
    span: Span,
    title: String,
    message: Option<String>,
    help: Option<String>,
    note: Option<String>,
    labels: Vec<Label>,
}

impl ReportBuilder {
    pub fn set_help<T: Display>(&mut self, help: T) -> &mut Self {
        self.help = Some(help.to_string());
        self
    }

    pub fn with_help<T: Display>(mut self, help: T) -> Self {
        self.set_help(help);
        self
    }

    pub fn set_note<T: Display>(&mut self, note: T) -> &mut Self {
        self.note = Some(note.to_string());
        self
    }

    pub fn with_note<T: Display>(mut self, note: T) -> Self {
        self.set_note(note);
        self
    }

    pub fn set_message<T: Display>(&mut self, message: T) -> &mut Self {
        self.message = Some(message.to_string());
        self
    }

    pub fn with_message<T: Display>(mut self, message: T) -> Self {
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

    pub fn finish(self) -> Report {
        let mut labels = Vec::with_capacity(self.labels.len() + 1);
        labels.push(
            if let Some(message) = &self.message {
                self.span.labeled(message)
            } else {
                self.span.label()
            }
            .with_color(self.level.into()),
        );
        labels.extend(self.labels);
        Report {
            level: self.level,
            span: self.span,
            title: self.title,
            help: self.help,
            note: self.note,
            labels,
        }
    }
}

#[derive(Copy, Clone)]
pub struct ReportConfig {
    pub code_context: bool,
    pub compact: bool,
}

impl Default for ReportConfig {
    fn default() -> Self {
        Self {
            code_context: false,
            compact: false,
        }
    }
}

pub struct Report {
    level: ReportLevel,
    span: Span,
    title: String,
    help: Option<String>,
    note: Option<String>,
    labels: Vec<Label>,
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
            builder.add_labels(
                self.labels
                    .iter()
                    .map(|label| label.as_ariadne_label(self.level)),
            );
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
        if empty {
            if let Some(help) = help {
                eprintln!("{}", help);
            }
            if let Some(note) = note {
                eprintln!("{}", note)
            }
        }
    }
}

pub struct ReportChannel {
    sender: Sender<Box<Report>>,
    receiver: Receiver<Box<Report>>,
}

#[derive(Clone)]
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
