use std::fmt::{Debug, Display, Formatter};
use std::process::exit;
use std::sync::LazyLock;

use crate::report::{ReportConfig, ReportKind, ReportLevel};

pub static ARGS: LazyLock<Args> = LazyLock::new(|| Args::parse(std::env::args().skip(1).collect()));

struct ArgsError(String);

impl Display for ArgsError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ArgParser: {}", self.0)
    }
}

impl From<ArgsError> for ReportLevel {
    fn from(_value: ArgsError) -> Self {
        Self::Error
    }
}

impl ReportKind for ArgsError {}

macro_rules! error {
    ($($ident:tt)*) => {
        ArgsError(format!($($ident)*))
            .make(crate::span::Span::empty())
            .finish()
            .print(ReportConfig::default());
        exit(1);
    };
}

#[derive(Copy, Clone)]
pub struct Arg<T: Copy + Clone> {
    pub value: T,
    set: bool,
}

impl<T: Copy + Clone> Arg<T> {
    fn new(default: T) -> Self {
        Self {
            value: default,
            set: false,
        }
    }

    fn try_mut<N: std::fmt::Display>(&mut self, name: N, value: T) {
        if self.set {
            error!("{} may only be used once", name);
        }
        self.set = true;
        self.value = value;
    }

    pub fn to_value(&self) -> T {
        self.value.clone()
    }
}

impl<T: Copy + Clone> AsRef<T> for Arg<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T: Debug + Copy + Clone> Debug for Arg<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Arg({:?})", self.value)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Args {
    pub input: &'static str,
    pub output: Arg<&'static str>,
    pub debug: Arg<bool>,
    pub report_level: Arg<ReportLevel>,
    pub compact: Arg<bool>,
    pub context: Arg<bool>,
}

impl Args {
    pub fn config(&self) -> ReportConfig {
        ReportConfig {
            compact: self.compact.to_value(),
            code_context: self.context.to_value(),
        }
    }

    pub fn default() -> Self {
        Self {
            input: "",
            output: Arg::new("./out"),
            debug: Arg::new(false),
            report_level: Arg::new(ReportLevel::Warn),
            compact: Arg::new(false),
            context: Arg::new(true),
        }
    }

    fn handle_arg(
        &mut self,
        argument: &str,
        arguments: &mut std::iter::Peekable<std::vec::IntoIter<String>>,
    ) {
        let args: Vec<String> = if argument.starts_with("--") {
            vec![argument.into()]
        } else {
            argument.chars().skip(1).map(|c| format!("-{c}")).collect()
        };
        let args_len = args.len();

        for (i, arg) in args.into_iter().enumerate() {
            let _is_end = i == args_len - 1;

            macro_rules! is_end {
                () => {
                    if !_is_end {
                        error!("{} may only be used at the end of a group", arg);
                    }
                };
            }
            match arg.as_str() {
                "-h" => {
                    println!("Usage: {}", USAGE);
                    exit(0);
                }
                "--help" => {
                    println!("\x1b[1mUSAGE\x1b[0m\n{}\n\n{}", USAGE, HELP_MESSAGE);
                    exit(0);
                }
                "-V" | "--version" => {
                    println!("ruse {}", env!("CARGO_PKG_VERSION"));
                    exit(0);
                }
                "-d" | "--debug" => self.debug.try_mut(arg, true),
                "-l" | "--report-level" => {
                    is_end!();
                    let Some(value) = arguments.next() else {
                        error!("{} expected LEVEL", arg);
                    };
                    let level = match value.as_str() {
                        "advice" => ReportLevel::Advice,
                        "warn" => ReportLevel::Warn,
                        "error" => ReportLevel::Error,
                        "silent" => ReportLevel::Silent,
                        _ => {
                            error!("'{}' is not a valid LEVEL", value);
                        }
                    };
                    self.report_level.try_mut(arg, level);
                }
                "-o" | "--output" => {
                    is_end!();
                    let Some(output) = arguments.next() else {
                        error!("{} expected FILE", arg);
                    };
                    self.output.try_mut(arg, Box::leak(output.into_boxed_str()));
                }
                "--no-context" => {
                    self.context.try_mut(arg, false);
                }
                "--compact" => {
                    self.compact.try_mut(arg, true);
                }
                _ => {
                    error!("unrecognized argument {}", arg);
                }
            }
        }
    }

    pub fn parse(args: Vec<String>) -> Self {
        let mut out = Self::default();
        let mut args = args.into_iter().peekable();
        while let Some(arg) = args.next() {
            if arg.starts_with("-") {
                out.handle_arg(&arg, &mut args)
            } else {
                out.input = arg.leak();
                break;
            }
        }
        if let Some(arg) = args.next() {
            error!("unexpected argument '{}'", arg);
        }
        if out.input.is_empty() {
            error!("specify an input file");
        }

        out
    }
}

const USAGE: &str = "ruse [-hVd] [-l LEVEL] [-o FILE] <INPUT FILE>";
const HELP_MESSAGE: &str = "\x1b[1mDESCRIPTION\x1b[0m
    Ruse is a compiled programming language designed and developed by
    Haven Selph. It is intended to be a learning experience, and is
    absolutely not gauranteed to be consitent, functional in all
    contexts, or performant. Use at your own discretion.

\x1b[1mOPTIONS\x1b[0m
    -h, --help                        Show this message (or only usage with -h)
    -V, --version
    -d, --debug                       Show debug information (likely not useful for you)
    -l, --report-level LEVEL          Set minimum level for a report to be shown
       (default: error)               [advice|warn|error|silent]
    -o, --output FILE
        (default: ./out)

        --no-context                   Disable code context
        --compact                      Display reports in one line
";
