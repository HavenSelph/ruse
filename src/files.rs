use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{BufReader, Read, Result};
use std::sync::{LazyLock, RwLock};

use ariadne::{Cache, Source};

use crate::report::{ReportKind, ReportLevel, UnwrapReport};
use crate::span::Span;

struct InvalidFile(String);

impl Display for InvalidFile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "InvalidFile: {}", self.0)
    }
}

impl From<InvalidFile> for ReportLevel {
    fn from(_value: InvalidFile) -> Self {
        Self::Error
    }
}

impl ReportKind for InvalidFile {}

static CACHE: LazyLock<RwLock<HashMap<&'static str, &'static Source<&'static str>>>> =
    LazyLock::new(|| {
        let mut hm = HashMap::with_capacity(2);
        hm.insert("", &*Box::leak(Source::from("").into()));
        RwLock::new(hm)
    });

pub struct ScannerCache {}
impl Cache<&'static str> for ScannerCache {
    type Storage = &'static str;

    fn fetch(
        &mut self,
        id: &&'static str,
    ) -> std::result::Result<&Source<Self::Storage>, Box<dyn Debug + '_>> {
        Ok(get_source(id).unwrap_report())
    }

    fn display<'a>(&self, id: &'a &'static str) -> Option<Box<dyn Display + 'a>> {
        Some(Box::new(id))
    }
}

pub struct Scanner {
    filename: &'static str,
    index: usize,
    contents: &'static mut String,
    reader: BufReader<File>,
}

impl Scanner {
    fn new(filename: &'static str) -> Result<Self> {
        let file = File::open(filename)?;
        let file_size = file.metadata()?.len() as usize;

        Ok(Self {
            filename,
            index: 0,
            contents: Box::leak(Box::new(String::with_capacity(file_size))),
            reader: BufReader::new(file),
        })
    }

    fn read(mut self) -> Result<Self> {
        let mut buf = [0u8; 1];

        while self.reader.read(&mut buf)? > 0 {
            match std::str::from_utf8(&buf) {
                Ok(s) => match s {
                    "\r" => {
                        continue;
                    }
                    _ => self.contents.push_str(s),
                },
                Err(_) => {
                    eprintln!("Failed to read from file: {}", self.filename);
                    std::process::exit(1);
                }
            }
            self.index += 1;
        }
        Ok(self)
    }
}

pub fn get_source(filename: &'static str) -> crate::report::ResultFinal<&Source<&'static str>> {
    {
        let binding = CACHE.read().unwrap();
        if let Some(contents) = binding.get(filename) {
            return Ok(contents);
        }
    }

    let contents = Scanner::new(filename)
        .map_err(|e| {
            Box::new(
                InvalidFile(filename.to_string())
                    .make(Span::empty())
                    .with_note(e)
                    .finish(),
            )
        })?
        .read()
        .map_err(|e| {
            Box::new(
                InvalidFile(filename.to_string())
                    .make(Span::empty())
                    .with_note(e)
                    .finish(),
            )
        })?
        .contents
        .as_str();
    let mut binding = CACHE.write().unwrap();
    let inserted = binding
        .entry(filename)
        .or_insert(Box::leak(Box::new(Source::from(contents))));
    Ok(inserted)
}
