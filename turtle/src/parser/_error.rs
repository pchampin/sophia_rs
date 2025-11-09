//! I define [`Error`].

use sophia_api::source::{StreamError::SourceError, StreamResult, StreamResultExt};
use sophia_iri::{InvalidIri, resolve::IriParseError};

use super::_common::GenericSource;

/// Parsing error, capturing the position in the input where the error was encountered.
#[derive(thiserror::Error, Debug)]
#[error("{kind} at {line}:{col}")]
pub struct Error {
    kind: ErrorKind,
    line: usize,
    col: usize,
}

impl Error {
    /// Construct an [`Error`]
    pub fn new<E: Into<ErrorKind>>(err: E, line: usize, col: usize) -> Self {
        Error {
            kind: err.into(),
            line,
            col,
        }
    }

    /// Return the [kind][`ErrorKind`]
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// Return the position in the input.
    ///
    /// NB: lines and columns are numbered from 0.
    pub fn position(&self) -> (usize, usize) {
        (self.line, self.col)
    }

    /// Display this error with context.
    ///
    /// This method numbers line and column starting from 1 instead of 0,
    /// as expected by text editors.
    pub fn in_context(&self, context: &str) -> String {
        format!("{context}:{}:{} {}", self.line + 1, self.col + 1, self.kind,)
    }
}

/// Kind of [parsing errors][`Error`]
#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
    /// Invalid bnode label
    #[error("Invalid bnode label")]
    Bnode,
    /// Unespected end of data
    #[error("Unexpected end of data in state {0}")]
    EOF(String),
    /// Unexpected character(s) in the input
    #[error("Expected {0}")]
    Expected(String),
    /// IO error
    #[error("IO error: {0}")]
    IO(#[from] std::io::Error),
    /// Invalid IRI
    #[error("Invalid IRI: {0}")]
    Iri(#[from] sophia_iri::InvalidIri),
    /// Invalid prefix name
    #[error("Invalid prefix: {0}")]
    InvalidPrefix(#[from] sophia_api::prefix::InvalidPrefix),
    /// Invalid literal
    #[error("Invalid literal: language-tagged string without a language tag")]
    InvalidLiteral,
    /// Unknown prefix in PNAME
    #[error("Unknown prefix: {0}")]
    UnknownPrefix(String),
    /// Invalid escape sequence
    #[error("Invalid escape sequence")]
    InvalidEscape,
    /// Invalid variable name
    #[error("Invalid variable name")]
    Variable,
}

impl From<IriParseError> for ErrorKind {
    fn from(value: IriParseError) -> Self {
        InvalidIri(value.to_string()).into()
    }
}

pub(crate) trait ResultExt<T, O, E>: Sized
where
    E: std::error::Error,
{
    fn wrap_in(self, wrapper: T) -> StreamResult<O, Error, E> {
        self.wrap_in_at(wrapper, 0)
    }
    fn wrap_in_at(self, wrapper: T, offset: usize) -> StreamResult<O, Error, E>;
}

impl<S, O, E, E2> ResultExt<&'_ S, O, E2> for Result<O, E>
where
    S: GenericSource + ?Sized,
    E: Into<ErrorKind>,
    E2: std::error::Error,
{
    fn wrap_in_at(self, wrapper: &S, offset: usize) -> StreamResult<O, Error, E2> {
        self.map_err(|err| {
            SourceError(Error::new(
                err,
                wrapper.inner().line,
                wrapper.inner().col + offset,
            ))
        })
    }
}

pub(crate) trait AdjustCol: Sized {
    fn adjust_col(self, offset: usize) -> Self;
}

impl AdjustCol for Error {
    fn adjust_col(mut self, offset: usize) -> Self {
        self.col += offset;
        self
    }
}

impl<O, E: std::error::Error> AdjustCol for StreamResult<O, Error, E> {
    fn adjust_col(self, offset: usize) -> Self {
        self.map_source_err(|err| err.adjust_col(offset))
    }
}
