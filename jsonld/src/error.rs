//! JSON-LD errors.

use crate::options::ProcessingMode;
use json_ld::ExpandError;
use std::sync::PoisonError;

/// JSON-LD error
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum JsonLdError {
    /// Invalid JSON encountered while parsing.
    #[error("invalid JSON: {0}")]
    InvalidJson(#[from] json_syntax::parse::Error),

    /// An invalid JSON literal was encountered when serializing.
    #[error("invalid JSON literal: {0}")]
    InvalidJsonLiteral(json_syntax::parse::Error),

    /// An IO error.
    #[error("IO Error: {0}")]
    IoError(#[from] std::io::Error),

    /// Unsupported JSON-LD version in options
    #[error("unsupported version: {0:?}")]
    UnsupportedVersion(ProcessingMode),

    /// Poisonned lock
    ///
    /// NB: `PoisonError` is generic, so we keep the message only
    #[error("poisonned lock for document loader: {0}")]
    PoisonnedLock(String),

    /// An expansion error was encountered while parsing
    ///
    /// NB: `ExpandError` is generic, so we keep the message only
    #[error("error while expanding: {0}")]
    ExpandError(String),

    /// An UTF-8 error was encountered while parsing from a [`BufRead`](std::io::BufRead)
    #[error("{0}")]
    Utf8(#[from] std::string::FromUtf8Error),
}

impl<T> From<PoisonError<T>> for JsonLdError {
    fn from(value: PoisonError<T>) -> Self {
        Self::PoisonnedLock(format!("{value}"))
    }
}

impl From<ExpandError> for JsonLdError {
    fn from(value: ExpandError) -> Self {
        Self::ExpandError(format!("{value}"))
    }
}
