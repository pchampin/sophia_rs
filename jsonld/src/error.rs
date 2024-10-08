//! JSON-LD errors.

use crate::options::ProcessingMode;
use crate::vocabulary::ArcIri;
use json_ld::ExpandError;
use json_syntax::parse::Error;
use locspan::{Location, Meta, Span};
use std::fmt::Display;
use std::sync::PoisonError;

/// JSON-LD error
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum JsonLdError {
    /// Invalid JSON encountered while parsing.
    #[error("invalid JSON at {location:?}: {error}")]
    InvalidJson {
        /// The JSON parsing error
        error: Error<Location<ArcIri, Span>>,
        /// The location where the error occurred
        location: Location<ArcIri, Span>,
    },

    /// An invalid JSON literal was encountered when serializing.
    #[error("invalid JSON literal at {location:?}: {error}")]
    InvalidJsonLiteral {
        /// The JSON parsing error
        error: Error<Span>,
        /// The location where the error occurred
        location: Span,
    },

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

impl From<Meta<Error<Location<ArcIri, Span>>, Location<ArcIri, Span>>> for JsonLdError {
    fn from(other: Meta<Error<Location<ArcIri, Span>>, Location<ArcIri, Span>>) -> Self {
        let Meta(error, location) = other;
        Self::InvalidJson { error, location }
    }
}

impl From<Meta<Error<Span>, Span>> for JsonLdError {
    fn from(other: Meta<Error<Span>, Span>) -> Self {
        let Meta(error, location) = other;
        Self::InvalidJsonLiteral { error, location }
    }
}

impl<T> From<PoisonError<T>> for JsonLdError {
    fn from(value: PoisonError<T>) -> Self {
        Self::PoisonnedLock(format!("{value}"))
    }
}

impl<M, E, C> From<ExpandError<M, E, C>> for JsonLdError
where
    ExpandError<M, E, C>: Display,
{
    fn from(value: ExpandError<M, E, C>) -> Self {
        Self::ExpandError(format!("{value}"))
    }
}
