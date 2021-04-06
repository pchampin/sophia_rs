//! JSON-LD errors.

use crate::config::JsonLdSpecVersion;

/// JSON-LD error
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum JsonLdError {
    /// An invalid JSON literal.
    #[error("invalid JSON literal: {0}")]
    InvalidJsonLiteral(#[from] json::Error),

    /// An IO error.
    #[error("IO Error: {0}")]
    IoError(#[from] std::io::Error),

    /// An unsupported JSON-LD version
    #[error("unsupported version: {0:?}")]
    UnsupportedVersion(JsonLdSpecVersion),
}
