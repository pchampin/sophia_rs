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

    /// https://www.w3.org/TR/json-ld11-api/#dom-jsonlderrorcode-conflicting-indexes
    #[error("conflicting index")]
    ConflictingIndex,

    /// Can not deserialize
    #[error("can not deserialize: {0:?}")]
    CanNotDeserialize(String),

    /// JsonLdSource is already exhausted
    #[error("exhausted source")]
    ExhaustedSource,
}
