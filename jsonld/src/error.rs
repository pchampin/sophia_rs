use crate::config::JsonLdSpecVersion;

/// JSON-LD error
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum JsonLdError {
    #[error("invalid JSON literal: {0}")]
    InvalidJsonLiteral(#[from] json::Error),
    #[error("IO Error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("unsupported version: {0:?}")]
    UnsupportedVersion(JsonLdSpecVersion),
}
