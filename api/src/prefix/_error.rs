use thiserror::Error;

/// This error is raised when trying to parse an invalid IRI.
#[derive(Debug, Error)]
#[error("The given prefix '{0}' does not match PN_PREFIX?")]
pub struct InvalidPrefix(pub String);
