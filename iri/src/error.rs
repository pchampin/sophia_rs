//! Error and result type for IRI resolution.

use thiserror::Error;

/// Type alias for `Result` with default error `TermError`.
///
/// Can be used like `std::result::Result` as well.
pub type Result<T, E = InvalidIri> = std::result::Result<T, E>;

/// This error is raised when trying to parse an invalid IRI.
#[derive(Debug, Error)]
#[error("The given IRI '{0}' is not valid according to RFC3987")]
pub struct InvalidIri(pub String);
