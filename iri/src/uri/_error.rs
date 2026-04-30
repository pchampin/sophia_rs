//! Error and result type for URI resolution.

use thiserror::Error;

/// Type alias for `Result` with default error `InvalidUri`.
///
/// Can be used like `std::result::Result` as well.
pub type Result<T, E = InvalidUri> = std::result::Result<T, E>;

/// This error is raised when trying to parse an invalid URI.
#[derive(Debug, Error)]
#[error("The given URI '{0}' is not valid according to RFC3986")]
pub struct InvalidUri(pub String);
