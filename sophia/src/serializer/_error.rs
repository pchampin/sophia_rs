use thiserror::Error;
use std::io;
use crate::error::Infallible;

pub type SerializationResult<T> = Result<T, SerializationError>;

#[derive(Debug, Error)]
pub enum SerializationError {
	#[error("Custom: {msg}")]
	Custom { msg: String },
	#[error("IO: {source}")]
	Io { #[from] source: io::Error },
}

impl From<String> for SerializationError {
	fn from(msg: String) -> Self {
		Self::Custom { msg }
	}
}

impl From<Infallible> for SerializationError {
	/// This implementation is only for support of the type system.
	/// As `Infallible` can not be instantiatet this will never execute.
	fn from(_: Infallible) -> Self {
		unreachable!()
	}
}