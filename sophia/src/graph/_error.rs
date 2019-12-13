use thiserror::Error;
use crate::error::Infallible;

pub type GraphResult<T> = Result<T, GraphError>;

#[derive(Debug, Error)]
pub enum GraphError {
	#[error("Custom: {msg}")]
	Custom { msg: String },
	#[error("MutationFailed: {msg}")]
	CustomMuatation { msg: String },
}

impl From<String> for GraphError {
	fn from(msg: String) -> Self {
		Self::Custom { msg }
	}
}

impl From<Infallible> for GraphError {
	/// This implementation is only for support of the type system.
	/// As `Infallible` can not be instantiatet this will never execute.
	fn from(_: Infallible) -> Self {
		unreachable!()
	}
}