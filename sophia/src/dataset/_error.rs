use thiserror::Error;
use crate::error::Infallible;

pub type DatasetResult<T> = Result<T, DatasetError>;

#[derive(Debug, Error)]
pub enum DatasetError {
	#[error("Custom: {msg}")]
	Custom { msg: String },
	#[error("MutationFailed: {msg}")]
	CustomMuatation { msg: String },
	#[error("UnsopportedGraphName: {name}")]
	UnsupportedGraphName{ name: String },
	#[error("FromInnerGraph: {source}")]
	FromGraph { #[from] source: crate::graph::GraphError },
}

impl From<String> for DatasetError {
	fn from(msg: String) -> Self {
		Self::Custom { msg }
	}
}

impl From<Infallible> for DatasetError {
	/// This implementation is only for support of the type system.
	/// As `Infallible` can not be instantiatet this will never execute.
	fn from(_: Infallible) -> Self {
		unreachable!()
	}
}