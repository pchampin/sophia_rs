use thiserror::Error as ThisError;

pub type TermResult<T> = Result<T, TermError>;

#[derive(Debug, ThisError)]
pub enum TermError {
	#[error("Custom: {msg}")]
	Custom { msg: String },
	#[error("InvalidDatatype: {dt}")]
	InvalidDatatype{ dt: String },
	#[error("InvalidIri: {iri}")]
	InvalidIri{ iri: String },
	#[error("InvalidLanguageTag: {lang}: {msg}")]
	InvalidLanguageTag{ lang: String, msg: String },
	#[error("InvalidVariableName: {var}")]
	InvalidVariableName{ var: String },
	#[error("InvalidPrefix: {prefix}")]
	InvalidPrefix{ prefix: String },
	#[error("IriMustBeAbsolute: <{iri}>")]
	IriMustBeAbsolute{ iri: String },
}

impl From<String> for TermError {
	fn from(msg: String) -> Self {
		Self::Custom { msg }
	}
}