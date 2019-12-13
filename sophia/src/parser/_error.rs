use thiserror::Error;
use pest::error::{Error as PestError, ErrorVariant, InputLocation, LineColLocation};
use std::io;
use crate::term::TermError;

pub type ParserResult<T> = Result<T, ParserError>;

#[derive(Debug, Error)]
pub enum ParserError {
	#[error("Custom: {msg}")]
	Custom { msg: String },
	#[error("ParserFailed: at {}: {msg}", display_location(.location, .line_col))]
	Located { msg: String, location: InputLocation, line_col: LineColLocation},
	#[error("IoFailed: {source}")]
	Io { #[from] source: io::Error },
	#[error("IoFailed at line {line}: {source}")]
	IoAt { source: io::Error, line: usize },
	#[error("InvalidTerm: {source}")]
	InvalidTerm { #[from] source: TermError },
}

impl From<String> for ParserError {
	fn from(msg: String) -> Self {
		Self::Custom { msg }
	}
}

impl ParserError {
	pub fn new_located(msg: String, line_offset: usize) -> Self {
		let location = InputLocation::Pos(0);
		let line_col = LineColLocation::Pos((line_offset, 0));
		ParserError::Located { msg, location, line_col }
	}
	/// Utility function for converting [Pest] errors into [Sophia errors](../../error/index.html).
	///
	/// [Pest]: https://docs.rs/crate/pest/
	///
	pub fn from_pest_err<R: pest::RuleType>(err: PestError<R>, lineoffset: usize) -> Self {
		let msg = match err.variant {
			ErrorVariant::ParsingError {
				positives,
				negatives,
			} => format!("expected: {:?}\nunexpected: {:?}", positives, negatives),
			ErrorVariant::CustomError { message } => message,
		};
		let location = err.location.clone();
		use ::pest::error::LineColLocation::*;
		let line_col = match err.line_col.clone() {
			Pos((l, c)) => Pos((l + lineoffset, c)),
			Span((l1, c1), (l2, c2)) => Span((l1 + lineoffset, c1), (l2 + lineoffset, c2)),
		};
		ParserError::Located { msg, location, line_col }
	}
	pub fn from_io_at(source: io::Error, line: usize) -> Self {
		Self::IoAt {source, line}
	}
}

fn display_location(il: &InputLocation, lcl: &LineColLocation) -> String {
	let line = *match lcl {
		LineColLocation::Pos((line, _)) => line,
		LineColLocation::Span((line, _), _) => line,
	};
	if line == 0 {
		match il {
			InputLocation::Pos(pos) => format!("{}", pos),
			InputLocation::Span((s, e)) => format!("{}-{}", s, e),
		}
	} else {
		match lcl {
			LineColLocation::Pos((l, c)) => format!("{}:{}", l, c),
			LineColLocation::Span((l1, c1), (l2, c2)) => format!("{}:{}-{}:{}", l1, c1, l2, c2),
		}
	}
}
