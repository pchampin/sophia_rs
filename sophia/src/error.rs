//! Types for handling errors.
use crate::parser::Location;
use crate::term::TermError;
use std::convert::Infallible;

error_chain! {
    errors {
        /// Raised whenever an invalid prefix is used in a PName.
        InvalidPrefix(prefix: String) {
            display("invalid prefix <{}>", prefix)
        }
        /// Raised whenever an IRI can not be rendered absolute in a strict RDF graph.
        IriMustBeAbsolute(iri: String) {
            display("IRI must be absolute <{}>", iri)
        }
        /// Raised by parsers when they encounter a problem.
        ParserError(message: String, location: Location) {
            display("parse error at {}: {}", location, message)
        }
        /// Raised by serializers when they encounter a problem.
        SerializerError(message: String) {
            display("error while serializing: {}", message)
        }
        /// Raised by some mutable dataset
        TermError(te: TermError) {
            display("From term: {}", te)
        }
    }
}

/// Only implemented to satisfy the type system.
impl From<Infallible> for Error {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

/// Required by parser::xml
impl From<TermError> for Error {
    fn from(te: TermError) -> Self {
        ErrorKind::TermError(te).into()
    }
}

/// Required by parser::xml
impl crate::parser::WithLocation for Error {
    fn location(&self) -> Location {
        match self {
            Error(ErrorKind::ParserError(_, location), ..) => location.clone(),
            _ => Location::Unknown,
        }
    }
}

/// Make a Parser Error with minimal information
pub fn make_parser_error(message: String, line_offset: usize) -> ErrorKind {
    ErrorKind::ParserError(message, Location::from_lico(line_offset, 0))
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
