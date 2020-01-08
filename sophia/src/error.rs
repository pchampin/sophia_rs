//! Types for handling errors.
use crate::term::TermError;
use std::convert::Infallible;
use std::fmt;

error_chain! {
    errors {
        /// Raised by the methods of the [`Graph`](../graph/trait.Graph.html) trait.
        GraphError(message: String) {
            display("error while querying Graph: {}", message)
        }
        /// Raised by the methods of the [`MutableGraph`](../graph/trait.MutableGraph.html) trait.
        GraphMutationError(msg: String) {
            display("error while modifying Graph: {}", msg)
        }
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
        UnsupportedGraphName(graph_name: String) {
            display("unsupported graph_name: {}", graph_name)
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

/// Required until `NtPatserError` is introduced.
impl From<TermError> for Error {
    fn from(te: TermError) -> Self {
        ErrorKind::TermError(te).into()
    }
}

/// A position in a parsed stream.
#[derive(Clone, Debug)]
pub enum Position {
    // Byte offset (starting at 0)
    Offset(usize),
    // Line-Column position (both starting at 1)
    LiCo(usize, usize),
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Position::Offset(offset) => write!(f, "{}", offset),
            Position::LiCo(li, co) => write!(f, "{}:{}", li, co),
        }
    }
}

/// The location of a ParseError
#[derive(Clone, Debug)]
pub enum Location {
    Unknown,
    Pos(Position),
    Span(Position, Position),
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Unknown => write!(f, "?"),
            Location::Pos(pos) => write!(f, "{}", pos),
            Location::Span(s, e) => write!(f, "{}-{}", s, e),
        }
    }
}

impl Location {
    pub fn from_offset(offset: usize) -> Location {
        Location::Pos(Position::Offset(offset))
    }
    pub fn from_lico(line: usize, column: usize) -> Location {
        Location::Pos(Position::LiCo(line, column))
    }
    pub fn from_offsets(offset1: usize, offset2: usize) -> Location {
        Location::Span(Position::Offset(offset1), Position::Offset(offset2))
    }
    pub fn from_licos(line1: usize, column1: usize, line2: usize, column2: usize) -> Location {
        Location::Span(
            Position::LiCo(line1, column1),
            Position::LiCo(line2, column2),
        )
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
