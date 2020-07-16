// this module is transparently re-exported by its parent `parser`
use std::fmt;

/// A location in a parsed stream, which can be unknown, a specific point, or a span.
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
    /// Build a location which is a byte-offset (starting at 0) in the stream.
    pub fn from_offset(offset: usize) -> Location {
        Location::Pos(Position::Offset(offset))
    }
    /// Build a location which is a line-column position (both starting at 1) in the stream.
    pub fn from_lico(line: usize, column: usize) -> Location {
        Location::Pos(Position::LiCo(line, column))
    }
    /// Build a location which is a span between two byte-offsets (starting at 0) in the stream.
    pub fn from_offsets(offset1: usize, offset2: usize) -> Location {
        Location::Span(Position::Offset(offset1), Position::Offset(offset2))
    }
    /// Build a location which a span between two line-column positions (both starting at 1)
    /// in the stream.
    pub fn from_licos(line1: usize, column1: usize, line2: usize, column2: usize) -> Location {
        Location::Span(
            Position::LiCo(line1, column1),
            Position::LiCo(line2, column2),
        )
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

/// This trait is meant to be implemented by errors raised by parsers.
///
/// See also [`LocatableError`](./trait.LocatableError.html)
/// and [`LocatableResult`](./trait.LocatableResult.html).
pub trait WithLocation {
    fn location(&self) -> Location;
}
