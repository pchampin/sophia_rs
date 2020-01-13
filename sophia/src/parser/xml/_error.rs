use crate::parser::{Location, Position, WithLocation};
use quick_xml::Reader;
use std::io::BufRead;

pub type Result<T, E = XmlParserError> = std::result::Result<T, E>;

/// This error is raised at parsing RDF/XML documents.
#[derive(Debug, thiserror::Error)]
pub enum XmlParserError {
    /// Errors raised by malformed XML.
    #[error("Invalid XML: {source} at {location}")]
    Xml {
        source: quick_xml::Error,
        location: Location,
    },
    /// Errors raised by violating the RDF/XML specifications.
    #[error("Invalid RDF/XML: {source} at {location}")]
    InterpretRdf {
        source: RdfError,
        location: Location,
    },
    /// Errors happening not at parsing, e.g. at setup.
    #[error("Error at parser setup: {0}")]
    Operational(#[from] RdfError),
}

impl WithLocation for XmlParserError {
    fn location(&self) -> Location {
        use crate::parser::xml::XmlParserError::*;

        match self {
            Xml { location, .. } | InterpretRdf { location, .. } => location.clone(),
            _ => Location::Unknown,
        }
    }
}

/// Utility extension for errors.
pub trait ResultExt<T> {
    fn locate_err(self, r: &Reader<impl BufRead>) -> Result<T, XmlParserError>;
}

impl<T, E> ResultExt<T> for Result<T, E>
where
    E: IntoLocatedParserError,
{
    /// If self is `Err` then convert the error and apply the buffer position
    /// as the error's location.
    fn locate_err(self, r: &Reader<impl BufRead>) -> Result<T, XmlParserError> {
        self.map_err(|e| e.at_location(r))
    }
}

/// Turn an error into a located parser error.
pub trait IntoLocatedParserError {
    fn at_location(self, r: &Reader<impl BufRead>) -> XmlParserError;
}

impl IntoLocatedParserError for quick_xml::Error {
    fn at_location(self, r: &Reader<impl BufRead>) -> XmlParserError {
        let location = Location::Pos(Position::Offset(r.buffer_position()));
        XmlParserError::Xml {
            source: self,
            location,
        }
    }
}

/// Errors that violate the
/// [RDF/XML specification](https://www.w3.org/TR/rdf-syntax-grammar/).
///
/// _Note:_ Errors of malformed XML are not handled by this type.
#[derive(Debug, thiserror::Error)]
pub enum RdfError {
    #[error("ID {0} occured at least twice")]
    DuplicateId(String),
    #[error("The namespace {0} is unknown")]
    UnknownNamespace(String),
    #[error("The name {0} is invalid for nodes in RDF/XML")]
    InvalidNodeName(String),
    #[error("The name {0} is invalid for properties in RDF/XML")]
    InvalidPropertyName(String),
    #[error("The name {0} is invalid for attributes in RDF/XML")]
    InvalidAttribute(String),
    #[error("{0} is not a valid XML name")]
    InvalidXmlName(String),
    #[error("The `parseType={0}` is not a valid in this context")]
    InvalidParseType(String),
    #[error("Can only have one if `rdf:ID`, `rdf:nodeID` and `rdf:about` at the same time")]
    AmbiguousSubject,
    #[error("Unexpected XML event: expected {expected}, found {found} instead")]
    UnexpectedEvent { expected: String, found: String },
    #[error("Document does not define a base IRI. Required to expand `{0}`")]
    NoBaseIri(String),
    #[error("The given base IRI `{0}` is not a valid IRI")]
    InvalidBaseIri(String),
    #[error("Prefixes must not be `_`")]
    InvalidPrefixBlank,
    #[error("Invalid RDF-term: {0}")]
    InvalidRdfTerm(#[from] crate::term::TermError),
    #[error("Invalid Url: {0}")]
    InvalidUrl(#[from] url::ParseError),
}

impl IntoLocatedParserError for RdfError {
    fn at_location(self, r: &Reader<impl BufRead>) -> XmlParserError {
        let location = Location::Pos(Position::Offset(r.buffer_position()));
        XmlParserError::InterpretRdf {
            source: self,
            location,
        }
    }
}
