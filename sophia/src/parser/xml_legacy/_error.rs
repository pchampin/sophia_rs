use quick_xml::Reader;
use sophia_api::parser::{Location, Position, WithLocation};
use std::error::Error;
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
    /// Errors happening at setup of the parser.
    #[error("Error at parser setup: {0}")]
    Setup(#[from] RdfError),
}

impl WithLocation for XmlParserError {
    fn location(&self) -> Location {
        use super::XmlParserError::*;

        match self {
            Xml { location, .. } | InterpretRdf { location, .. } => location.clone(),
            Setup(_) => Location::Unknown,
        }
    }
}

/// An error which can be enriched with location information.
pub trait LocatableError<L>: Error {
    type WithLocation: WithLocation + Error;

    /// Add location information to this error.
    fn locate_with(self, ls: L) -> Self::WithLocation;
}

impl<'a, BR> LocatableError<&'a Reader<BR>> for quick_xml::Error
where
    BR: BufRead,
{
    type WithLocation = XmlParserError;

    fn locate_with(self, ls: &'a Reader<BR>) -> Self::WithLocation {
        let location = Location::Pos(Position::Offset(ls.buffer_position()));
        XmlParserError::Xml {
            source: self,
            location,
        }
    }
}

/// An extension for `Result`s embedding a [`LocatableError`](trait.LocatableError.html).
pub trait LocatableResult<T, E, L>
where
    E: LocatableError<L>,
{
    /// Add location information to the embeded error, if any.
    fn locate_err_with(self, ls: L) -> Result<T, E::WithLocation>;
}

impl<T, E, L> LocatableResult<T, E, L> for Result<T, E>
where
    E: LocatableError<L>,
{
    fn locate_err_with(self, ls: L) -> Result<T, E::WithLocation> {
        self.map_err(|e| e.locate_with(ls))
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
    #[error("Can only have one of `rdf:ID`, `rdf:nodeID` and `rdf:about` at the same time")]
    AmbiguousSubject,
    #[error("Unexpected XML event: expected {expected}, found {found} instead")]
    UnexpectedEvent { expected: String, found: String },
    #[error("Document does not define a base IRI. Required to expand `{0}`")]
    NoBaseIri(String),
    #[error("Prefixes must not be `_`")]
    InvalidPrefixBlank,
    #[error("Invalid RDF-term: {0}")]
    InvalidRdfTerm(#[from] sophia_term::TermError),
    #[error("Invalid Url: {0}")]
    InvalidUrl(#[from] url::ParseError),
    #[error("The given base IRI `{0}` is not a valid IRI")]
    InvalidBaseIri(String),
    #[error("Invalid IRI: {0}")]
    InvalidIri(#[from] sophia_iri::error::InvalidIri),
}

impl<'a, BR> LocatableError<&'a Reader<BR>> for RdfError
where
    BR: BufRead,
{
    type WithLocation = XmlParserError;

    fn locate_with(self, ls: &'a Reader<BR>) -> Self::WithLocation {
        let location = Location::Pos(Position::Offset(ls.buffer_position()));
        XmlParserError::InterpretRdf {
            source: self,
            location,
        }
    }
}
