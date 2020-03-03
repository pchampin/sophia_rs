use thiserror::Error;

/// Type alias for `Result` with default error `TermError`.
///
/// Can be used like `std::result::Result` as well.
pub type Result<T, E = TermError> = std::result::Result<T, E>;

/// This error is raised when the creation of a term fails.
#[derive(Debug, Error)]
pub enum TermError {
    /// Datatypes must be IRIs.
    #[error("The supplied datatype '{0}' was not an IRI")]
    InvalidDatatype(String),
    /// The IRI of a term must apply to [RFC 3987](https://tools.ietf.org/html/rfc3987).
    #[error("The given IRI '{0}' is not valid according to RFC3987")]
    InvalidIri(String),
    /// An IRI must be represented by one `TermData` to be able to parse its components.
    #[error("IRI components could not be parsed in one as it has a suffix")]
    IriParse,
    /// The language tags of literals must apply to [BCP47](https://tools.ietf.org/html/bcp47).
    #[error("The given language tag '{tag}' is not valid according to BCP47: {err}")]
    InvalidLanguageTag { tag: String, err: String },
    /// Names of variables must apply to SPARQL's [production rules](https://www.w3.org/TR/sparql11-query/#rVARNAME).
    #[error("The name '{0}' is not valid for a variable according to the SPARQL specification")]
    InvalidVariableName(String),
    /// Names of variables must apply to N3's [production rules](https://www.w3.org/TR/turtle/#grammar-production-BlankNode).
    #[error(
        "The identifier '{0}' is not valid for a blank node according to the N3 specification"
    )]
    InvalidBlankNodeId(String),
    /// Raised when failing to downcast a term.
    #[error("The term '{term}' is not the expected {expect}")]
    UnexpectedKindOfTerm { term: String, expect: String },
}
