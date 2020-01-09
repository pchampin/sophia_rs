use thiserror::Error;

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
    /// The language tags of literals must apply to [BCP47](https://tools.ietf.org/html/bcp47).
    #[error("The given language tag '{tag}' is not valid according to BCP47: {err}")]
    InvalidLanguageTag { tag: String, err: String },
    /// Names of variables must apply to SPARQL's [production rules](https://www.w3.org/TR/sparql11-query/#rVARNAME).
    #[error("The name '{0}' is not valid for a variable according to the SPARQL specification")]
    InvalidVariableName(String),
}
