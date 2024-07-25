use super::util::IriBuf;
use std::io;

/// An error raised by a [`Loader`](crate::Loader).
#[derive(Debug, thiserror::Error)]
pub enum LoaderError {
    /// The requested IRI is not supported by this loader
    #[error("Unsupported IRI {0:?}: {1}")]
    UnsupportedIri(IriBuf, String),
    /// The requested IRI could not be found (e.g. 404)
    #[error("IRI could not be found {0:?}")]
    NotFound(IriBuf),
    /// An IO error was encountered while loading the content
    #[error("IO error when reading {0:?}: {1}")]
    IoError(IriBuf, io::Error),
    /// Can not guess the syntax of the resource
    /// (some loaders, such as [`LocalLoader`](crate::LocalLoader),
    /// do not always have access to content-type metadata)
    #[error("Can not guess syntax of {0:?}")]
    CantGuessSyntax(IriBuf),
    /// An error was encountered while parsing the data into an RDF graph
    #[error("Can not parse {0:?}: {1}")]
    ParseError(IriBuf, Box<dyn sophia_api::Error>),
}

impl LoaderError {
    /// Return the IRI that caused this error
    pub fn iri(&self) -> IriBuf {
        let iri = match self {
            LoaderError::UnsupportedIri(iri, _) => iri,
            LoaderError::NotFound(iri) => iri,
            LoaderError::IoError(iri, _) => iri,
            LoaderError::CantGuessSyntax(iri) => iri,
            LoaderError::ParseError(iri, _) => iri,
        };
        iri.clone()
    }
}
