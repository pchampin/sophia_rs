use sophia_api::term::TermKind;
use sophia_api::{prelude::*, term::SimpleTerm, Error};
use std::fmt;

/// An error raised when creating a [`Resource`](crate::Resource)
#[derive(Debug)]
pub enum ResourceError<E: Error> {
    /// The IRI is not absolute (an can therefore not be dereferenced)
    IriNotAbsolute(IriRef<Box<str>>),
    /// The resource could not be loaded
    LoaderError(crate::loader::LoaderError),
    /// The underlying graph raised an error
    GraphError {
        /// The identifier of the resource
        id: SimpleTerm<'static>,
        /// The error that was raised
        error: E,
    },
    /// A value was expected and not found for the given predicate on the given resource
    NoValueFor {
        /// The identifier of the resource
        id: SimpleTerm<'static>,
        /// The predicate
        predicate: SimpleTerm<'static>,
    },
    /// Multiple values were not expected and were found for the given predicate
    UnexpectedMultipleValueFor {
        /// The identifier of the resource
        id: SimpleTerm<'static>,
        /// The predicate
        predicate: SimpleTerm<'static>,
    },

    // Variants below are not used in this crate,
    // but are common cases of errors in [`TypedResource`] implementations.
    /// The node was expected to have given type
    MissingType {
        /// The identifier of the resource
        id: SimpleTerm<'static>,
        /// The expected type
        typ: SimpleTerm<'static>,
    },
    /// The term was expected to have a given kind
    UnexpectedKind {
        /// The identifier of the resource
        id: SimpleTerm<'static>,
        /// The predicate
        predicate: SimpleTerm<'static>,
        /// The term kind that was found
        found_kind: TermKind,
    },
    /// The literal was expected to have a given datatype
    UnexpectedDatatype {
        /// The identifier of the resource
        id: SimpleTerm<'static>,
        /// The predicate
        predicate: SimpleTerm<'static>,
        /// The datatype that was found
        found_datatype: SimpleTerm<'static>,
    },
    /// The literal was expected to have a different value
    UnexpectedValue {
        /// The identifier of the resource
        id: SimpleTerm<'static>,
        /// The predicate
        predicate: SimpleTerm<'static>,
        /// The datatype that was found
        found_value: SimpleTerm<'static>,
    },
}

impl<E: Error> ResourceError<E> {
    /// The identifier of the resource raising the error.
    ///
    /// NB: for errors raised during creation ([`ResourceError::IriNotAbsolute`], [`ResourceError::LoaderError`]),
    /// the identifier of the to-be-created resource is returned
    /// (*not* the resource from which it was discovered).
    pub fn resource_id(&self) -> SimpleTerm {
        match self {
            ResourceError::IriNotAbsolute(iriref) => iriref.as_simple(),
            ResourceError::LoaderError(err) => err.iri().into_term(),
            ResourceError::GraphError { id, .. } => id.as_simple(),
            ResourceError::NoValueFor { id, .. } => id.as_simple(),
            ResourceError::UnexpectedMultipleValueFor { id, .. } => id.as_simple(),
            ResourceError::MissingType { id, .. } => id.as_simple(),
            ResourceError::UnexpectedKind { id, .. } => id.as_simple(),
            ResourceError::UnexpectedDatatype { id, .. } => id.as_simple(),
            ResourceError::UnexpectedValue { id, .. } => id.as_simple(),
        }
    }
}

impl<E: Error> fmt::Display for ResourceError<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<E: Error> From<crate::loader::LoaderError> for ResourceError<E> {
    fn from(value: crate::loader::LoaderError) -> Self {
        Self::LoaderError(value)
    }
}

impl<E> std::error::Error for ResourceError<E> where E: sophia_api::Error {}

/// A result whose error is a [`ResourceError`]
pub type ResourceResult<T, G> = Result<T, ResourceError<<G as Graph>::Error>>;
