use sophia_api::term::TermKind;
use sophia_api::{prelude::*, term::SimpleTerm};
use std::error::Error;
use std::fmt;
use std::ops::Deref;

/// An error raised when creating a [`Resource`](crate::Resource)
#[derive(Debug)]
pub struct ResourceError<E: Error + Send + Sync + 'static>(Box<ResourceErrorKind<E>>);

/// An error raised when creating a [`Resource`](crate::Resource)
#[derive(Debug)]
pub enum ResourceErrorKind<E: Error + Send + Sync + 'static> {
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

impl<E> ResourceError<E>
where
    E: Error + Send + Sync + 'static,
{
    /// The identifier of the resource raising the error.
    ///
    /// NB: for errors raised during creation
    /// ([`ResourceErrorKind::IriNotAbsolute`], [`ResourceErrorKind::LoaderError`]),
    /// the identifier of the to-be-created resource is returned
    /// (*not* the resource from which it was discovered).
    pub fn resource_id(&self) -> SimpleTerm<'_> {
        use ResourceErrorKind::*;
        match &*self.0 {
            IriNotAbsolute(iriref) => iriref.as_simple(),
            LoaderError(err) => err.iri().into_term(),
            GraphError { id, .. } => id.as_simple(),
            NoValueFor { id, .. } => id.as_simple(),
            UnexpectedMultipleValueFor { id, .. } => id.as_simple(),
            MissingType { id, .. } => id.as_simple(),
            UnexpectedKind { id, .. } => id.as_simple(),
            UnexpectedDatatype { id, .. } => id.as_simple(),
            UnexpectedValue { id, .. } => id.as_simple(),
        }
    }
}

impl<E> Deref for ResourceError<E>
where
    E: Error + Send + Sync + 'static,
{
    type Target = ResourceErrorKind<E>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<E> fmt::Display for ResourceError<E>
where
    E: Error + Send + Sync + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl<E: Error> From<crate::loader::LoaderError> for ResourceError<E>
where
    E: Error + Send + Sync + 'static,
{
    fn from(value: crate::loader::LoaderError) -> Self {
        ResourceErrorKind::LoaderError(value).into()
    }
}

impl<E: Error> From<ResourceErrorKind<E>> for ResourceError<E>
where
    E: Error + Send + Sync + 'static,
{
    fn from(value: ResourceErrorKind<E>) -> Self {
        Self(Box::new(value))
    }
}

impl<E> Error for ResourceError<E> where E: Error + Send + Sync + 'static {}

/// A result whose error is a [`ResourceError`]
pub type ResourceResult<T, G> = Result<T, ResourceError<<G as Graph>::Error>>;
