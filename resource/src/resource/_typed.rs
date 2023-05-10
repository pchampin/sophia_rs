//! I define [`ResourceError`]

use super::{Resource, ResourceError};
use sophia_api::prelude::*;

/// This trait represent any type that "derives" from `Resource`.
///
/// It is automatically implemented by any type that
/// * implements [`TryFrom`]`<`[`Resource`]`>`,
/// * such that `TryFrom::Error` implements `From<`[`ResourceError`]`>`
pub trait TypedResource<G: Graph, L>: TryFrom<Resource<G, L>, Error = Self::TRError> {
    /// The error type raised when a resource can not be converted to this type.
    type TRError: From<ResourceError<G::Error>>;
}

impl<T, G, L> TypedResource<G, L> for T
where
    T: TryFrom<Resource<G, L>>,
    T::Error: From<ResourceError<G::Error>>,
    G: Graph,
{
    type TRError = T::Error;
}
