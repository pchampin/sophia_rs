//! I define [`ResourceError`]

use super::{Resource, ResourceError};
use sophia_api::prelude::*;

/// This trait represent any type that "derives" from `Resource`.
///
/// It is automatically implemented by any type that
/// * implements [`TryFrom`]`<`[`Resource`]`>`
/// * with its `TryFrom::Error` implementing `From<`[`ResourceError`]`>`
#[diagnostic::on_unimplemented(
    note = "`TypedResource<G, L>` is automatically implemented by any type that:",
    note = "- implements `TryFrom<Resource<G, L>>`",
    note = "- with its `TryFrom::Error` implementing `From<ResourceError<G::Error>>`"
)]
pub trait TypedResource<G: Graph, L>: TryFrom<Resource<G, L>, Error = Self::TRError> {
    /// The error type raised when a resource can not be converted to this type.
    type TRError: From<ResourceError<G::Error>>;
}

#[diagnostic::do_not_recommend]
impl<T, G, L> TypedResource<G, L> for T
where
    T: TryFrom<Resource<G, L>>,
    T::Error: From<ResourceError<G::Error>>,
    G: Graph,
{
    type TRError = T::Error;
}
