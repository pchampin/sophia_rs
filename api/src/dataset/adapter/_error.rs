// this module is transparently re-exported by its parent `adapter`
use std::error::Error;

/// This error is raised by the [adapter] from [`MutableGraph`] to [`MutableDataset`].
///
/// _Note:_ MGE is the [mutation error] of the wrapped [`MutableGraph`].
///
/// [adapter]: super::GraphAsDataset
/// [`MutableGraph`]: crate::graph::MutableGraph
/// [`MutableDataset`]: crate::dataset::MutableDataset
/// [mutation error]: crate::graph::MutableGraph::MutationError
#[derive(Debug, thiserror::Error)]
pub enum GraphAsDatasetError<MGE: 'static + Error> {
    /// Raised when the [adapter](super::GraphAsDataset) is requested to modify a triple in a named
    /// `Graph` which is not supported as it is only a single `Graph` wrapped.
    #[error("GraphAsDataset does not support named graphs")]
    GraphNamesNotSupported,
    /// Error from the wrapped `Graph`
    #[error("{0}")]
    FromGraph(#[from] MGE),
}
