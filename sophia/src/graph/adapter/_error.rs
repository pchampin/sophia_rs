use std::error::Error;

/// This error is raised by the [adapter] from [`MutableGraph`] to [`MutableDataset`].
///
/// _Note:_ MGE is the [mutation error] of the wrapped [`MutableGraph`].
///
/// [adapter]: ./struct.GraphAsDataset.html
/// [`MutableGraph`]: ../trait.MutableGraph.html
/// [`MutableDataset`]: ../../dataset/trait.MutableDataset.html
/// [mutation error]: ../trait.MutableGraph.html#associatedtype.MutationError
#[derive(Debug, thiserror::Error)]
pub enum GraphAsDatasetError<MGE: 'static + Error> {
    /// Raised when the [adapter] is requested to modify a triple in a named
    /// `Graph` which is not supported as it is only a singel `Graph` wrapped.
    ///
    /// [adapter]: ./struct.GraphAsDataset.html
    ///
    #[error("GraphAsDataset does not support named graphs")]
    GraphNamesNotSupported,
    /// Error from the wrapped `Graph`
    #[error("{0}")]
    FromGraph(#[from] MGE),
}
