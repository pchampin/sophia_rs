use sophia_api::graph::Graph;
use sophia_api::source::StreamResult;
use sophia_c14n::C14nError;

/// Computes whether two graphs are isomorphic.
///
/// # Error
/// If an error occurs while traversing `g1`,
/// a [`SourceError`](sophia_api::source::StreamError::SourceError`) is returned.
///
/// /// If an error occurs while traversing `g2`,
/// a [`SinkError`](sophia_api::source::StreamError::SinkError`) is returned.
pub fn isomorphic_graphs<G1, G2>(
    g1: &G1,
    g2: &G2,
) -> StreamResult<bool, C14nError<G1::Error>, C14nError<G2::Error>>
where
    G1: Graph,
    G2: Graph,
{
    super::isomorphic_datasets(&g1.as_dataset(), &g2.as_dataset())
}
