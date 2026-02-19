use super::*;

/// A [`GraphName`] is an optional [`Term`](super::Term),
/// used to name a graph in an [RDF dataset](https://www.w3.org/TR/rdf12-concepts/#section-dataset).
///
/// By convention, `None` is the "name" of the default graph.
pub type GraphName<T> = Option<T>;

/// Determines if two [`GraphName`]s represent the same RDF term.
pub fn graph_name_eq<T1: Term, T2: Term>(gn1: GraphName<T1>, gn2: GraphName<T2>) -> bool {
    match (gn1, gn2) {
        (Some(t1), Some(t2)) => t1.eq(t2),
        (None, None) => true,
        _ => false,
    }
}
