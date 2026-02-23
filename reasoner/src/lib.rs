//! A provide a forward-chaining reasoning engine for Simple, RDF and RDFS entailment.
//!
//! See [`ReasonableGraph`].
#![deny(missing_docs)]

use std::{collections::BTreeSet, marker::PhantomData, ops::Range};

use sophia_inmem::index::BasicTermIndex;

use crate::d_entailment::IllTypedLiteral;

pub mod d_entailment;
pub mod dataset;
pub mod ruleset;

mod _dedup;
mod _graph_impl;
mod _range_n;

/// An implementation of [`Graph`](sophia_api::graph::Graph)
/// supporting an entailment regime captured by
/// * a set `D` of recognized datatypes, and
/// * a ruleset `R` representing the level of semantics (Simple, RDF, RDFS).
///
/// The entailment regime is reflected in
/// * method [`Graph::triples_matching`](sophia_api::graph::Graph::triples_matching),
///   which will match any asserted *or inferred* triple,
/// * [`entails`](Self::entails) and [`entails_triples`](Self::entails_triples).
pub struct ReasonableGraph<D, R> {
    /// term index
    terms: BasicTermIndex<usize>,
    /// recognized datatypes, as (min-index, max-index)
    rdt: Range<usize>,
    /// asserted triples, sorted by subject-predicate-object
    spo: BTreeSet<[usize; 3]>,
    /// asserted triples, sorted by predicate-object-subject
    pos: BTreeSet<[usize; 3]>,
    /// asserted triples, sorted by object-subject-predicate
    osp: BTreeSet<[usize; 3]>,
    _phantom: PhantomData<(D, R)>,
    #[cfg(debug_assertions)]
    finalized: bool,
}

/// The error that can occur while collecting a [`ReasonableGraph`]
#[derive(Clone, Debug, thiserror::Error)]
pub enum Inconsistency {
    /// An ill-typed literal was encountered
    #[error("Ill-typed literal")]
    IllTypedLiteral(
        #[from]
        #[source]
        IllTypedLiteral,
    ),
    /// An inconsistency was derived
    #[error("Other inconsistency: {0}")]
    Other(String),
}

#[cfg(test)]
mod test;
