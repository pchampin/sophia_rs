//! A provide a forward-chaining reasoning engine for Simple, RDF and RDFS entailment.
//!
//! See [`ReasonableGraph`].
#![deny(missing_docs)]

use std::{
    collections::{BTreeSet, HashMap},
    marker::PhantomData,
    ops::Range,
    sync::Arc,
};

use sophia_api::term::{BaseDirection, BnodeId, IriRef, LanguageTag, VarName};

use crate::d_entailment::IllTypedLiteral;

pub mod d_entailment;
pub mod ruleset;

mod _dedup;
mod _graph_impl;
mod _range_n;
mod _term_impl;

/// An implementation of [`Graph`](sophia_api::graph::Graph)
/// supporting an entailment regime captured by
/// * a set `D` of recognized datatypes, and
/// * a ruleset `R` representing the level of semantics (Simple, RDF, RDFS).
///
/// The entailment regime is reflected in
/// * method [`Graph::triples_matching`](sophia_api::graph::Graph::triples_matching),
///   which will match any asserted *or inferred* triple,
/// * [`entails`](Self::entails) and [`entails_triples`](Self::entails_triples).
#[derive(Clone)]
pub struct ReasonableGraph<D, R> {
    /// index-to-term
    i2t: Vec<Arc<InternalTerm>>,
    /// term-to-index
    t2i: HashMap<Arc<InternalTerm>, usize>,
    /// recognized datatypes, as (min-index, max-index)
    rdt: Range<usize>,
    /// asserted triples, sorted by subject-predicate-object
    spo: BTreeSet<[usize; 3]>,
    /// asserted triples, sorted by predicate-object-subject
    pos: BTreeSet<[usize; 3]>,
    /// asserted triples, sorted by object-subject-predicate
    osp: BTreeSet<[usize; 3]>,
    _phantom: PhantomData<(D, R)>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum InternalTerm {
    Iri(IriRef<Box<str>>),
    BlankNode(BnodeId<Box<str>>),
    TypedLiteral(Box<str>, usize),
    LangString(Box<str>, LanguageTag<Box<str>>, Option<BaseDirection>),
    TripleTerm([usize; 3]),
    Variable(VarName<Box<str>>),
}

/// The type of [`Term`](sophia_api::term::Term) returned by [`ReasonableGraph`].
pub struct ReasonableTerm<'a, D, R> {
    graph: &'a ReasonableGraph<D, R>,
    index: usize,
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
