//! A provide a forward-chaining reasoning engine for Simple, RDF and RDFS entailment.
//!
//! See [`ReasonableGraph`].
#![deny(missing_docs)]

use std::{
    collections::{BTreeSet, HashMap},
    marker::PhantomData,
    sync::Arc,
};

use sophia_api::term::{BaseDirection, BnodeId, IriRef, LanguageTag, VarName};

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
    i2t: Vec<Arc<InternalTerm>>,
    t2i: HashMap<Arc<InternalTerm>, usize>,
    spo: BTreeSet<[usize; 3]>,
    pos: BTreeSet<[usize; 3]>,
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

#[cfg(test)]
mod test;
