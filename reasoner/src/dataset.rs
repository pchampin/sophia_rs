//! A provide [`ReasonableDataset`]

use std::{
    collections::BTreeMap,
    convert::Infallible,
    iter::{empty, once},
};

use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};
use resiter::{Filter, Map};
use sophia_api::{
    dataset::CollectibleDataset,
    graph::Graph,
    prelude::Dataset,
    quad::{Quad, Spog},
    source::StreamError::SinkError,
    term::{Term, matcher::TermMatcher},
};
use sophia_inmem::index::{BasicTermIndex, IndexedTerm, TermIndex};

use crate::{Inconsistency, ReasonableGraph, d_entailment::Recognized, ruleset::RuleSet};

/// A [`ReasonableDataset`] is a dataset where each graph (default or named)
/// is a [`ReasonableGraph`].
///
/// Note that reasoning is performed independently in each graph;
/// a triple in one graph does not impact the reasoning on other graphs.
/// Note also that all graphs share the same set of [`Recognized`] datatypes,
/// and the same [`RuleSet`].
pub struct ReasonableDataset<D, R> {
    names: BasicTermIndex<usize>,
    graphs: BTreeMap<Option<usize>, ReasonableGraph<D, R>>,
    // used when a graph is required which is not present in this dataset
    //
    // TODO if Dataset::graph is refactored to return a result,
    // this fields should be removed, and graph should return an error rather than en empty graph
    empty: ReasonableGraph<D, R>,
}

impl<D, R> Clone for ReasonableDataset<D, R> {
    fn clone(&self) -> Self {
        Self {
            names: self.names.clone(),
            graphs: self.graphs.clone(),
            empty: self.empty.clone(),
        }
    }
}

impl<D, R> std::fmt::Debug for ReasonableDataset<D, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReasonableDataset")
            .field("names", &self.names)
            .field("graphs", &self.graphs)
            .field("empty", &self.empty)
            .finish()
    }
}

impl<D: Recognized, R: RuleSet> Dataset for ReasonableDataset<D, R> {
    type Quad<'x>
        = Spog<IndexedTerm<'x, usize>>
    where
        Self: 'x;

    type Error = Infallible;

    fn quads(
        &self,
    ) -> impl Iterator<Item = sophia_api::dataset::DResult<Self, Self::Quad<'_>>> + '_ {
        self.graphs.iter().flat_map(|(n, g)| {
            let gn = n.map(|i| self.names.get_term(i));
            g.triples().map_ok(move |spo| (spo, gn))
        })
    }

    fn quads_matching<'s, 't, S, P, O, G>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
        gm: G,
    ) -> impl Iterator<
        Item = Result<
            <Self as sophia_api::dataset::Dataset>::Quad<'s>,
            <Self as sophia_api::dataset::Dataset>::Error,
        >,
    > + 't
    where
        's: 't,
        S: TermMatcher + 't,
        P: TermMatcher + 't,
        O: TermMatcher + 't,
        G: sophia_api::term::matcher::GraphNameMatcher + 't,
    {
        if let Some(gn) = gm.constant() {
            match gn
                .map(|t| self.names.get_index(t.borrow_term()).ok_or(()))
                .transpose()
            {
                Ok(opt) => {
                    let g = self.graphs.get(&opt).unwrap();
                    let gn = opt.map(|i| self.names.get_term(i));
                    Box::new(g.triples_matching(sm, pm, om).map_ok(move |spo| (spo, gn)))
                        as Box<dyn Iterator<Item = _>>
                }
                Err(_) => Box::new(empty()),
            }
        } else {
            let dataset = self;
            let graphs =
                once((None, None)).chain(self.names.iter().filter_map(move |t| {
                    gm.matches(Some(&t)).then_some((Some(t.index()), Some(t)))
                }));
            let current = Some(None);
            let triples = vec![].into_iter();

            Box::new(QuadsMatching {
                dataset,
                graphs,
                current,
                triples,
                sm,
                pm,
                om,
            })
        }
    }

    fn subjects(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.graphs.iter().flat_map(|(_, g)| g.subjects())
    }

    fn subjects_matching<'s, M: TermMatcher + 's>(
        &'s self,
        matcher: M,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'s, Self>>,
    > + 's {
        self.subjects().filter_ok(move |t| matcher.matches(t))
    }

    fn predicates(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.graphs.iter().flat_map(|(_, g)| g.predicates())
    }

    fn predicates_matching<'s, M: TermMatcher + 's>(
        &'s self,
        matcher: M,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'s, Self>>,
    > + 's {
        self.predicates().filter_ok(move |t| matcher.matches(t))
    }

    fn objects(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.graphs.iter().flat_map(|(_, g)| g.objects())
    }

    fn objects_matching<'s, M: TermMatcher + 's>(
        &'s self,
        matcher: M,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'s, Self>>,
    > + 's {
        self.objects().filter_ok(move |t| matcher.matches(t))
    }

    fn graph_names(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.names.iter().map(Ok)
    }

    fn graph_names_matching<'s, M: TermMatcher + 's>(
        &'s self,
        matcher: M,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'s, Self>>,
    > + 's {
        self.graph_names().filter_ok(move |gn| matcher.matches(gn))
    }

    fn iris(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.graphs
            .iter()
            .flat_map(|(_, g)| g.iris())
            .chain(self.names.iris().map(Ok))
    }

    fn blank_nodes(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.graphs
            .iter()
            .flat_map(|(_, g)| g.blank_nodes())
            .chain(self.names.blank_nodes().map(Ok))
    }

    fn literals(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.graphs
            .iter()
            .flat_map(|(_, g)| g.literals())
            .chain(self.names.literals().map(Ok))
    }

    fn triple_terms<'s>(
        &'s self,
    ) -> Box<
        dyn Iterator<
                Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'s, Self>>,
            > + 's,
    >
    where
        sophia_api::dataset::DTerm<'s, Self>: Clone,
    {
        Box::new(
            self.graphs
                .iter()
                .flat_map(|(_, g)| g.triple_terms())
                .chain(self.names.triple_terms().map(Ok)),
        )
    }

    fn variables(
        &self,
    ) -> impl Iterator<
        Item = sophia_api::dataset::DResult<Self, sophia_api::dataset::DTerm<'_, Self>>,
    > + '_ {
        self.graphs
            .iter()
            .flat_map(|(_, g)| g.variables())
            .chain(self.names.variables().map(Ok))
    }

    fn graph<'s, T>(
        &'s self,
        graph_name: sophia_api::term::GraphName<T>,
    ) -> impl Graph<Error = Self::Error> + 's
    where
        T: sophia_api::prelude::Term + 's,
    {
        match graph_name {
            None => self.graphs.get(&None).unwrap(), // there is always a default graph
            Some(t) => match self.names.get_index(t) {
                Some(i) => self.graphs.get(&Some(i)).unwrap(),
                None => &self.empty,
            },
        }
    }
}

impl<D: Recognized, R: RuleSet> CollectibleDataset for ReasonableDataset<D, R> {
    type CollectError = Inconsistency;

    fn from_quad_source<TS: sophia_api::prelude::QuadSource>(
        mut quads: TS,
    ) -> sophia_api::source::StreamResult<Self, TS::Error, Self::CollectError> {
        let mut ret = Self {
            names: BasicTermIndex::new(),
            graphs: [(None, ReasonableGraph::prepare())].into_iter().collect(),
            empty: ReasonableGraph::prepare(),
        };
        quads.try_for_each_quad(|q| -> Result<(), Inconsistency> {
            let g = if let Some(gn) = q.g() {
                let ign = ret.names.ensure_index(gn).unwrap();
                ret.graphs
                    .entry(Some(ign))
                    .or_insert_with(|| ReasonableGraph::prepare())
            } else {
                ret.graphs.get_mut(&None).unwrap()
            };
            g.insert_triple(q.spog().0)
        })?;
        ret.graphs
            .par_iter_mut()
            .try_for_each(|(_, g)| g.finalize())
            .map_err(SinkError)?;
        ret.empty.finalize().map_err(SinkError)?;
        Ok(ret)
    }
}

//

struct QuadsMatching<'a, D, R, I, S, P, O> {
    dataset: &'a ReasonableDataset<D, R>,
    graphs: I,
    current: Option<Option<IndexedTerm<'a, usize>>>,
    triples: std::vec::IntoIter<Result<[IndexedTerm<'a, usize>; 3], Infallible>>,
    sm: S,
    pm: P,
    om: O,
}

impl<'a, D, R, I, S, P, O> Iterator for QuadsMatching<'a, D, R, I, S, P, O>
where
    D: Recognized,
    R: RuleSet,
    I: Iterator<Item = (Option<usize>, Option<IndexedTerm<'a, usize>>)>,
    S: TermMatcher,
    P: TermMatcher,
    O: TermMatcher,
{
    type Item = Result<Spog<IndexedTerm<'a, usize>>, Infallible>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(gn) = self.current {
            if let Some(res) = self.triples.next() {
                Some(res.map(|spo| (spo, gn)))
            } else if let Some((io, to)) = self.graphs.next() {
                self.current = Some(to);
                self.triples = self
                    .dataset
                    .graphs
                    .get(&io)
                    .unwrap()
                    .triples_matching(
                        self.sm.matcher_ref(),
                        self.pm.matcher_ref(),
                        self.om.matcher_ref(),
                    )
                    .collect::<Vec<_>>()
                    .into_iter();
                self.next()
            } else {
                self.current = None;
                None
            }
        } else {
            None
        }
    }
}
