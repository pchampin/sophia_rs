// An indexed graph where Triple Terms are also based on indexes (unlike, e.g. sophia_inmem::FastGraph)

use std::marker::PhantomData;

use sophia_api::{
    graph::{CollectibleGraph, GResult, Graph},
    prelude::Any,
    source::{StreamError::SinkError, TripleSource},
    sparql::SparqlDataset,
    term::{SimpleTerm, Term, matcher::TermMatcher},
    triple::Triple,
};
use sophia_inmem::index::{IndexedTerm, TermIndex};
use sophia_sparql::SparqlWrapper;

use crate::{
    _dedup::UsizeIteratorDedup,
    _range_n::RangeN,
    Inconsistency, ReasonableGraph,
    d_entailment::{IllTypedLiteral, NormalizeError, Recognized},
    ruleset::RuleSet,
};

impl<D, R> Clone for ReasonableGraph<D, R> {
    fn clone(&self) -> Self {
        Self {
            terms: self.terms.clone(),
            rdt: self.rdt.clone(),
            spo: self.spo.clone(),
            pos: self.pos.clone(),
            osp: self.osp.clone(),
            _phantom: self._phantom,
            #[cfg(debug_assertions)]
            finalized: self.finalized,
        }
    }
}

impl<D, R> std::fmt::Debug for ReasonableGraph<D, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReasonableGraph")
            .field("terms", &self.terms)
            .field("rdt", &self.rdt)
            .field("spo", &self.spo)
            .field("pos", &self.pos)
            .field("osp", &self.osp)
            .finish()
    }
}

impl<D: Recognized, R: RuleSet> ReasonableGraph<D, R> {
    /// This method prepares a [`ReasonableGraph`] that still needs to be
    /// [finalized](ReasonableGraph::finalize), potentially after several calls to
    /// [`insert_triple`](ReasonableGraph::insert_triple).
    ///
    /// Using a [`ReasonableGraph`] before it is finalized will produce incorrect results.
    pub(crate) fn prepare() -> Self {
        let mut ret = Self {
            terms: Default::default(),
            rdt: (0..0),
            spo: Default::default(),
            pos: Default::default(),
            osp: Default::default(),
            _phantom: PhantomData,
            #[cfg(debug_assertions)]
            finalized: false,
        };
        R::prepare(&mut ret);
        ret
    }

    /// This method must not be used after this [`ReasonableGraph`] has been [finalized](ReasonableGraph::finalize).
    pub(crate) fn insert_triple<T: Triple>(&mut self, t: T) -> Result<(), Inconsistency> {
        let si = self.get_or_make_index(&t.s())?;
        let pi = self.get_or_make_index(&t.p())?;
        let oi = self.get_or_make_index(&t.o())?;
        self.insert([si, pi, oi]);
        Ok(())
    }

    pub(crate) fn finalize(&mut self) -> Result<(), Inconsistency> {
        #[cfg(debug_assertions)]
        {
            self.finalized = true;
        }
        R::saturate(self)
    }

    /// Return true if `self` entails the `other` graph,
    /// under the entailment regimes captured by `D` and `R`.
    pub fn entails<G: Graph>(&self, other: G) -> Result<bool, NormalizeError<G::Error>> {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.entails_triples(other.triples())
    }

    /// Return true if `self` entails all given `triples`,
    /// under the entailment regimes captured by `D` and `R`.
    pub fn entails_triples<TS: TripleSource>(
        &self,
        triples: TS,
    ) -> Result<bool, NormalizeError<TS::Error>> {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        let query = SparqlWrapper::prepare_ask_from_triples(D::normalize_triples(triples))?;
        let sparql_other = SparqlWrapper(&self.as_dataset());
        Ok(sparql_other.query(&query).unwrap().into_boolean())
    }

    /// Return a IndexedTerm, assuming the index is valid
    pub(crate) fn get_term(&self, index: usize) -> IndexedTerm<'_, usize> {
        debug_assert!(index < self.terms.len());
        self.terms.get_term(index)
    }

    fn get_triple(&self, spo: &[usize; 3]) -> [IndexedTerm<'_, usize>; 3] {
        (*spo).map(|i| self.get_term(i))
    }

    pub(crate) fn get_index<T: Term + ?Sized>(&self, term: &T) -> Option<usize> {
        self.terms.get_index(term.borrow_term())
    }

    pub(crate) fn get_or_make_index<T: Term + ?Sized>(
        &mut self,
        term: &T,
    ) -> Result<usize, IllTypedLiteral> {
        Ok(match term.as_simple() {
            SimpleTerm::LiteralDatatype(lex, dt) => {
                let (lex, dt) = D::normalize_or_fallback((lex, dt))?;
                self.terms
                    .ensure_index(SimpleTerm::LiteralDatatype(lex, dt))
                    .unwrap()
            }
            SimpleTerm::Triple(spo) => {
                let spo = [0, 1, 2].map(|i| self.get_or_make_index(&spo[i]).unwrap());
                self.terms.ensure_triple_term_index(spo).unwrap()
            }
            _ => self.terms.ensure_index(term.borrow_term()).unwrap(),
        })
    }

    pub(crate) fn insert(&mut self, [si, pi, oi]: [usize; 3]) -> bool {
        self.spo.insert([si, pi, oi]) && {
            self.pos.insert([pi, oi, si]);
            self.osp.insert([oi, si, pi]);
            true
        }
    }

    pub(crate) fn insert_all(&mut self, triples: &mut Vec<[usize; 3]>) -> bool {
        let mut ret = false;
        for t in triples.drain(..) {
            ret = self.insert(t) || ret;
        }
        ret
    }
}

impl<D: Recognized, R: RuleSet> Graph for ReasonableGraph<D, R> {
    type Triple<'x>
        = [IndexedTerm<'x, usize>; 3]
    where
        Self: 'x;

    type Error = std::convert::Infallible;

    fn triples(&self) -> impl Iterator<Item = GResult<Self, Self::Triple<'_>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.spo
            .iter()
            .copied()
            .map(move |spo| Ok(spo.map(|i| self.get_term(i))))
    }

    fn triples_matching<'s, 't, S, P, O>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
    ) -> impl Iterator<Item = GResult<Self, Self::Triple<'s>>> + 't
    where
        's: 't,
        S: TermMatcher + 't,
        P: TermMatcher + 't,
        O: TermMatcher + 't,
    {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        match (sm.constant(), pm.constant(), om.constant()) {
            (None, None, None) => Box::new(
                self.spo
                    .iter()
                    .map(|spo| self.get_triple(spo))
                    .filter(move |[s, p, o]| sm.matches(s) && pm.matches(p) && om.matches(o))
                    .map(Ok),
            ),
            (Some(sm), None, None) => {
                if let Some(si) = self.get_index(sm) {
                    Box::new(
                        self.spo
                            .range1(si)
                            .map(|spo| self.get_triple(spo))
                            .filter(move |[_, p, o]| pm.matches(p) && om.matches(o))
                            .map(Ok),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
            (None, Some(pm), None) => {
                if let Some(pi) = self.get_index(pm) {
                    Box::new(
                        self.pos
                            .range1(pi)
                            .map(|pos| self.get_triple(pos))
                            .filter(move |[_, o, s]| om.matches(o) && sm.matches(s))
                            .map(|[p, o, s]| Ok([s, p, o])),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
            (None, None, Some(om)) => {
                if let Some(oi) = self.get_index(om) {
                    Box::new(
                        self.osp
                            .range1(oi)
                            .map(|osp| self.get_triple(osp))
                            .filter(move |[_, s, p]| sm.matches(s) && pm.matches(p))
                            .map(|[o, s, p]| Ok([s, p, o])),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
            (None, Some(pm), Some(om)) => {
                if let Some(pi) = self.get_index(pm)
                    && let Some(oi) = self.get_index(om)
                {
                    Box::new(
                        self.pos
                            .range2(pi, oi)
                            .map(|pos| self.get_triple(pos))
                            .filter(move |[.., s]| sm.matches(s))
                            .map(|[p, o, s]| Ok([s, p, o])),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
            (Some(sm), None, Some(om)) => {
                if let Some(si) = self.get_index(sm)
                    && let Some(oi) = self.get_index(om)
                {
                    Box::new(
                        self.osp
                            .range2(oi, si)
                            .map(|osp| self.get_triple(osp))
                            .filter(move |[.., p]| pm.matches(p))
                            .map(|[o, s, p]| Ok([s, p, o])),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
            (Some(sm), Some(pm), None) => {
                if let Some(si) = self.get_index(sm)
                    && let Some(pi) = self.get_index(pm)
                {
                    Box::new(
                        self.spo
                            .range2(si, pi)
                            .map(|spo| self.get_triple(spo))
                            .filter(move |[.., o]| om.matches(o))
                            .map(Ok),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
            (Some(sm), Some(pm), Some(om)) => {
                if let Some(si) = self.get_index(sm)
                    && let Some(pi) = self.get_index(pm)
                    && let Some(oi) = self.get_index(om)
                {
                    Box::new(
                        self.spo
                            .range3(si, pi, oi)
                            .map(|spo| Ok(self.get_triple(spo))),
                    ) as Box<dyn Iterator<Item = _>>
                } else {
                    Box::new(std::iter::empty())
                }
            }
        }
    }

    fn subjects(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.spo
            .iter()
            .map(|spo| spo[0])
            .dedup()
            .map(|i| Ok(self.get_term(i)))
    }

    fn subjects_matching<'s, M: TermMatcher + 's>(
        &'s self,
        matcher: M,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'s, Self>>> + 's {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        if matcher.constant().is_some() {
            if let Some(t) = self.triples_matching(matcher, Any, Any).next() {
                Box::new(std::iter::once(Ok(t.unwrap()[0]))) as Box<dyn Iterator<Item = _>>
            } else {
                Box::new(std::iter::empty())
            }
        } else {
            Box::new(
                self.subjects()
                    .filter(move |t| matcher.matches(t.as_ref().unwrap())),
            )
        }
    }

    fn predicates(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.pos
            .iter()
            .map(|pos| pos[0])
            .dedup()
            .map(|i| Ok(self.get_term(i)))
    }

    fn predicates_matching<'s, M: TermMatcher + 's>(
        &'s self,
        matcher: M,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'s, Self>>> + 's {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        if matcher.constant().is_some() {
            if let Some(t) = self.triples_matching(Any, matcher, Any).next() {
                Box::new(std::iter::once(Ok(t.unwrap()[1]))) as Box<dyn Iterator<Item = _>>
            } else {
                Box::new(std::iter::empty())
            }
        } else {
            Box::new(
                self.subjects()
                    .filter(move |t| matcher.matches(t.as_ref().unwrap())),
            )
        }
    }

    fn objects(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.osp
            .iter()
            .map(|osp| osp[0])
            .dedup()
            .map(|i| Ok(self.get_term(i)))
    }

    fn objects_matching<'s, M: TermMatcher + 's>(
        &'s self,
        matcher: M,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'s, Self>>> + 's {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        if matcher.constant().is_some() {
            if let Some(t) = self.triples_matching(Any, Any, matcher).next() {
                Box::new(std::iter::once(Ok(t.unwrap()[2]))) as Box<dyn Iterator<Item = _>>
            } else {
                Box::new(std::iter::empty())
            }
        } else {
            Box::new(
                self.subjects()
                    .filter(move |t| matcher.matches(t.as_ref().unwrap())),
            )
        }
    }

    fn iris(&self) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.terms.iris().map(Ok)
    }

    fn blank_nodes(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.terms.blank_nodes().map(Ok)
    }

    fn literals(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.terms.literals().map(Ok)
    }

    fn triple_terms<'s>(
        &'s self,
    ) -> Box<dyn Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'s, Self>>> + 's>
    where
        sophia_api::graph::GTerm<'s, Self>: Clone,
    {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        Box::new(self.terms.literals().map(Ok))
    }

    fn variables(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.terms.variables().map(Ok)
    }

    fn contains<TS, TP, TO>(&self, s: TS, p: TP, o: TO) -> GResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        #[cfg(debug_assertions)]
        debug_assert!(self.finalized);
        self.triples_matching([s], [p], [o])
            .next()
            .transpose()
            .map(|o| o.is_some())
    }
}

impl<D: Recognized, R: RuleSet> CollectibleGraph for ReasonableGraph<D, R> {
    type CollectError = Inconsistency;

    fn from_triple_source<TS: sophia_api::prelude::TripleSource>(
        mut triples: TS,
    ) -> sophia_api::source::StreamResult<Self, TS::Error, Self::CollectError> {
        let mut ret = Self::prepare();
        triples.try_for_each_triple(|t| -> Result<(), Inconsistency> { ret.insert_triple(t) })?;
        ret.finalize().map_err(SinkError)?;
        Ok(ret)
    }
}
