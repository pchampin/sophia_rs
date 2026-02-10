// An indexed graph where Triple Terms are also based on indexes (unlike, e.g. sophia_inmem::FastGraph)

use std::{marker::PhantomData, sync::Arc};

use sophia_api::{
    graph::{CollectibleGraph, GResult, Graph},
    prelude::Any,
    source::TripleSource,
    sparql::SparqlDataset,
    term::{SimpleTerm, Term, matcher::TermMatcher},
    triple::Triple,
};
use sophia_sparql::SparqlWrapper;

use crate::{
    _dedup::UsizeIteratorDedup,
    _range_n::RangeN,
    InternalTerm, ReasonableGraph, ReasonableTerm,
    d_entailment::{IllTypedLiteral, Recognized},
    ruleset::RuleSet,
};

impl<D, R> std::fmt::Debug for ReasonableGraph<D, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReasonableGraph")
            .field("i2t", &self.i2t)
            .field("t2i", &self.t2i)
            .field("spo", &self.spo)
            .field("pos", &self.pos)
            .field("osp", &self.osp)
            .finish()
    }
}

impl<D: Recognized, R: RuleSet> ReasonableGraph<D, R> {
    fn new() -> Self {
        let mut ret = Self {
            i2t: Default::default(),
            t2i: Default::default(),
            spo: Default::default(),
            pos: Default::default(),
            osp: Default::default(),
            _phantom: PhantomData,
        };
        R::prepare(&mut ret);
        ret
    }

    /// Return true if `self` entails the `other` graph,
    /// under the entailment regimes captured by `D` and `R`.
    pub fn entails<G: Graph>(&self, other: G) -> GResult<G, bool> {
        self.entails_triples(other.triples())
    }

    /// Return true if `self` entails all given `triples`,
    /// under the entailment regimes captured by `D` and `R`.
    pub fn entails_triples<TS: TripleSource>(&self, triples: TS) -> Result<bool, TS::Error> {
        let query = SparqlWrapper::prepare_ask_from_triples(D::normalize_triples(triples)).unwrap();
        let sparql_other = SparqlWrapper(&self.as_dataset());
        Ok(sparql_other.query(&query).unwrap().into_boolean())
    }

    /// Return a ReasonableTerm, assuming the index is valid
    fn get_term(&self, index: usize) -> ReasonableTerm<'_, D, R> {
        debug_assert!(index < self.i2t.len());
        ReasonableTerm::new(self, index)
    }

    fn get_triple(&self, spo: &[usize; 3]) -> [ReasonableTerm<'_, D, R>; 3] {
        (*spo).map(|i| self.get_term(i))
    }

    pub(crate) fn get_index<T: Term + ?Sized>(&self, term: &T) -> Option<usize> {
        self.t2i.get(&self.try_to_internal(term)?).copied()
    }

    /// Convert a given term to in InternalTerm, if possible
    ///
    /// It is not possible if the subterms (literal's datatype, triple term's component)
    /// are not already present in the graph.
    fn try_to_internal<T: Term + ?Sized>(&self, term: &T) -> Option<InternalTerm> {
        Some(match term.as_simple() {
            SimpleTerm::Iri(iri_ref) => {
                InternalTerm::Iri(iri_ref.map_unchecked(|m| m.to_string().into()))
            }
            SimpleTerm::BlankNode(bnode_id) => {
                InternalTerm::BlankNode(bnode_id.map_unchecked(|m| m.to_string().into()))
            }
            SimpleTerm::LiteralDatatype(lex, dt) => {
                let (lex, dt) = D::normalize_or_fallback((lex, dt)).ok()?;
                let idt = self.get_index(&dt)?;
                InternalTerm::TypedLiteral(lex.into(), idt)
            }
            SimpleTerm::LiteralLanguage(lex, tag, dir) => InternalTerm::LangString(
                lex.to_string().into(),
                tag.map_unchecked(|m| m.to_string().into()),
                dir,
            ),
            SimpleTerm::Triple(spo) => InternalTerm::TripleTerm([
                self.get_index(&spo[0])?,
                self.get_index(&spo[1])?,
                self.get_index(&spo[2])?,
            ]),
            SimpleTerm::Variable(var_name) => {
                InternalTerm::Variable(var_name.map_unchecked(|m| m.to_string().into()))
            }
        })
    }

    pub(crate) fn get_or_make_index<T: Term + ?Sized>(
        &mut self,
        term: &T,
    ) -> Result<usize, IllTypedLiteral> {
        let key = Arc::new(match term.as_simple() {
            SimpleTerm::Iri(iri_ref) => {
                InternalTerm::Iri(iri_ref.map_unchecked(|m| m.to_string().into()))
            }
            SimpleTerm::BlankNode(bnode_id) => {
                InternalTerm::BlankNode(bnode_id.map_unchecked(|m| m.to_string().into()))
            }
            SimpleTerm::LiteralDatatype(lex, dt) => {
                let (lex, dt) = D::normalize_or_fallback((lex, dt))?;
                let idt = self.get_or_make_index(&dt)?;
                InternalTerm::TypedLiteral(lex.into(), idt)
            }
            SimpleTerm::LiteralLanguage(lex, tag, dir) => InternalTerm::LangString(
                lex.to_string().into(),
                tag.map_unchecked(|m| m.to_string().into()),
                dir,
            ),
            SimpleTerm::Triple(spo) => InternalTerm::TripleTerm([
                self.get_or_make_index(&spo[0])?,
                self.get_or_make_index(&spo[1])?,
                self.get_or_make_index(&spo[2])?,
            ]),
            SimpleTerm::Variable(var_name) => {
                InternalTerm::Variable(var_name.map_unchecked(|m| m.to_string().into()))
            }
        });
        Ok(*self.t2i.entry(key).or_insert_with_key(|key| {
            let ret = self.i2t.len();
            self.i2t.push(key.clone());
            ret
        }))
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
        = [ReasonableTerm<'x, D, R>; 3]
    where
        Self: 'x;

    type Error = std::convert::Infallible;

    fn triples(&self) -> impl Iterator<Item = GResult<Self, Self::Triple<'_>>> + '_ {
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
        self.i2t.iter().enumerate().filter_map(|(idx, item)| {
            if matches!(item.as_ref(), InternalTerm::Iri(_)) {
                Some(Ok(self.get_term(idx)))
            } else {
                None
            }
        })
    }

    fn blank_nodes(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        self.i2t.iter().enumerate().filter_map(|(idx, item)| {
            if matches!(item.as_ref(), InternalTerm::BlankNode(_)) {
                Some(Ok(self.get_term(idx)))
            } else {
                None
            }
        })
    }

    fn literals(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        self.i2t.iter().enumerate().filter_map(|(idx, item)| {
            if matches!(
                item.as_ref(),
                InternalTerm::TypedLiteral(..) | InternalTerm::LangString(..)
            ) {
                Some(Ok(self.get_term(idx)))
            } else {
                None
            }
        })
    }

    fn quoted_triples<'s>(
        &'s self,
    ) -> Box<dyn Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'s, Self>>> + 's>
    where
        sophia_api::graph::GTerm<'s, Self>: Clone,
    {
        Box::new(self.i2t.iter().enumerate().filter_map(|(idx, item)| {
            if matches!(item.as_ref(), InternalTerm::TripleTerm(_)) {
                Some(Ok(self.get_term(idx)))
            } else {
                None
            }
        }))
    }

    fn variables(
        &self,
    ) -> impl Iterator<Item = GResult<Self, sophia_api::graph::GTerm<'_, Self>>> + '_ {
        self.i2t.iter().enumerate().filter_map(|(idx, item)| {
            if matches!(item.as_ref(), InternalTerm::Variable(_)) {
                Some(Ok(self.get_term(idx)))
            } else {
                None
            }
        })
    }

    fn contains<TS, TP, TO>(&self, s: TS, p: TP, o: TO) -> GResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        self.triples_matching([s], [p], [o])
            .next()
            .transpose()
            .map(|o| o.is_some())
    }
}

impl<D: Recognized, R: RuleSet> CollectibleGraph for ReasonableGraph<D, R> {
    type CollectError = IllTypedLiteral;

    fn from_triple_source<TS: sophia_api::prelude::TripleSource>(
        mut triples: TS,
    ) -> sophia_api::source::StreamResult<Self, TS::Error, Self::CollectError> {
        let mut ret = Self::new();
        triples.try_for_each_triple(|t| -> Result<(), IllTypedLiteral> {
            let si = ret.get_or_make_index(&t.s())?;
            let pi = ret.get_or_make_index(&t.p())?;
            let oi = ret.get_or_make_index(&t.o())?;
            ret.insert([si, pi, oi]);
            Ok(())
        })?;
        R::saturate(&mut ret);
        Ok(ret)
    }
}
