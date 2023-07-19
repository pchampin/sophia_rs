use std::collections::BTreeSet;
use std::iter::{empty, once};

use sophia_api::graph::{CollectibleGraph, SetGraph};
use sophia_api::prelude::*;

use crate::index::*;

mod _iter;
pub(crate) use _iter::TermData;
use _iter::*;

/// A graph with a single triple index (SPO).
/// Fast to load but slow to query, with a relatively low memory footprint.
#[derive(Clone, Debug, Default)]
pub struct GenericLightGraph<TI: TermIndex> {
    terms: TI,
    triples: BTreeSet<[TI::Index; 3]>,
}

impl<TI: TermIndex + Default> GenericLightGraph<TI> {
    pub fn new() -> Self {
        Self {
            terms: TI::default(),
            triples: BTreeSet::new(),
        }
    }
}

impl<TI: TermIndex> Graph for GenericLightGraph<TI> {
    type Triple<'x> = [<TI::Term as Term>::BorrowTerm<'x>; 3] where Self: 'x;
    type Error = TI::Error;

    fn triples(&self) -> sophia_api::graph::GTripleSource<Self> {
        Box::new(
            self.triples
                .iter()
                .map(|ti| Ok(ti.map(|i| self.terms.get_term(i)))),
        )
    }

    fn triples_matching<'s, S, P, O>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
    ) -> sophia_api::graph::GTripleSource<'s, Self>
    where
        S: sophia_api::term::matcher::TermMatcher + 's,
        P: sophia_api::term::matcher::TermMatcher + 's,
        O: sophia_api::term::matcher::TermMatcher + 's,
    {
        if let Some(sc) = sm.constant() {
            let Some(si) = self.terms.get_index(sc.borrow_term()) else {
                return Box::new(empty())
            };
            let s = self.terms.get_term(si);
            if let Some(pc) = pm.constant() {
                let Some(pi) = self.terms.get_index(pc.borrow_term()) else {
                    return Box::new(empty())
                };
                let p = self.terms.get_term(pi);
                if let Some(oc) = om.constant() {
                    let Some(oi) = self.terms.get_index(oc.borrow_term()) else {
                        return Box::new(empty())
                    };
                    let o = self.terms.get_term(oi);

                    if self.triples.contains(&[si, pi, oi]) {
                        Box::new(once(Ok([s, p, o])))
                    } else {
                        Box::new(empty())
                    }
                } else {
                    let r = [si, pi, TI::Index::ZERO]..=[si, pi, TI::Index::MAX];
                    Box::new(
                        self.triples
                            .range(r)
                            .map(|t| self.terms.get_term(t[2]))
                            .filter(move |o| om.matches(o))
                            .map(move |o| Ok([s, p, o])),
                    )
                }
            } else {
                let r =
                    [si, TI::Index::ZERO, TI::Index::ZERO]..=[si, TI::Index::MAX, TI::Index::MAX];
                BcMatchingIterator::boxed(&self.terms, self.triples.range(r), pm, om, |spo| spo)
            }
        } else {
            SpoMatchingIterator::boxed(&self.terms, self.triples.iter(), sm, pm, om)
        }
    }
}

impl<TI: TermIndex> MutableGraph for GenericLightGraph<TI> {
    type MutationError = TI::Error;

    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> sophia_api::graph::MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        let is = self.terms.ensure_index(s)?;
        let ip = self.terms.ensure_index(p)?;
        let io = self.terms.ensure_index(o)?;
        Ok(self.triples.insert([is, ip, io]))
    }

    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> sophia_api::graph::MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        let Some(is) = self.terms.get_index(s) else {
            return Ok(false);
        };
        let Some(ip) = self.terms.get_index(p) else {
            return Ok(false);
        };
        let Some(io) = self.terms.get_index(o) else {
            return Ok(false);
        };
        Ok(self.triples.remove(&[is, ip, io]))
    }
}

impl<TI: TermIndex + Default> CollectibleGraph for GenericLightGraph<TI> {
    fn from_triple_source<TS: TripleSource>(
        mut triples: TS,
    ) -> sophia_api::source::StreamResult<Self, TS::Error, Self::Error> {
        let mut g = Self::new();
        triples.try_for_each_triple(|t| g.insert_triple(t).map(|_| ()))?;
        Ok(g)
    }
}

impl<TI: TermIndex> SetGraph for GenericLightGraph<TI> {}

//

/// A heavily indexed graph.
/// Fast to query but slow to load, with a relatively high memory footprint.
#[derive(Clone, Debug, Default)]
pub struct GenericFastGraph<TI: TermIndex> {
    terms: TI,
    spo: BTreeSet<[TI::Index; 3]>,
    pos: BTreeSet<[TI::Index; 3]>,
    osp: BTreeSet<[TI::Index; 3]>,
}

impl<TI: TermIndex + Default> GenericFastGraph<TI> {
    pub fn new() -> Self {
        Self {
            terms: TI::default(),
            spo: BTreeSet::new(),
            pos: BTreeSet::new(),
            osp: BTreeSet::new(),
        }
    }
}

impl<TI: TermIndex> Graph for GenericFastGraph<TI> {
    type Triple<'x> = [<TI::Term as Term>::BorrowTerm<'x>; 3] where Self: 'x;
    type Error = TI::Error;

    fn triples(&self) -> sophia_api::graph::GTripleSource<Self> {
        Box::new(
            self.spo
                .iter()
                .map(|ti| Ok(ti.map(|i| self.terms.get_term(i)))),
        )
    }

    fn triples_matching<'s, S, P, O>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
    ) -> sophia_api::graph::GTripleSource<'s, Self>
    where
        S: sophia_api::term::matcher::TermMatcher + 's,
        P: sophia_api::term::matcher::TermMatcher + 's,
        O: sophia_api::term::matcher::TermMatcher + 's,
    {
        let si = match sm.constant().map(|t| self.terms.get_index(t.borrow_term())) {
            None => None,
            Some(None) => return Box::new(empty()),
            Some(Some(i)) => Some(i),
        };
        let pi = match pm.constant().map(|t| self.terms.get_index(t.borrow_term())) {
            None => None,
            Some(None) => return Box::new(empty()),
            Some(Some(i)) => Some(i),
        };
        let oi = match om.constant().map(|t| self.terms.get_index(t.borrow_term())) {
            None => None,
            Some(None) => return Box::new(empty()),
            Some(Some(i)) => Some(i),
        };
        match (si, pi, oi) {
            (Some(si), Some(pi), Some(oi)) => {
                let ti = [si, pi, oi];
                if self.spo.contains(&ti) {
                    Box::new(once(Ok(ti.map(|i| self.terms.get_term(i)))))
                } else {
                    Box::new(empty())
                }
            }
            (Some(si), Some(pi), None) => {
                let [s, p] = [si, pi].map(|i| self.terms.get_term(i));
                let r = [si, pi, TI::Index::ZERO]..=[si, pi, TI::Index::MAX];
                Box::new(
                    self.spo
                        .range(r)
                        .map(|t| self.terms.get_term(t[2]))
                        .filter(move |o| om.matches(o))
                        .map(move |o| Ok([s, p, o])),
                )
            }
            (Some(si), None, None) => {
                let r =
                    [si, TI::Index::ZERO, TI::Index::ZERO]..=[si, TI::Index::MAX, TI::Index::MAX];
                BcMatchingIterator::boxed(&self.terms, self.spo.range(r), pm, om, |spo| spo)
            }
            (None, Some(pi), Some(oi)) => {
                let [p, o] = [pi, oi].map(|i| self.terms.get_term(i));
                let r = [pi, oi, TI::Index::ZERO]..=[pi, oi, TI::Index::MAX];
                Box::new(
                    self.pos
                        .range(r)
                        .map(|t| self.terms.get_term(t[2]))
                        .filter(move |s| sm.matches(s))
                        .map(move |s| Ok([s, p, o])),
                )
            }
            (None, Some(pi), None) => {
                let r =
                    [pi, TI::Index::ZERO, TI::Index::ZERO]..=[pi, TI::Index::MAX, TI::Index::MAX];
                BcMatchingIterator::boxed(&self.terms, self.pos.range(r), om, sm, |[p, o, s]| {
                    [s, p, o]
                })
            }
            (Some(si), None, Some(oi)) => {
                let [o, s] = [oi, si].map(|i| self.terms.get_term(i));
                let r = [oi, si, TI::Index::ZERO]..=[oi, si, TI::Index::MAX];
                Box::new(
                    self.osp
                        .range(r)
                        .map(|t| self.terms.get_term(t[2]))
                        .filter(move |p| pm.matches(p))
                        .map(move |p| Ok([s, p, o])),
                )
            }
            (None, None, Some(oi)) => {
                let r =
                    [oi, TI::Index::ZERO, TI::Index::ZERO]..=[oi, TI::Index::MAX, TI::Index::MAX];
                BcMatchingIterator::boxed(&self.terms, self.osp.range(r), sm, pm, |[o, s, p]| {
                    [s, p, o]
                })
            }
            (None, None, None) => {
                SpoMatchingIterator::boxed(&self.terms, self.spo.iter(), sm, pm, om)
            }
        }
    }
}

impl<TI: TermIndex> MutableGraph for GenericFastGraph<TI> {
    type MutationError = TI::Error;

    fn insert<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> sophia_api::graph::MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        let is = self.terms.ensure_index(s)?;
        let ip = self.terms.ensure_index(p)?;
        let io = self.terms.ensure_index(o)?;
        if self.spo.insert([is, ip, io]) {
            let i = self.pos.insert([ip, io, is]);
            debug_assert!(i);
            let i = self.osp.insert([io, is, ip]);
            debug_assert!(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn remove<TS, TP, TO>(&mut self, s: TS, p: TP, o: TO) -> sophia_api::graph::MgResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
    {
        let Some(is) = self.terms.get_index(s) else {
            return Ok(false);
        };
        let Some(ip) = self.terms.get_index(p) else {
            return Ok(false);
        };
        let Some(io) = self.terms.get_index(o) else {
            return Ok(false);
        };
        if self.spo.remove(&[is, ip, io]) {
            let i = self.pos.remove(&[ip, io, is]);
            debug_assert!(i);
            let i = self.osp.remove(&[io, is, ip]);
            debug_assert!(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<TI: TermIndex + Default> CollectibleGraph for GenericFastGraph<TI> {
    fn from_triple_source<TS: TripleSource>(
        mut triples: TS,
    ) -> sophia_api::source::StreamResult<Self, TS::Error, Self::Error> {
        let mut g = Self::new();
        triples.try_for_each_triple(|t| g.insert_triple(t).map(|_| ()))?;
        Ok(g)
    }
}

impl<TI: TermIndex> SetGraph for GenericFastGraph<TI> {}

pub type LightGraph = GenericLightGraph<SimpleTermIndex<u32>>;
pub type FastGraph = GenericFastGraph<SimpleTermIndex<u32>>;

#[cfg(test)]
mod test {
    use super::{FastGraph, LightGraph};

    sophia_api::test_graph_impl!(light_graph, LightGraph);
    sophia_api::test_graph_impl!(fast_graph, FastGraph);

    #[test]
    fn new_available() {
        // ::new() is only available if the underlying TermIndex implements Default,
        // so let's check that FastGraph and LightGraph do have it.
        let _ = FastGraph::new();
        let _ = LightGraph::new();
    }
}

/// Flavors of Graph implementations with a smaller memory-footprint.
///
/// The trade-off is that these implementations can only contain a small number (2^16) of terms.
///
pub mod small {
    use crate::index::SimpleTermIndex;

    /// A graph with a single triple index (SPO).
    /// Fast to load but slow to query, with a relatively low memory footprint.
    pub type LightGraph = super::GenericLightGraph<SimpleTermIndex<u16>>;
    /// A heavily indexed graph.
    /// Fast to query but slow to load, with a relatively high memory footprint.
    pub type FastGraph = super::GenericFastGraph<SimpleTermIndex<u16>>;

    #[cfg(all(test, feature = "all_tests"))]
    mod test {
        use super::*;
        sophia_api::test_graph_impl!(fast_graph, FastGraph);
        sophia_api::test_graph_impl!(light_graph, LightGraph);
    }
}
