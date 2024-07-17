//! In-memory implementations of [`Dataset`]
use std::collections::BTreeSet;
use std::iter::{empty, once};

use sophia_api::dataset::{CollectibleDataset, DResult, SetDataset};
use sophia_api::prelude::*;
use sophia_api::quad::Gspo;
use sophia_api::term::GraphName;

use crate::index::*;

mod _iter;
use _iter::*;

/// A dataset with a single quad index (GSPO).
/// Fast to load but slow on some queries, with a relatively low memory footprint.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GenericLightDataset<TI: TermIndex> {
    terms: TI,
    quads: BTreeSet<[TI::Index; 4]>,
}

impl<TI: GraphNameIndex + Default> GenericLightDataset<TI> {
    /// Construct an empty dataset
    pub fn new() -> Self {
        Self {
            terms: TI::default(),
            quads: BTreeSet::new(),
        }
    }
}

impl<TI: GraphNameIndex> Dataset for GenericLightDataset<TI> {
    type Quad<'x> = Gspo<<TI::Term as Term>::BorrowTerm<'x>> where Self: 'x;
    type Error = TI::Error;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        self.quads.iter().map(|[gi, ti @ ..]| {
            Ok((
                self.terms.get_graph_name(*gi),
                ti.map(|i| self.terms.get_term(i)),
            ))
        })
    }

    #[allow(refining_impl_trait)]
    fn quads_matching<'s, S, P, O, G>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
        gm: G,
    ) -> Box<dyn Iterator<Item = DResult<Self, Self::Quad<'s>>> + 's>
    where
        S: sophia_api::term::matcher::TermMatcher + 's,
        P: sophia_api::term::matcher::TermMatcher + 's,
        O: sophia_api::term::matcher::TermMatcher + 's,
        G: sophia_api::term::matcher::GraphNameMatcher + 's,
    {
        if let Some(gc) = gm.constant() {
            let gc = gc.map(|t| t.borrow_term());
            let Some(gi) = self.terms.get_graph_name_index(gc) else {
                return Box::new(empty());
            };
            let g = self.terms.get_graph_name(gi);
            if let Some(sc) = sm.constant() {
                let Some(si) = self.terms.get_index(sc.borrow_term()) else {
                    return Box::new(empty());
                };
                let s = self.terms.get_term(si);
                if let Some(pc) = pm.constant() {
                    let Some(pi) = self.terms.get_index(pc.borrow_term()) else {
                        return Box::new(empty());
                    };
                    let p = self.terms.get_term(pi);
                    if let Some(oc) = om.constant() {
                        let Some(oi) = self.terms.get_index(oc.borrow_term()) else {
                            return Box::new(empty());
                        };
                        let o = self.terms.get_term(oi);

                        if self.quads.contains(&[gi, si, pi, oi]) {
                            Box::new(once(Ok((g, [s, p, o]))))
                        } else {
                            Box::new(empty())
                        }
                    } else {
                        let r = [gi, si, pi, TI::Index::ZERO]..=[gi, si, pi, TI::Index::MAX];
                        Box::new(
                            self.quads
                                .range(r)
                                .map(|q| self.terms.get_term(q[3]))
                                .filter(move |o| om.matches(o))
                                .map(move |o| Ok((g, [s, p, o]))),
                        )
                    }
                } else {
                    let r = [gi, si, TI::Index::ZERO, TI::Index::ZERO]
                        ..=[gi, si, TI::Index::MAX, TI::Index::MAX];
                    CdMatchingIterator::boxed(
                        &self.terms,
                        self.quads.range(r),
                        pm.gn(),
                        om.gn(),
                        |gspo| gspo,
                    )
                }
            } else {
                let r = [gi, TI::Index::ZERO, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[gi, TI::Index::MAX, TI::Index::MAX, TI::Index::ZERO];
                Box::new(BcdMatchingIterator::boxed(
                    &self.terms,
                    self.quads.range(r),
                    sm.gn(),
                    pm.gn(),
                    om.gn(),
                    |gspo| gspo,
                ))
            }
        } else {
            GspoMatchingIterator::boxed(&self.terms, self.quads.iter(), gm, sm, pm, om)
        }
    }
}

impl<TI: GraphNameIndex> MutableDataset for GenericLightDataset<TI> {
    type MutationError = TI::Error;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> sophia_api::dataset::MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        let is = self.terms.ensure_index(s)?;
        let ip = self.terms.ensure_index(p)?;
        let io = self.terms.ensure_index(o)?;
        let ig = match g {
            None => self.terms.get_default_graph_index(),
            Some(gn) => self.terms.ensure_index(gn)?,
        };
        Ok(self.quads.insert([ig, is, ip, io]))
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> sophia_api::dataset::MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
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
        let Some(ig) = self.terms.get_graph_name_index(g) else {
            return Ok(false);
        };
        Ok(self.quads.remove(&[ig, is, ip, io]))
    }
}

impl<TI: GraphNameIndex + Default> CollectibleDataset for GenericLightDataset<TI> {
    fn from_quad_source<QS: QuadSource>(
        mut quads: QS,
    ) -> sophia_api::source::StreamResult<Self, QS::Error, Self::Error> {
        let mut d = Self::new();
        quads.try_for_each_quad(|q| d.insert_quad(q).map(|_| ()))?;
        Ok(d)
    }
}

impl<TI: GraphNameIndex> SetDataset for GenericLightDataset<TI> {}

//

/// A heavily indexed dataset.
/// Fast to query but slow to load, with a relatively high memory footprint.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct GenericFastDataset<TI: GraphNameIndex> {
    terms: TI,
    gspo: BTreeSet<[TI::Index; 4]>,
    gpos: BTreeSet<[TI::Index; 4]>,
    gosp: BTreeSet<[TI::Index; 4]>,
    spog: BTreeSet<[TI::Index; 4]>,
    posg: BTreeSet<[TI::Index; 4]>,
    ospg: BTreeSet<[TI::Index; 4]>,
}

impl<TI: GraphNameIndex + Default> GenericFastDataset<TI> {
    /// Construct an empty dataset
    pub fn new() -> Self {
        Self {
            terms: TI::default(),
            gspo: BTreeSet::new(),
            gpos: BTreeSet::new(),
            gosp: BTreeSet::new(),
            spog: BTreeSet::new(),
            posg: BTreeSet::new(),
            ospg: BTreeSet::new(),
        }
    }
}

impl<TI: GraphNameIndex> Dataset for GenericFastDataset<TI> {
    type Quad<'x> = Gspo<<TI::Term as Term>::BorrowTerm<'x>> where Self: 'x;
    type Error = TI::Error;

    fn quads(&self) -> impl Iterator<Item = DResult<Self, Self::Quad<'_>>> + '_ {
        self.gspo.iter().map(|[gi, ti @ ..]| {
            Ok((
                self.terms.get_graph_name(*gi),
                ti.map(|i| self.terms.get_term(i)),
            ))
        })
    }

    #[allow(refining_impl_trait)]
    fn quads_matching<'s, S, P, O, G>(
        &'s self,
        sm: S,
        pm: P,
        om: O,
        gm: G,
    ) -> Box<dyn Iterator<Item = DResult<Self, Self::Quad<'s>>> + 's>
    where
        S: sophia_api::term::matcher::TermMatcher + 's,
        P: sophia_api::term::matcher::TermMatcher + 's,
        O: sophia_api::term::matcher::TermMatcher + 's,
        G: sophia_api::term::matcher::GraphNameMatcher + 's,
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
        let gi = match gm
            .constant()
            .map(|g| self.terms.get_graph_name_index(g.map(|t| t.borrow_term())))
        {
            None => None,
            Some(None) => return Box::new(empty()),
            Some(Some(i)) => Some(i),
        };
        match (gi, si, pi, oi) {
            (Some(gi), Some(si), Some(pi), Some(oi)) => {
                if self.gspo.contains(&[gi, si, pi, oi]) {
                    Box::new(once(Ok((
                        self.terms.get_graph_name(gi),
                        [si, pi, oi].map(|i| self.terms.get_term(i)),
                    ))))
                } else {
                    Box::new(empty())
                }
            }
            (Some(gi), Some(si), Some(pi), None) => {
                let g = self.terms.get_graph_name(gi);
                let [s, p] = [si, pi].map(|i| self.terms.get_term(i));
                let r = [gi, si, pi, TI::Index::ZERO]..=[gi, si, pi, TI::Index::MAX];
                Box::new(
                    self.gspo
                        .range(r)
                        .map(|q| self.terms.get_term(q[3]))
                        .filter(move |o| om.matches(o))
                        .map(move |o| Ok((g, [s, p, o]))),
                )
            }
            (Some(gi), Some(si), None, Some(oi)) => {
                let g = self.terms.get_graph_name(gi);
                let [o, s] = [oi, si].map(|i| self.terms.get_term(i));
                let r = [gi, oi, si, TI::Index::ZERO]..=[gi, oi, si, TI::Index::MAX];
                Box::new(
                    self.gosp
                        .range(r)
                        .map(|q| self.terms.get_term(q[3]))
                        .filter(move |p| pm.matches(p))
                        .map(move |p| Ok((g, [s, p, o]))),
                )
            }
            (Some(gi), None, Some(pi), Some(oi)) => {
                let g = self.terms.get_graph_name(gi);
                let [p, o] = [pi, oi].map(|i| self.terms.get_term(i));
                let r = [gi, pi, oi, TI::Index::ZERO]..=[gi, pi, oi, TI::Index::MAX];
                Box::new(
                    self.gpos
                        .range(r)
                        .map(|q| self.terms.get_term(q[3]))
                        .filter(move |s| sm.matches(s))
                        .map(move |s| Ok((g, [s, p, o]))),
                )
            }
            (None, Some(si), Some(pi), Some(oi)) => {
                let [s, p, o] = [si, pi, oi].map(|i| self.terms.get_term(i));
                let r = [si, pi, oi, TI::Index::ZERO]..=[si, pi, oi, TI::Index::MAX];
                Box::new(
                    self.spog
                        .range(r)
                        .map(|q| self.terms.get_graph_name(q[3]))
                        .filter(move |g| gm.matches(g.as_ref()))
                        .map(move |g| Ok((g, [s, p, o]))),
                )
            }
            (Some(gi), Some(si), None, None) => {
                let r = [gi, si, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[gi, si, TI::Index::MAX, TI::Index::MAX];
                CdMatchingIterator::boxed(
                    &self.terms,
                    self.gspo.range(r),
                    pm.gn(),
                    om.gn(),
                    |gspo| gspo,
                )
            }
            (Some(gi), None, Some(pi), None) => {
                let r = [gi, pi, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[gi, pi, TI::Index::MAX, TI::Index::MAX];
                CdMatchingIterator::boxed(
                    &self.terms,
                    self.gpos.range(r),
                    om.gn(),
                    sm.gn(),
                    |[g, p, o, s]| [g, s, p, o],
                )
            }
            (Some(gi), None, None, Some(oi)) => {
                let r = [gi, oi, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[gi, oi, TI::Index::MAX, TI::Index::MAX];
                CdMatchingIterator::boxed(
                    &self.terms,
                    self.gosp.range(r),
                    sm.gn(),
                    pm.gn(),
                    |[g, o, s, p]| [g, s, p, o],
                )
            }
            (None, Some(si), Some(pi), None) => {
                let r = [si, pi, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[si, pi, TI::Index::MAX, TI::Index::MAX];
                CdMatchingIterator::boxed(
                    &self.terms,
                    self.spog.range(r),
                    om.gn(),
                    gm,
                    |[s, p, o, g]| [g, s, p, o],
                )
            }
            (None, Some(si), None, Some(oi)) => {
                let r = [oi, si, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[oi, si, TI::Index::MAX, TI::Index::MAX];
                CdMatchingIterator::boxed(
                    &self.terms,
                    self.ospg.range(r),
                    pm.gn(),
                    gm,
                    |[o, s, p, g]| [g, s, p, o],
                )
            }
            (None, None, Some(pi), Some(oi)) => {
                let r = [pi, oi, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[pi, oi, TI::Index::MAX, TI::Index::MAX];
                CdMatchingIterator::boxed(
                    &self.terms,
                    self.posg.range(r),
                    sm.gn(),
                    gm,
                    |[p, o, s, g]| [g, s, p, o],
                )
            }
            (Some(gi), None, None, None) => {
                let r = [gi, TI::Index::ZERO, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[gi, TI::Index::MAX, TI::Index::MAX, TI::Index::MAX];
                BcdMatchingIterator::boxed(
                    &self.terms,
                    self.gspo.range(r),
                    sm.gn(),
                    pm.gn(),
                    om.gn(),
                    |gspo| gspo,
                )
            }
            (None, Some(si), None, None) => {
                let r = [si, TI::Index::ZERO, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[si, TI::Index::MAX, TI::Index::MAX, TI::Index::MAX];
                BcdMatchingIterator::boxed(
                    &self.terms,
                    self.spog.range(r),
                    pm.gn(),
                    om.gn(),
                    gm,
                    |[s, p, o, g]| [g, s, p, o],
                )
            }
            (None, None, Some(pi), None) => {
                let r = [pi, TI::Index::ZERO, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[pi, TI::Index::MAX, TI::Index::MAX, TI::Index::MAX];
                BcdMatchingIterator::boxed(
                    &self.terms,
                    self.posg.range(r),
                    om.gn(),
                    sm.gn(),
                    gm,
                    |[p, o, s, g]| [g, s, p, o],
                )
            }
            (None, None, None, Some(oi)) => {
                let r = [oi, TI::Index::ZERO, TI::Index::ZERO, TI::Index::ZERO]
                    ..=[oi, TI::Index::MAX, TI::Index::MAX, TI::Index::MAX];
                BcdMatchingIterator::boxed(
                    &self.terms,
                    self.ospg.range(r),
                    sm.gn(),
                    pm.gn(),
                    gm,
                    |[o, s, p, g]| [g, s, p, o],
                )
            }
            (None, None, None, None) => {
                GspoMatchingIterator::boxed(&self.terms, self.gspo.iter(), gm, sm, pm, om)
            }
        }
    }
}

impl<TI: GraphNameIndex> MutableDataset for GenericFastDataset<TI> {
    type MutationError = TI::Error;

    fn insert<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> sophia_api::dataset::MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
    {
        let is = self.terms.ensure_index(s)?;
        let ip = self.terms.ensure_index(p)?;
        let io = self.terms.ensure_index(o)?;
        let ig = match g {
            None => self.terms.get_default_graph_index(),
            Some(gn) => self.terms.ensure_index(gn)?,
        };
        if self.gspo.insert([ig, is, ip, io]) {
            let i = self.gpos.insert([ig, ip, io, is]);
            debug_assert!(i);
            let i = self.gosp.insert([ig, io, is, ip]);
            debug_assert!(i);
            let i = self.spog.insert([is, ip, io, ig]);
            debug_assert!(i);
            let i = self.posg.insert([ip, io, is, ig]);
            debug_assert!(i);
            let i = self.ospg.insert([io, is, ip, ig]);
            debug_assert!(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn remove<TS, TP, TO, TG>(
        &mut self,
        s: TS,
        p: TP,
        o: TO,
        g: GraphName<TG>,
    ) -> sophia_api::dataset::MdResult<Self, bool>
    where
        TS: Term,
        TP: Term,
        TO: Term,
        TG: Term,
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
        let Some(ig) = self.terms.get_graph_name_index(g) else {
            return Ok(false);
        };
        if self.gspo.remove(&[ig, is, ip, io]) {
            let i = self.gpos.remove(&[ig, ip, io, is]);
            debug_assert!(i);
            let i = self.gosp.remove(&[ig, io, is, ip]);
            debug_assert!(i);
            let i = self.spog.remove(&[is, ip, io, ig]);
            debug_assert!(i);
            let i = self.posg.remove(&[ip, io, is, ig]);
            debug_assert!(i);
            let i = self.ospg.remove(&[io, is, ip, ig]);
            debug_assert!(i);
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

impl<TI: GraphNameIndex + Default> CollectibleDataset for GenericFastDataset<TI> {
    fn from_quad_source<QS: QuadSource>(
        mut quads: QS,
    ) -> sophia_api::source::StreamResult<Self, QS::Error, Self::Error> {
        let mut d = Self::new();
        quads.try_for_each_quad(|q| d.insert_quad(q).map(|_| ()))?;
        Ok(d)
    }
}

impl<TI: GraphNameIndex> SetDataset for GenericFastDataset<TI> {}

/// A dataset with a single quad index (GSPO).
/// Fast to load but slow on some queries, with a relatively low memory footprint.
///
/// Default configuration of [`GenericLightDataset`].
pub type LightDataset = GenericLightDataset<SimpleTermIndex<u32>>;

/// A heavily indexed dataset.
/// Fast to query but slow to load, with a relatively high memory footprint.
///
/// Default configuration of [`GenericFastDataset`].
pub type FastDataset = GenericFastDataset<SimpleTermIndex<u32>>;

#[cfg(test)]
mod test {
    use super::{FastDataset, LightDataset};

    sophia_api::test_dataset_impl!(light_dataset, LightDataset);
    sophia_api::test_dataset_impl!(fast_dataset, FastDataset);

    #[test]
    fn new_available() {
        // ::new() is only available if the underlying TermIndex implements Default,
        // so let's check that FastDataset and LightDataset do have it.
        let _ = FastDataset::new();
        let _ = LightDataset::new();
    }
}

/// Flavors of Dataset implementations with a smaller memory-footprint.
///
/// The trade-off is that these implementations can only contain a small number (2^16) of distinct terms.
///
pub mod small {
    use crate::index::SimpleTermIndex;

    /// A dataset with a single quad index (GSPO).
    /// Fast to load but slow to query, with a relatively low memory footprint.
    pub type LightDataset = super::GenericLightDataset<SimpleTermIndex<u16>>;
    /// A heavily indexed dataset.
    /// Fast to query but slow to load, with a relatively high memory footprint.
    pub type FastDataset = super::GenericFastDataset<SimpleTermIndex<u16>>;

    #[cfg(all(test, feature = "all_tests"))]
    mod test {
        use super::*;
        sophia_api::test_dataset_impl!(fast_dataset, FastDataset);
        sophia_api::test_dataset_impl!(light_dataset, LightDataset);
    }
}
