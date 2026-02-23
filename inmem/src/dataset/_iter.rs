use std::collections::btree_set::{Iter as BTreeSetIter, Range};
use std::convert::Infallible;
use std::iter::empty;

use sophia_api::quad::Gspo;
use sophia_api::term::matcher::{GraphNameMatcher, TermMatcher};
use sophia_api::term::{GraphName, graph_name_eq};

use crate::index::{GraphNameIndex, TermIndex};

pub struct GspoMatchingIterator<'a, TI, GM, SM, PM, OM>
where
    TI: TermIndex + 'a,
    GM: GraphNameMatcher,
    SM: TermMatcher,
    PM: TermMatcher,
    OM: TermMatcher,
{
    terms: &'a TI,
    gspo: BTreeSetIter<'a, [TI::Index; 4]>,
    g: GraphNameData<'a, TI, GM>,
    s: TermData<'a, TI, SM>,
    p: TermData<'a, TI, PM>,
    o: TermData<'a, TI, OM>,
}

impl<'a, 'b, TI, GM, SM, PM, OM> GspoMatchingIterator<'a, TI, GM, SM, PM, OM>
where
    'a: 'b,
    TI: GraphNameIndex + 'a,
    GM: GraphNameMatcher + 'b,
    SM: TermMatcher + 'b,
    PM: TermMatcher + 'b,
    OM: TermMatcher + 'b,
{
    pub fn boxed(
        terms: &'a TI,
        gspo: BTreeSetIter<'a, [TI::Index; 4]>,
        gm: GM,
        sm: SM,
        pm: PM,
        om: OM,
    ) -> Box<dyn Iterator<Item = Result<Qud<'a, TI>, Infallible>> + 'b> {
        match gspo.clone().next() {
            None => Box::new(empty()),
            Some(first) => Box::new(Self::new(terms, gspo, gm, sm, pm, om, first).map(Ok)),
        }
    }

    fn new(
        terms: &'a TI,
        gspo: BTreeSetIter<'a, [TI::Index; 4]>,
        gm: GM,
        sm: SM,
        pm: PM,
        om: OM,
        first: &[TI::Index; 4],
    ) -> Self {
        let [gi, si, pi, oi] = *first;
        let g = GraphNameData::new(gm, gi, terms);
        let s = TermData::new(sm, si, terms);
        let p = TermData::new(pm, pi, terms);
        let o = TermData::uninit(om, oi, terms);
        Self {
            terms,
            gspo,
            g,
            s,
            p,
            o,
        }
    }
}

pub type Qud<'a, TI> = Gspo<<TI as TermIndex>::Term<'a>>;

impl<'a, TI, GM, SM, PM, OM> Iterator for GspoMatchingIterator<'a, TI, GM, SM, PM, OM>
where
    TI: GraphNameIndex + 'a,
    GM: GraphNameMatcher,
    SM: TermMatcher,
    PM: TermMatcher,
    OM: TermMatcher,
{
    type Item = Qud<'a, TI>;

    fn next(&mut self) -> Option<Self::Item> {
        let [gi, si, pi, oi] = *self.gspo.next()?;

        if gi != self.g.i {
            self.g.update(gi, self.terms);
        }
        if !self.g.b {
            return self.next();
        }

        if si != self.s.i {
            self.s.update(si, self.terms);
        }
        if !self.s.b {
            return self.next();
        }

        if pi != self.p.i {
            self.p.update(pi, self.terms);
        }
        if !self.p.b {
            return self.next();
        }

        self.o.update(oi, self.terms);
        if !self.o.b {
            self.next()
        } else {
            Some((self.g.t, [self.s.t, self.p.t, self.o.t]))
        }
    }
}

//

/// An iterator over a range of index-quads 'abcd',
/// where all quads are assumed to have the same first element 'a'.
pub struct BcdMatchingIterator<'a, TI, BM, CM, DM>
where
    TI: GraphNameIndex + 'a,
    BM: GraphNameMatcher,
    CM: GraphNameMatcher,
    DM: GraphNameMatcher,
{
    terms: &'a TI,
    abcd: Range<'a, [TI::Index; 4]>,
    a: GraphName<TI::Term<'a>>,
    b: GraphNameData<'a, TI, BM>,
    c: GraphNameData<'a, TI, CM>,
    d: GraphNameData<'a, TI, DM>,
}

impl<'a, 'b, TI, BM, CM, DM> BcdMatchingIterator<'a, TI, BM, CM, DM>
where
    'a: 'b,
    TI: GraphNameIndex + 'a,
    BM: GraphNameMatcher + 'b,
    CM: GraphNameMatcher + 'b,
    DM: GraphNameMatcher + 'b,
{
    pub fn boxed<F>(
        terms: &'a TI,
        abcd: Range<'a, [TI::Index; 4]>,
        bm: BM,
        cm: CM,
        dm: DM,
        mut to_gspo: F,
    ) -> Box<dyn Iterator<Item = Result<Qud<'a, TI>, Infallible>> + 'b>
    where
        F: FnMut(GnQuad<'a, TI>) -> GnQuad<'a, TI> + 'a,
    {
        match abcd.clone().next() {
            None => Box::new(empty()),
            Some(first) => {
                Box::new(Self::new(terms, abcd, bm, cm, dm, first).map(move |q| {
                    let [g, s, p, o] = to_gspo(q);
                    debug_assert!(s.is_some());
                    debug_assert!(p.is_some());
                    debug_assert!(o.is_some());
                    // the following is safe, because s, p and o are never None
                    let s = unsafe { s.unwrap_unchecked() };
                    let p = unsafe { p.unwrap_unchecked() };
                    let o = unsafe { o.unwrap_unchecked() };
                    Ok((g, [s, p, o]))
                }))
            }
        }
    }

    fn new(
        terms: &'a TI,
        abcd: Range<'a, [TI::Index; 4]>,
        bm: BM,
        cm: CM,
        dm: DM,
        first: &[TI::Index; 4],
    ) -> Self {
        let [ai, bi, ci, di] = *first;
        let a = terms.get_graph_name(ai);
        let b = GraphNameData::new(bm, bi, terms);
        let c = GraphNameData::new(cm, ci, terms);
        let d = GraphNameData::uninit(dm, di, terms);
        Self {
            terms,
            abcd,
            a,
            b,
            c,
            d,
        }
    }
}

type GnQuad<'a, TI> = [GraphName<<TI as TermIndex>::Term<'a>>; 4];

impl<'a, TI, BM, CM, DM> Iterator for BcdMatchingIterator<'a, TI, BM, CM, DM>
where
    TI: GraphNameIndex + 'a,
    BM: GraphNameMatcher,
    CM: GraphNameMatcher,
    DM: GraphNameMatcher,
{
    type Item = GnQuad<'a, TI>;

    fn next(&mut self) -> Option<Self::Item> {
        let [ai, bi, ci, di] = *self.abcd.next()?;
        debug_assert!(graph_name_eq(self.terms.get_graph_name(ai), self.a));

        if bi != self.b.i {
            self.b.update(bi, self.terms);
        }
        if !self.b.b {
            return self.next();
        }

        if ci != self.c.i {
            self.c.update(ci, self.terms);
        }
        if !self.c.b {
            return self.next();
        }

        self.d.update(di, self.terms);
        if !self.d.b {
            self.next()
        } else {
            Some([self.a, self.b.t, self.c.t, self.d.t])
        }
    }
}

//

/// An iterator over a range of index-quads 'abcd',
/// where all quads are assumed to have the same first and second elements 'a' and 'b'.
pub struct CdMatchingIterator<'a, TI, CM, DM>
where
    TI: GraphNameIndex + 'a,
    CM: GraphNameMatcher,
    DM: GraphNameMatcher,
{
    terms: &'a TI,
    abcd: Range<'a, [TI::Index; 4]>,
    a: GraphName<TI::Term<'a>>,
    b: GraphName<TI::Term<'a>>,
    c: GraphNameData<'a, TI, CM>,
    d: GraphNameData<'a, TI, DM>,
}

impl<'a, 'b, TI, CM, DM> CdMatchingIterator<'a, TI, CM, DM>
where
    'a: 'b,
    TI: GraphNameIndex + 'a,
    CM: GraphNameMatcher + 'b,
    DM: GraphNameMatcher + 'b,
{
    pub fn boxed<F>(
        terms: &'a TI,
        abcd: Range<'a, [TI::Index; 4]>,
        cm: CM,
        dm: DM,
        mut to_gspo: F,
    ) -> Box<dyn Iterator<Item = Result<Qud<'a, TI>, Infallible>> + 'b>
    where
        F: FnMut(GnQuad<'a, TI>) -> GnQuad<'a, TI> + 'a,
    {
        match abcd.clone().next() {
            None => Box::new(empty()),
            Some(first) => Box::new(Self::new(terms, abcd, cm, dm, first).map(move |q| {
                let [g, s, p, o] = to_gspo(q);
                debug_assert!(s.is_some());
                debug_assert!(p.is_some());
                debug_assert!(o.is_some());
                // the following is safe, because s, p and o are never None
                let s = unsafe { s.unwrap_unchecked() };
                let p = unsafe { p.unwrap_unchecked() };
                let o = unsafe { o.unwrap_unchecked() };
                Ok((g, [s, p, o]))
            })),
        }
    }

    fn new(
        terms: &'a TI,
        abcd: Range<'a, [TI::Index; 4]>,
        cm: CM,
        dm: DM,
        first: &[TI::Index; 4],
    ) -> Self {
        let [ai, bi, ci, di] = *first;
        let a = terms.get_graph_name(ai);
        let b = terms.get_graph_name(bi);
        let c = GraphNameData::new(cm, ci, terms);
        let d = GraphNameData::uninit(dm, di, terms);
        Self {
            terms,
            abcd,
            a,
            b,
            c,
            d,
        }
    }
}

impl<'a, TI, CM, DM> Iterator for CdMatchingIterator<'a, TI, CM, DM>
where
    TI: GraphNameIndex + 'a,
    CM: GraphNameMatcher,
    DM: GraphNameMatcher,
{
    type Item = GnQuad<'a, TI>;

    fn next(&mut self) -> Option<Self::Item> {
        let [ai, bi, ci, di] = *self.abcd.next()?;
        debug_assert!(graph_name_eq(self.terms.get_graph_name(ai), self.a));
        debug_assert!(graph_name_eq(self.terms.get_graph_name(bi), self.b));

        if ci != self.c.i {
            self.c.update(ci, self.terms);
        }
        if !self.c.b {
            return self.next();
        }

        self.d.update(di, self.terms);
        if !self.d.b {
            self.next()
        } else {
            Some([self.a, self.b, self.c.t, self.d.t])
        }
    }
}

//

use crate::graph::TermData;

struct GraphNameData<'a, TI, M>
where
    TI: TermIndex + 'a,
    M: GraphNameMatcher,
{
    m: M,
    i: TI::Index,
    t: GraphName<TI::Term<'a>>,
    b: bool,
}

impl<'a, TI, M> GraphNameData<'a, TI, M>
where
    TI: GraphNameIndex + 'a,
    M: GraphNameMatcher,
{
    fn uninit(m: M, i: TI::Index, terms: &'a TI) -> Self {
        let t = terms.get_graph_name(i);
        let b = true;
        Self { m, i, t, b }
    }

    fn new(m: M, i: TI::Index, terms: &'a TI) -> Self {
        let t = terms.get_graph_name(i);
        let b = m.matches(t.as_ref());
        Self { m, i, t, b }
    }

    fn update(&mut self, i: TI::Index, terms: &'a TI) {
        self.i = i;
        self.t = terms.get_graph_name(i);
        self.b = self.m.matches(self.t.as_ref());
    }
}
