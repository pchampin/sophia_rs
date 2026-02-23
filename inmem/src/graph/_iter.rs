use std::collections::btree_set::{Iter as BTreeSetIter, Range};
use std::convert::Infallible;
use std::iter::empty;

use sophia_api::term::Term;
use sophia_api::term::matcher::TermMatcher;

use crate::index::TermIndex;

pub struct SpoMatchingIterator<'a, TI, SM, PM, OM>
where
    TI: TermIndex + 'a,
    SM: TermMatcher,
    PM: TermMatcher,
    OM: TermMatcher,
{
    terms: &'a TI,
    spo: BTreeSetIter<'a, [TI::Index; 3]>,
    s: TermData<'a, TI, SM>,
    p: TermData<'a, TI, PM>,
    o: TermData<'a, TI, OM>,
}

impl<'a, 'b, TI, SM, PM, OM> SpoMatchingIterator<'a, TI, SM, PM, OM>
where
    'a: 'b,
    TI: TermIndex + 'a,
    SM: TermMatcher + 'b,
    PM: TermMatcher + 'b,
    OM: TermMatcher + 'b,
{
    pub fn boxed(
        terms: &'a TI,
        spo: BTreeSetIter<'a, [TI::Index; 3]>,
        sm: SM,
        pm: PM,
        om: OM,
    ) -> Box<dyn Iterator<Item = Result<Trpl<'a, TI>, Infallible>> + 'b> {
        match spo.clone().next() {
            None => Box::new(empty()),
            Some(first) => Box::new(Self::new(terms, spo, sm, pm, om, first).map(Ok)),
        }
    }

    fn new(
        terms: &'a TI,
        spo: BTreeSetIter<'a, [TI::Index; 3]>,
        sm: SM,
        pm: PM,
        om: OM,
        first: &[TI::Index; 3],
    ) -> Self {
        let [si, pi, oi] = *first;
        let s = TermData::new(sm, si, terms);
        let p = TermData::new(pm, pi, terms);
        let o = TermData::uninit(om, oi, terms);
        Self {
            terms,
            spo,
            s,
            p,
            o,
        }
    }
}

type Trpl<'a, TI> = [<TI as TermIndex>::Term<'a>; 3];

impl<'a, TI, SM, PM, OM> Iterator for SpoMatchingIterator<'a, TI, SM, PM, OM>
where
    TI: TermIndex + 'a,
    SM: TermMatcher,
    PM: TermMatcher,
    OM: TermMatcher,
{
    type Item = Trpl<'a, TI>;

    fn next(&mut self) -> Option<Self::Item> {
        let [si, pi, oi] = *self.spo.next()?;

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
            Some([self.s.t, self.p.t, self.o.t])
        }
    }
}

//

/// An iterator over a range of index-triples 'abc',
/// where all triples are assumed to have the same first element 'a'.
pub struct BcMatchingIterator<'a, TI, BM, CM>
where
    TI: TermIndex + 'a,
    BM: TermMatcher,
    CM: TermMatcher,
{
    terms: &'a TI,
    abc: Range<'a, [TI::Index; 3]>,
    a: TI::Term<'a>,
    b: TermData<'a, TI, BM>,
    c: TermData<'a, TI, CM>,
}

impl<'a, 'b, TI, BM, CM> BcMatchingIterator<'a, TI, BM, CM>
where
    'a: 'b,
    TI: TermIndex + 'a,
    BM: TermMatcher + 'b,
    CM: TermMatcher + 'b,
{
    pub fn boxed<F>(
        terms: &'a TI,
        abc: Range<'a, [TI::Index; 3]>,
        bm: BM,
        cm: CM,
        mut to_spo: F,
    ) -> Box<dyn Iterator<Item = Result<Trpl<'a, TI>, Infallible>> + 'b>
    where
        F: FnMut(Trpl<'a, TI>) -> Trpl<'a, TI> + 'b,
    {
        match abc.clone().next() {
            None => Box::new(empty()),
            Some(first) => {
                Box::new(Self::new(terms, abc, bm, cm, first).map(move |t| Ok(to_spo(t))))
            }
        }
    }

    fn new(
        terms: &'a TI,
        abc: Range<'a, [TI::Index; 3]>,
        bm: BM,
        cm: CM,
        first: &[TI::Index; 3],
    ) -> Self {
        let [ai, bi, ci] = *first;
        let a = terms.get_term(ai);
        let b = TermData::new(bm, bi, terms);
        let c = TermData::uninit(cm, ci, terms);
        Self {
            terms,
            abc,
            a,
            b,
            c,
        }
    }
}

impl<'a, TI, BM, CM> Iterator for BcMatchingIterator<'a, TI, BM, CM>
where
    TI: TermIndex + 'a,
    BM: TermMatcher,
    CM: TermMatcher,
{
    type Item = Trpl<'a, TI>;

    fn next(&mut self) -> Option<Self::Item> {
        let [ai, bi, ci] = *self.abc.next()?;
        debug_assert!(Term::eq(&self.terms.get_term(ai), self.a));

        if bi != self.b.i {
            self.b.update(bi, self.terms);
        }
        if !self.b.b {
            return self.next();
        }

        self.c.update(ci, self.terms);
        if !self.c.b {
            self.next()
        } else {
            Some([self.a, self.b.t, self.c.t])
        }
    }
}

//

pub struct TermData<'a, TI, M>
where
    TI: TermIndex + 'a,
    M: TermMatcher,
{
    pub m: M,
    pub i: TI::Index,
    pub t: TI::Term<'a>,
    pub b: bool,
}

impl<'a, TI, M> TermData<'a, TI, M>
where
    TI: TermIndex + 'a,
    M: TermMatcher,
{
    pub fn uninit(m: M, i: TI::Index, terms: &'a TI) -> Self {
        let t = terms.get_term(i);
        let b = true;
        Self { m, i, t, b }
    }

    pub fn new(m: M, i: TI::Index, terms: &'a TI) -> Self {
        let t = terms.get_term(i);
        let b = m.matches(&t);
        Self { m, i, t, b }
    }

    pub fn update(&mut self, i: TI::Index, terms: &'a TI) {
        self.i = i;
        self.t = terms.get_term(i);
        self.b = self.m.matches(&self.t);
    }
}
