//! I define [`IsoTerm`],
//! a [`Term`] wrapper implementing [`Ord`] and [`Eq`]
//! in a way that is convenient to compute graph/dataset isomorphism.
//!
//! More specifically, all blank nodes are considered equal.
use sophia_api::impl_term_for_wrapper;
use sophia_api::quad::Spog;
use sophia_api::term::{Term, TermKind};
use std::cmp::{Ordering, PartialOrd};

#[derive(Clone, Copy, Debug)]
pub struct IsoTerm<T>(pub(crate) T);

impl_term_for_wrapper!(IsoTerm);

impl<T1, T2> PartialEq<IsoTerm<T1>> for IsoTerm<T2>
where
    T1: Term,
    T2: Term,
{
    fn eq(&self, other: &IsoTerm<T1>) -> bool {
        use TermKind::BlankNode;
        if self.kind() == BlankNode && other.kind() == BlankNode {
            true
        } else {
            Term::eq(&self.0, other.0.borrow_term())
        }
    }
}

impl<T: Term> Eq for IsoTerm<T> {}

impl<T1, T2> PartialOrd<IsoTerm<T1>> for IsoTerm<T2>
where
    T1: Term,
    T2: Term,
{
    fn partial_cmp(&self, other: &IsoTerm<T1>) -> Option<Ordering> {
        use TermKind::BlankNode;
        if self.kind() == BlankNode && other.kind() == BlankNode {
            Some(Ordering::Equal)
        } else {
            Some(Term::cmp(&self.0, other.0.borrow_term()))
        }
    }
}

impl<T: Term> Ord for IsoTerm<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        use TermKind::BlankNode;
        if self.kind() == BlankNode && other.kind() == BlankNode {
            Ordering::Equal
        } else {
            Term::cmp(&self.0, other.0.borrow_term())
        }
    }
}

fn eq_gn<T1, T2>(g1: &Option<IsoTerm<T1>>, g2: &Option<IsoTerm<T2>>) -> bool
where
    T1: Term,
    T2: Term,
{
    match (g1, g2) {
        (None, None) => true,
        (Some(t1), Some(t2)) => t1 == t2,
        _ => false,
    }
}

pub fn eq_triples<T1, T2>((t1, t2): (&[IsoTerm<T1>; 3], &[IsoTerm<T2>; 3])) -> bool
where
    T1: Term,
    T2: Term,
{
    t1[0] == t2[0] && t1[1] == t2[1] && t1[2] == t2[2]
}

pub fn cmp_quads<T1, T2>((q1, q2): (&Spog<IsoTerm<T1>>, &Spog<IsoTerm<T2>>)) -> bool
where
    T1: Term,
    T2: Term,
{
    eq_triples((&q1.0, &q2.0)) && eq_gn(&q1.1, &q2.1)
}
