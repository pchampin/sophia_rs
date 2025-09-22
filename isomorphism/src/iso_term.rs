//! I define [`IsoTerm`],
//! a [`Term`] wrapper ensuring that [`Ord`] and [`Eq`]
//! are consistent with [`Term::cmp`] and [`Term::eq`] respectively.
use sophia_api::{impl_term_for_wrapper, term::Term};
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
        Term::eq(&self.0, other.0.borrow_term())
    }
}

impl<T: Term> Eq for IsoTerm<T> {}

impl<T1, T2> PartialOrd<IsoTerm<T1>> for IsoTerm<T2>
where
    T1: Term,
    T2: Term,
{
    fn partial_cmp(&self, other: &IsoTerm<T1>) -> Option<Ordering> {
        Some(Term::cmp(&self.0, other.0.borrow_term()))
    }
}

impl<T: Term> Ord for IsoTerm<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        Term::cmp(&self.0, other.0.borrow_term())
    }
}
