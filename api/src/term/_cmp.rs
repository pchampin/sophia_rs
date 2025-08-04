use super::*;

/// A wrapper for any term type that ensures comparability
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct CmpTerm<T>(pub T);

crate::impl_term_for_wrapper!(CmpTerm);

impl<T1: Term, T2: Term> PartialEq<T2> for CmpTerm<T1> {
    fn eq(&self, other: &T2) -> bool {
        Term::eq(&self.0, other.borrow_term())
    }
}

impl<T: Term> Eq for CmpTerm<T> {}

impl<T1: Term, T2: Term> PartialOrd<T2> for CmpTerm<T1> {
    fn partial_cmp(&self, other: &T2) -> Option<Ordering> {
        Some(Term::cmp(&self.0, other.borrow_term()))
    }
}

impl<T: Term> Ord for CmpTerm<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        Term::cmp(&self.0, other.borrow_term())
    }
}

impl<T: Term> std::hash::Hash for CmpTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(&self.0, state)
    }
}

impl<T: Term> AsRef<T> for CmpTerm<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T: Term> std::ops::Deref for CmpTerm<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: Term + FromTerm> FromTerm for CmpTerm<T> {
    fn from_term<U: Term>(term: U) -> Self {
        Self(term.into_term())
    }
}

impl<T: Term + TryFromTerm> TryFromTerm for CmpTerm<T> {
    type Error = T::Error;

    fn try_from_term<U: Term>(term: U) -> Result<Self, Self::Error> {
        term.try_into_term().map(Self)
    }
}
