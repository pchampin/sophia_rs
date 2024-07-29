use super::*;

/// Matches on the inverse of the inner [`Term`] or [`GraphName`]
pub struct Not<M>(pub M);

impl<M: TermMatcher> TermMatcher for Not<M> {
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        !self.0.matches(term)
    }
}
