use super::*;

/// Result type of [`TermMatcher::matcher_ref`] and [`GraphNameMatcher::matcher_ref`].
#[derive(Debug)]
pub struct MatcherRef<'a, T: ?Sized>(pub(super) &'a T);

impl<'a, T> Clone for MatcherRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a, T> Copy for MatcherRef<'a, T> {}

impl<'a, T: TermMatcher + ?Sized> TermMatcher for MatcherRef<'a, T> {
    type Term = T::Term;

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        self.0.matches(term)
    }

    fn constant(&self) -> Option<&Self::Term> {
        self.0.constant()
    }
}
