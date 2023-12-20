use super::*;

/// Generic trait for matching [`Term`]s.
pub trait TermMatcher {
    /// The type of term that this TermMatcher contains
    type Term: Term + ?Sized;

    /// Check whether this matcher matches `t`.
    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool;

    /// Return `None`, unless this matcher can only match a single term,
    /// in which case this method may return that term.
    ///
    /// This method is provided for optimization purposes,
    /// so implementing it is optional.
    fn constant(&self) -> Option<&Self::Term> {
        None
    }

    /// Converts this [`TermMatcher`] into a [`GraphNameMatcher`]
    ///
    /// If you only want to borrow this matcher as a [`GraphNameMatcher`],
    /// call [`gn`](TermMatcher::gn) on the result of [`matcher_ref`](TermMatcher::matcher_ref).
    fn gn(self) -> TermMatcherGn<Self>
    where
        Self: Sized,
    {
        TermMatcherGn(self)
    }

    /// Return a [`TermMatcher`] that is actually just a reference to this one.
    fn matcher_ref(&self) -> MatcherRef<'_, Self> {
        MatcherRef(self)
    }
}

/// Matches the wrapped term if any, otherwise matches nothing.
impl<T> TermMatcher for Option<T>
where
    T: Term,
{
    type Term = T;

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        match self {
            Some(mine) => mine.eq(term.borrow_term()),
            None => false,
        }
    }
    fn constant(&self) -> Option<&Self::Term> {
        self.as_ref()
    }
}

/// Matches any of the terms in the array.
impl<T, const N: usize> TermMatcher for [T; N]
where
    T: Term,
{
    type Term = T;

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        self.iter().any(|mine| mine.eq(term.borrow_term()))
    }
    fn constant(&self) -> Option<&Self::Term> {
        if N == 1 {
            Some(&self[0])
        } else {
            None
        }
    }
}

/// Matches any of the terms in the slice.
impl<T> TermMatcher for &[T]
where
    T: Term,
{
    type Term = T;

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        self.iter().any(|mine| mine.eq(term.borrow_term()))
    }
    fn constant(&self) -> Option<&Self::Term> {
        if self.len() == 1 {
            Some(&self[0])
        } else {
            None
        }
    }
}

/// Matches only embedded triple whose components match the corresponding matchers.
impl<S, P, O> TermMatcher for (S, P, O)
where
    S: TermMatcher,
    P: TermMatcher,
    O: TermMatcher,
{
    type Term = S::Term; // not actually used

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        let (sm, pm, om) = self;
        match term.triple() {
            None => false,
            Some(t) => t.matched_by(sm.matcher_ref(), pm.matcher_ref(), om.matcher_ref()),
        }
    }
}

/// Matches any term if the given kind
impl TermMatcher for TermKind {
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        term.kind() == *self
    }
}

/// Matches any term satisfying the function.
impl<F> TermMatcher for F
where
    F: Fn(SimpleTerm<'_>) -> bool + ?Sized,
{
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        (self)(term.as_simple())
    }
}
