use super::*;

/// Generic trait for matching [`GraphName`]s.
pub trait GraphNameMatcher {
    /// The type of term that this GraphNameMatcher contains
    type Term: Term + ?Sized;

    /// Check whether this matcher matches `t`.
    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool;

    /// Return `None`, unless this matcher can only match a single graph name,
    /// in which case this method may return that graph name.
    ///
    /// This method is provided for optimization purposes,
    /// so implementing it is optional.
    fn constant(&self) -> Option<GraphName<&Self::Term>> {
        None
    }

    /// Return a [`GraphNameMatcher`] that is actually just a reference to this one.
    fn matcher_ref(&self) -> MatcherRef<'_, Self> {
        MatcherRef(self)
    }
}

impl<T> GraphNameMatcher for Option<Option<T>>
where
    T: Term,
{
    type Term = T;

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        match self {
            Some(mine) => graph_name_eq(
                mine.as_ref().map(|gn| gn.borrow_term()),
                graph_name.map(|gn| gn.borrow_term()),
            ),
            None => false,
        }
    }
    fn constant(&self) -> Option<GraphName<&Self::Term>> {
        self.as_ref().map(GraphName::as_ref)
    }
}

impl<T, const N: usize> GraphNameMatcher for [GraphName<T>; N]
where
    T: Term,
{
    type Term = T;

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        self.iter().any(|mine| {
            graph_name_eq(
                mine.as_ref().map(|gn| gn.borrow_term()),
                graph_name.map(|gn| gn.borrow_term()),
            )
        })
    }
    fn constant(&self) -> Option<GraphName<&Self::Term>> {
        if N == 1 { Some(self[0].as_ref()) } else { None }
    }
}

impl<T> GraphNameMatcher for &[GraphName<T>]
where
    T: Term,
{
    type Term = T;

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        self.iter().any(|mine| {
            graph_name_eq(
                mine.as_ref().map(|gn| gn.borrow_term()),
                graph_name.map(|gn| gn.borrow_term()),
            )
        })
    }
    fn constant(&self) -> Option<GraphName<&Self::Term>> {
        if self.len() == 1 {
            Some(self[0].as_ref())
        } else {
            None
        }
    }
}

impl GraphNameMatcher for Option<TermKind> {
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        graph_name.map(Term::kind) == *self
    }
}

/// Matches only embedded triple whose components match the corresponding matchers.
impl<S, P, O> GraphNameMatcher for Option<(S, P, O)>
where
    S: TermMatcher,
    P: TermMatcher,
    O: TermMatcher,
{
    type Term = S::Term; // not actually used

    fn matches<T2: Term + ?Sized>(&self, graph_name: Option<&T2>) -> bool {
        match (self, graph_name.map(Term::triple)) {
            (None, None) => true,
            (Some((sm, pm, om)), Some(Some(t))) => {
                t.matched_by(sm.matcher_ref(), pm.matcher_ref(), om.matcher_ref())
            }
            _ => false,
        }
    }
}

#[diagnostic::do_not_recommend]
impl<F> GraphNameMatcher for F
where
    F: Fn(GraphName<SimpleTerm>) -> bool + ?Sized,
{
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        (self)(graph_name.map(Term::as_simple))
    }
}

impl GraphNameMatcher for Any {
    type Term = SimpleTerm<'static>;

    fn matches<T2: Term + ?Sized>(&self, _: GraphName<&T2>) -> bool {
        true
    }
}

impl<M: GraphNameMatcher> GraphNameMatcher for Not<M> {
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        !self.0.matches(graph_name)
    }
}
