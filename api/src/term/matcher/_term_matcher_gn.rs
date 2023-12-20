use super::*;

/// Wrapper type returned by [`TermMatcher::gn`]
#[derive(Clone, Copy, Debug)]
pub struct TermMatcherGn<T>(pub(super) T);

impl<M> GraphNameMatcher for TermMatcherGn<M>
where
    M: TermMatcher,
{
    type Term = M::Term;

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        match graph_name {
            Some(term) => self.0.matches(term),
            None => false,
        }
    }
    fn constant(&self) -> Option<GraphName<&Self::Term>> {
        self.0.constant().map(Some)
    }
}

impl<'a, T: GraphNameMatcher + ?Sized> GraphNameMatcher for MatcherRef<'a, T> {
    type Term = T::Term;

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        self.0.matches(graph_name)
    }

    fn constant(&self) -> Option<GraphName<&Self::Term>> {
        self.0.constant()
    }
}
