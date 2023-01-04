//! I define generic traits and default implementations for *matchers*,
//! objects that can be used to match zero, one or several terms.
//!
//! For a list of matcher implementations,
//! check [`TermMarcher`'s ](TermMatcher#foreign-impls) and
//! [`GraphNameMatcher`'s implementors lists](GraphNameMatcher#foreign-impls).
//!
//! For methods using matchers (with examples), see for example
//! [`Triple::matched_by`](crate::triple::Triple::matched_by),
//! [`Graph::triples_matching`](crate::graph::Graph::triples_matching),
//! [`MutableGraph::remove_matching`](crate::graph::MutableGraph::remove_matching),
//! [`MutableGraph::retain_matching`](crate::graph::MutableGraph::retain_matching),
//! [`Dataset::quads_matching`](crate::dataset::Dataset::quads_matching),
//! [`MutableDataset::remove_matching`](crate::dataset::MutableDataset::remove_matching),
//! [`MutableDataset::retain_matching`](crate::dataset::MutableDataset::retain_matching).

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

    /// Convert this [`TermMatcher`] into a [`GraphNameMatcher`]
    fn gn(&self) -> TermMatcherGn<Self> {
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
        (self)(SimpleTerm::from_term_ref(term))
    }
}

#[derive(Clone, Copy, Debug)]
/// A universal matcher: it matches any [`Term`] or [`GraphName`] (even the default graph).
pub struct Any;

impl TermMatcher for Any {
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, _: &T2) -> bool {
        true
    }
}

/// Result type of [`TermMatcher::matcher_ref`] and [`GraphNameMatcher::matcher_ref`].
#[derive(Debug)]
pub struct MatcherRef<'a, T: ?Sized>(&'a T);

impl<'a, T> Clone for MatcherRef<'a, T> {
    fn clone(&self) -> Self {
        MatcherRef(self.0)
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

// ================ GraphNameMatcher ================

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
        if N == 1 {
            Some(self[0].as_ref())
        } else {
            None
        }
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

impl<F> GraphNameMatcher for F
where
    F: Fn(GraphName<SimpleTerm>) -> bool + ?Sized,
{
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, graph_name: GraphName<&T2>) -> bool {
        (self)(graph_name.map(SimpleTerm::from_term_ref))
    }
}

impl GraphNameMatcher for Any {
    type Term = SimpleTerm<'static>;

    fn matches<T2: Term + ?Sized>(&self, _: GraphName<&T2>) -> bool {
        true
    }
}

/// Wrapper type returned by [`TermMatcher::gn`]
#[derive(Clone, Copy, Debug)]
pub struct TermMatcherGn<'a, T: ?Sized>(&'a T);

impl<M> GraphNameMatcher for TermMatcherGn<'_, M>
where
    M: TermMatcher + ?Sized,
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

#[cfg(test)]
mod test {
    use super::*;
    use sophia_iri::IriRef;

    const T1: IriRef<&str> = IriRef::new_unchecked_const("tag:t1");
    const T2: IriRef<&str> = IriRef::new_unchecked_const("tag:t2");
    const T3: IriRef<&str> = IriRef::new_unchecked_const("tag:t3");

    fn is_term_matcher<M: TermMatcher>(_: M) {}

    #[allow(dead_code)] // just check this compiles
    fn check_term_macher_implementations() {
        is_term_matcher(Any);
        is_term_matcher(Some(T1));
        is_term_matcher([T1, T2]);
        is_term_matcher(&[T1, T2][..]);
        is_term_matcher(|t: SimpleTerm| t != T1);
        is_term_matcher(TermKind::Iri);
        is_term_matcher(([T1], [T2], [T3]));
        is_term_matcher([T1, T2].matcher_ref());
    }

    fn is_graph_name_matcher<M: GraphNameMatcher>(_: M) {}

    #[allow(dead_code)] // just check this compiles
    fn check_graph_name_macher_implementations() {
        is_graph_name_matcher(Any);
        is_graph_name_matcher(Some(Some(T1)));
        is_graph_name_matcher([Some(T1), Some(T2), None]);
        is_graph_name_matcher(&[Some(T1), Some(T2), None][..]);
        is_graph_name_matcher(|t: Option<SimpleTerm>| t.is_some());
        is_graph_name_matcher([T1, T2].gn());
        is_graph_name_matcher(Some(TermKind::Iri));
        is_graph_name_matcher(Some(([T1], [T2], [T3])));
        is_graph_name_matcher([Some(T1)].matcher_ref());
    }

    #[test]
    fn option() {
        let none: Option<IriRef<&str>> = None;
        assert!(!none.matches(&T1));
        assert!(!none.matches(&T2));
        assert!(!none.matches(&T3));
        assert_eq!(none.constant(), None);

        let some = Some(T1);
        assert!(some.matches(&T1));
        assert!(!some.matches(&T2));
        assert!(!none.matches(&T3));
        assert_eq!(some.constant(), Some(&T1));
    }

    #[test]
    fn array() {
        let a0: [IriRef<&str>; 0] = [];
        assert!(!a0.matches(&T1));
        assert!(!a0.matches(&T2));
        assert!(!a0.matches(&T3));
        assert_eq!(a0.constant(), None);

        let a1 = [T1];
        assert!(a1.matches(&T1));
        assert!(!a1.matches(&T2));
        assert!(!a1.matches(&T3));
        assert_eq!(a1.constant(), Some(&T1));

        let a2 = [T1, T2];
        assert!(a2.matches(&T1));
        assert!(a2.matches(&T2));
        assert!(!a2.matches(&T3));
        assert_eq!(a2.constant(), None);

        let a3 = [T1, T2, T3];
        assert!(a3.matches(&T1));
        assert!(a3.matches(&T2));
        assert!(a3.matches(&T3));
        assert_eq!(a3.constant(), None);
    }

    #[test]
    fn slice() {
        let a0: [IriRef<&str>; 0] = [];
        let s0 = &a0[..];
        assert!(!s0.matches(&T1));
        assert!(!s0.matches(&T2));
        assert!(!s0.matches(&T3));
        assert_eq!(s0.constant(), None);

        let a1 = [T1];
        let s1 = &a1[..];
        assert!(s1.matches(&T1));
        assert!(!s1.matches(&T2));
        assert!(!s1.matches(&T3));
        assert_eq!(s1.constant(), Some(&T1));

        let a2 = [T1, T2];
        let s2 = &a2[..];
        assert!(s2.matches(&T1));
        assert!(s2.matches(&T2));
        assert!(!s2.matches(&T3));
        assert_eq!(s2.constant(), None);

        let a3 = [T1, T2, T3];
        let s3 = &a3[..];
        assert!(s3.matches(&T1));
        assert!(s3.matches(&T2));
        assert!(s3.matches(&T3));
        assert_eq!(s3.constant(), None);
    }

    #[test]
    fn term_kind() {
        assert!(TermKind::Iri.matches(&T1));
        assert!(!TermKind::BlankNode.matches(&T1));
    }

    #[test]
    fn tuple_as_embedded_triple() {
        let et = SimpleTerm::Triple(Box::new([T1, T2, T3].map(SimpleTerm::from_term)));
        assert!(([T1], TermKind::Iri, Any).matches(&et));
        assert!(!([T2], TermKind::Iri, Any).matches(&et));
        assert!(!([T1], TermKind::BlankNode, Any).matches(&et));
        assert!(!([T1], TermKind::Iri, [T1]).matches(&et));
    }

    #[test]
    fn closure() {
        let c = |t: SimpleTerm| t != T1;
        assert!(!TermMatcher::matches(&c, &T1));
        assert!(TermMatcher::matches(&c, &T2));
        assert!(TermMatcher::matches(&c, &T3));
        assert!(TermMatcher::constant(&c).is_none());
    }

    #[test]
    fn any() {
        assert!(TermMatcher::matches(&Any, &T1));
        assert!(TermMatcher::matches(&Any, &T2));
        assert!(TermMatcher::matches(&Any, &T3));
        assert!(TermMatcher::constant(&Any).is_none());
    }

    #[test]
    fn matcher_ref() {
        let c = [T1].matcher_ref();
        assert!(c.matches(&T1));
        assert!(!c.matches(&T2));
        assert!(!c.matches(&T3));
        assert_eq!(c.constant(), Some(&T1));
    }

    const DEFAULT: Option<&IriRef<&str>> = None;

    #[test]
    fn graph_name_option() {
        let none: Option<Option<IriRef<&str>>> = None;
        assert!(!none.matches(DEFAULT));
        assert!(!none.matches(Some(&T1)));
        assert!(!none.matches(Some(&T2)));
        assert!(!none.matches(Some(&T3)));
        assert_eq!(none.constant(), None);

        let some = Some(Some(T1));
        assert!(!some.matches(DEFAULT));
        assert!(some.matches(Some(&T1)));
        assert!(!some.matches(Some(&T2)));
        assert!(!none.matches(Some(&T3)));
        assert_eq!(some.constant(), Some(Some(&T1)));
    }

    #[test]
    fn graph_name_array() {
        let a0: [Option<&IriRef<&str>>; 0] = [];
        assert!(!a0.matches(DEFAULT));
        assert!(!a0.matches(Some(&T1)));
        assert!(!a0.matches(Some(&T2)));
        assert!(!a0.matches(Some(&T3)));
        assert_eq!(a0.constant(), None);

        let a1 = [Some(T1)];
        assert!(!a1.matches(DEFAULT));
        assert!(a1.matches(Some(&T1)));
        assert!(!a1.matches(Some(&T2)));
        assert!(!a1.matches(Some(&T3)));
        assert_eq!(a1.constant(), Some(Some(&T1)));

        let a2 = [Some(T1), None];
        assert!(a2.matches(DEFAULT));
        assert!(a2.matches(Some(&T1)));
        assert!(!a2.matches(Some(&T2)));
        assert!(!a2.matches(Some(&T3)));
        assert_eq!(a2.constant(), None);

        let a3 = [Some(T1), None, Some(T3)];
        assert!(a3.matches(DEFAULT));
        assert!(a3.matches(Some(&T1)));
        assert!(!a3.matches(Some(&T2)));
        assert!(a3.matches(Some(&T3)));
        assert_eq!(a3.constant(), None);
    }

    #[test]
    fn graph_name_slice() {
        let a0: [Option<&IriRef<&str>>; 0] = [];
        let s0 = &a0[..];
        assert!(!s0.matches(DEFAULT));
        assert!(!s0.matches(Some(&T1)));
        assert!(!s0.matches(Some(&T2)));
        assert!(!s0.matches(Some(&T3)));
        assert_eq!(s0.constant(), None);

        let a1 = [Some(T1)];
        let s1 = &a1[..];
        assert!(!s1.matches(DEFAULT));
        assert!(s1.matches(Some(&T1)));
        assert!(!s1.matches(Some(&T2)));
        assert!(!s1.matches(Some(&T3)));
        assert_eq!(s1.constant(), Some(Some(&T1)));

        let a2 = [Some(T1), None];
        let s2 = &a2[..];
        assert!(s2.matches(DEFAULT));
        assert!(s2.matches(Some(&T1)));
        assert!(!s2.matches(Some(&T2)));
        assert!(!s2.matches(Some(&T3)));
        assert_eq!(s2.constant(), None);

        let a3 = [Some(T1), None, Some(T3)];
        let s3 = &a3[..];
        assert!(s3.matches(DEFAULT));
        assert!(s3.matches(Some(&T1)));
        assert!(!s3.matches(Some(&T2)));
        assert!(s3.matches(Some(&T3)));
        assert_eq!(s3.constant(), None);
    }

    #[test]
    fn graph_name_term_kind() {
        assert!(Some(TermKind::Iri).matches(Some(&T1)));
        assert!(!Some(TermKind::BlankNode).matches(Some(&T1)));
    }

    #[test]
    fn graph_name_tuple_as_embedded_triple() {
        let et = SimpleTerm::Triple(Box::new([T1, T2, T3].map(SimpleTerm::from_term)));
        let gn = Some(&et);
        assert!(Some(([T1], TermKind::Iri, Any)).matches(gn));
        assert!(!Some(([T2], TermKind::Iri, Any)).matches(gn));
        assert!(!Some(([T1], TermKind::BlankNode, Any)).matches(gn));
        assert!(!Some(([T1], TermKind::Iri, [T1])).matches(gn));
    }

    #[test]
    fn graph_name_closure() {
        let c = |t: Option<SimpleTerm>| !graph_name_eq(t, Some(&T1));
        assert!(GraphNameMatcher::matches(&c, DEFAULT));
        assert!(!GraphNameMatcher::matches(&c, Some(&T1)));
        assert!(GraphNameMatcher::matches(&c, Some(&T2)));
        assert!(GraphNameMatcher::matches(&c, Some(&T3)));
        assert!(GraphNameMatcher::constant(&c).is_none());
    }

    #[test]
    fn graph_name_any() {
        assert!(GraphNameMatcher::matches(&Any, DEFAULT));
        assert!(GraphNameMatcher::matches(&Any, Some(&T1)));
        assert!(GraphNameMatcher::matches(&Any, Some(&T2)));
        assert!(GraphNameMatcher::matches(&Any, Some(&T3)));
        assert!(GraphNameMatcher::constant(&Any).is_none());
    }

    #[test]
    fn graph_name_term_matcher_gn() {
        let a1 = [T1].gn();
        assert!(!a1.matches(DEFAULT));
        assert!(a1.matches(Some(&T1)));
        assert!(!a1.matches(Some(&T2)));
        assert!(!a1.matches(Some(&T3)));
        assert_eq!(a1.constant(), Some(Some(&T1)));

        let a2 = [T1, T2].gn();
        assert!(!a2.matches(DEFAULT));
        assert!(a2.matches(Some(&T1)));
        assert!(a2.matches(Some(&T2)));
        assert!(!a2.matches(Some(&T3)));
        assert_eq!(a2.constant(), None);
    }

    #[test]
    fn graph_name_matcher_ref() {
        let c = [Some(T1)].matcher_ref();
        assert!(!c.matches(DEFAULT));
        assert!(c.matches(Some(&T1)));
        assert!(!c.matches(Some(&T2)));
        assert!(!c.matches(Some(&T3)));
        assert_eq!(c.constant(), Some(Some(&T1)));
    }
}
