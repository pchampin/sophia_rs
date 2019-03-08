// this module is transparently re-exported by its sibbling `matcher`

use std::borrow::Borrow;
use std::hash::Hash;

use crate::term::*;

use super::graph_key::GraphKey;

/// A graph-key-matcher is something that can be used
/// to discriminate members of a set of graph keys.
/// 
/// See [`Dataset::quads_matching`](../../dataset/trait.Dataset.html#method.quads_matching),
/// [`MutableDataset::remove_matching`](../../dataset/trait.MutableDataset.html#method.remove_matching),
/// [`MutableDataset::retain`](../../dataset/trait.MutableDataset.html#method.retain).
pub trait GraphKeyMatcher {
    type TermData: Borrow<str> + Clone + Eq + Hash;
    /// If this matcher matches only one graph key, return it, else `None`.
    fn constant(&self) -> Option<&GraphKey<Self::TermData>>;

    /// Check whether this matcher matches `t`.
    fn matches<T> (&self, g: &GraphKey<T>) -> bool
    where T: Borrow<str> + Clone + Eq + Hash;
}

impl<U> GraphKeyMatcher for GraphKey<U>
where
    U: Borrow<str> + Clone + Eq + Hash,
{
    type TermData = U;
    fn constant(&self) -> Option<&GraphKey<Self::TermData>> {
        Some(self)
    }
    fn matches<T> (&self, g: &GraphKey<T>) -> bool
    where
        T: Borrow<str> + Clone + Eq + Hash,
    {
        g==self
    }
}

impl<U> GraphKeyMatcher for Term<U>
where
    U: Borrow<str> + Clone + Eq + Hash,
{
    type TermData = U;
    fn constant(&self) -> Option<&GraphKey<Self::TermData>> {
        Some(self.as_graph_key())
    }
    fn matches<T> (&self, g: &GraphKey<T>) -> bool
    where
        T: Borrow<str> + Clone + Eq + Hash,
    {
        g==self
    }
}

impl<U> GraphKeyMatcher for Option<GraphKey<U>>
where
    U: Borrow<str> + Clone + Eq + Hash,
{
    type TermData = U;
    fn constant(&self) -> Option<&GraphKey<Self::TermData>> {
        self.as_ref()
    }
    fn matches<T> (&self, g: &GraphKey<T>) -> bool
    where
        T: Borrow<str> + Clone + Eq + Hash,
    {
        match self {
            Some(graph_key) => g == graph_key,
            None => true,
        }
    }
}

impl<U> GraphKeyMatcher for Option<Term<U>>
where
    U: Borrow<str> + Clone + Eq + Hash,
{
    type TermData = U;
    fn constant(&self) -> Option<&GraphKey<Self::TermData>> {
        self.as_ref().map(Term::as_graph_key)
    }
    fn matches<T> (&self, g: &GraphKey<T>) -> bool
    where
        T: Borrow<str> + Clone + Eq + Hash,
    {
        match self {
            Some(term) => g == term,
            None => true,
        }
    }
}

impl<M> GraphKeyMatcher for [M]
where
    M: GraphKeyMatcher,
{
    type TermData = M::TermData;
    fn constant(&self) -> Option<&GraphKey<Self::TermData>> {
        if self.len() == 1 { self[0].constant() }
        else { None }
    }
    fn matches<T> (&self, g: &GraphKey<T>) -> bool
    where
        T: Borrow<str> + Clone + Eq + Hash,
    {
        for matcher in self {
            if matcher.matches(g) { return true; }
        }
        false
    }
}

impl<F: Fn(&GraphKey<&str>) -> bool> GraphKeyMatcher for F {
    type TermData = &'static str;
    fn constant(&self) -> Option<&GraphKey<Self::TermData>> {
        None
    }
    fn matches<T> (&self, t: &GraphKey<T>) -> bool
    where
        T: Borrow<str> + Clone + Eq + Hash,
    {
        match t {
            GraphKey::Default => {
                (self)(&GraphKey::Default)
            }
            GraphKey::Name(n) => {
                let n = RefTerm::from(n);
                (self)(n.as_graph_key())
            }
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_graph_key_as_matcher() {
        let m = GraphKey::Name(BoxTerm::new_iri("http://champin.net/#pa").unwrap());
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &n1);
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_term_as_matcher() {
        let m = BoxTerm::new_iri("http://champin.net/#pa").unwrap();
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &n1);
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_some_graph_key_as_matcher() {
        let m = Some(GraphKey::Name(BoxTerm::new_iri("http://champin.net/#pa").unwrap()));
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &n1);
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_none_graph_key_as_matcher() {
        let m: Option<GraphKey<Box<str>>> = None;
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(m.matches(&n2));
    }

    #[test]
    fn test_some_term_as_matcher() {
        let m = Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap());
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &n1);
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_none_term_as_matcher() {
        let m: Option<BoxTerm> = None;
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(m.matches(&n2));
    }

    #[test]
    fn test_vec1_as_matcher() {
        let m = vec![
            GraphKey::Name(BoxTerm::new_iri("http://champin.net/#pa").unwrap())
        ];
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m[..]);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &n1);
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_vec2_as_matcher() {
        let m = vec![
            GraphKey::Name(BoxTerm::new_iri("http://champin.net/#pa").unwrap()),
            GraphKey::Default,
        ];
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_vec0_as_matcher() {
        let m: Vec<GraphKey<Box<str>>> = vec![];
        // comparing to a term using a different holder, and differently cut,
        // to make the test less obvious
        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(!m.matches(&n0));
        assert!(!m.matches(&n1));
        assert!(!m.matches(&n2));
    }


    #[test]
    fn test_func_as_matcher() {
        let m = |t: &GraphKey<&str>| match t.name() {
            None => false,
            Some(t) => t.value().starts_with("http://champin"),
        };

        let n0: GraphKey<&str> = GraphKey::Default;
        let n1 = GraphKey::Name(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = GraphKey::Name(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphKeyMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }
}