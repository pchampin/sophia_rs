// this module is transparently re-exported by its sibbling `matcher`

use crate::term::*;

use super::graph_id::*;

/// A graph-key-matcher is something that can be used
/// to discriminate members of a set of graph identifiers.
///
/// See [`Dataset::quads_matching`](../../dataset/trait.Dataset.html#method.quads_matching),
/// [`MutableDataset::remove_matching`](../../dataset/trait.MutableDataset.html#method.remove_matching),
/// [`MutableDataset::retain`](../../dataset/trait.MutableDataset.html#method.retain).
pub trait GraphIdMatcher {
    type TermData: TermData;
    /// If this matcher matches only one graph identifier, return it, else `None`.
    fn constant(&self) -> Option<&GraphId<Self::TermData>>;

    /// Check whether this matcher matches `t`.
    fn matches<T>(&self, g: &GraphId<T>) -> bool
    where
        T: TermData;
}

impl GraphIdMatcher for crate::term::matcher::AnyTerm
{
    type TermData = &'static str;
    fn constant(&self) -> Option<&GraphId<Self::TermData>> {
        None
    }
    fn matches<T>(&self, _g: &GraphId<T>) -> bool
    where
        T: TermData,
    {
        true
    }
}

impl<U> GraphIdMatcher for GraphId<U>
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<&GraphId<Self::TermData>> {
        Some(self)
    }
    fn matches<T>(&self, g: &GraphId<T>) -> bool
    where
        T: TermData,
    {
        g.same_graph_name(self)
    }
}

impl<U> GraphIdMatcher for Term<U>
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<&GraphId<Self::TermData>> {
        Some(self.as_graph_id())
    }
    fn matches<T>(&self, g: &GraphId<T>) -> bool
    where
        T: TermData,
    {
        g.same_graph_name(self.as_graph_id())
    }
}

impl<M> GraphIdMatcher for [M]
where
    M: GraphIdMatcher,
{
    type TermData = M::TermData;
    fn constant(&self) -> Option<&GraphId<Self::TermData>> {
        if self.len() == 1 {
            self[0].constant()
        } else {
            None
        }
    }
    fn matches<T>(&self, g: &GraphId<T>) -> bool
    where
        T: TermData,
    {
        for matcher in self {
            if matcher.matches(g) {
                return true;
            }
        }
        false
    }
}

/// This is somewhat redundant with [M],
/// but it is useful with `Dataset::union_graph`,
/// were a matcher must be *moved* rather than borrowed.
impl<M> GraphIdMatcher for Vec<M>
where
    M: GraphIdMatcher,
{
    type TermData = M::TermData;
    fn constant(&self) -> Option<&GraphId<Self::TermData>> {
        if self.len() == 1 {
            self[0].constant()
        } else {
            None
        }
    }
    fn matches<T>(&self, g: &GraphId<T>) -> bool
    where
        T: TermData,
    {
        for matcher in self {
            if matcher.matches(g) {
                return true;
            }
        }
        false
    }
}

impl<F: Fn(&GraphId<&str>) -> bool> GraphIdMatcher for F {
    type TermData = &'static str;
    fn constant(&self) -> Option<&GraphId<Self::TermData>> {
        None
    }
    fn matches<T>(&self, t: &GraphId<T>) -> bool
    where
        T: TermData,
    {
        match t {
            None => (self)(&None),
            Some(n) => {
                let n = RefTerm::from(n);
                (self)(n.as_graph_id())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_graph_id_as_matcher() {
        let m = Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap());
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: GraphId<&str> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphIdMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(mc.unwrap().same_graph_name(&n1));
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_term_as_matcher() {
        let m = BoxTerm::new_iri("http://champin.net/#pa").unwrap();
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: GraphId<&str> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphIdMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(mc.unwrap().same_graph_name(&n1));
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_any_as_matcher() {
        let m = crate::term::matcher::ANY;
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: GraphId<&str> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphIdMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(m.matches(&n2));
    }

    #[test]
    fn test_vec1_as_matcher() {
        let m = vec![Some(
            BoxTerm::new_iri("http://champin.net/#pa").unwrap(),
        )];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: GraphId<&str> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphIdMatcher::constant(&m[..]);
        assert!(mc.is_some());
        assert!(mc.unwrap().same_graph_name(&n1));
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_vec2_as_matcher() {
        let m = vec![
            Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap()),
            None,
        ];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: GraphId<&str> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphIdMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_vec0_as_matcher() {
        let m: Vec<GraphId<Box<str>>> = vec![];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: GraphId<&str> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphIdMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(!m.matches(&n0));
        assert!(!m.matches(&n1));
        assert!(!m.matches(&n2));
    }

    #[test]
    fn test_func_as_matcher() {
        let m = |t: &GraphId<&str>| match t {
            None => false,
            Some(t) => t.value().starts_with("http://champin"),
        };

        let n0: GraphId<&str> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphIdMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(!m.matches(&n0));
        assert!(m.matches(&n1));
        assert!(!m.matches(&n2));
    }
}
