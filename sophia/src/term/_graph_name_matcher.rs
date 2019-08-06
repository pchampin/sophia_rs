// this module is transparently re-exported by its sibbling `matcher`

use crate::term::*;

/// A graph name matcher is something that can be used
/// to discriminate members of a set of graph identifiers.
///
/// See [`Dataset::quads_matching`](../../dataset/trait.Dataset.html#method.quads_matching),
/// [`MutableDataset::remove_matching`](../../dataset/trait.MutableDataset.html#method.remove_matching),
/// [`MutableDataset::retain`](../../dataset/trait.MutableDataset.html#method.retain).
pub trait GraphNameMatcher {
    type TermData: TermData;
    /// If this matcher matches only one graph identifier, return it, else `None`.
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>>;

    /// Check whether this matcher matches `t`.
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
    where
        T: TermData;
}

impl GraphNameMatcher for crate::term::matcher::AnyTerm
{
    type TermData = &'static str;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        None
    }
    fn matches<T>(&self, _g: Option<&Term<T>>) -> bool
    where
        T: TermData,
    {
        true
    }
}

impl<U> GraphNameMatcher for Option<&'_ Term<U>>
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        Some(*self)
    }
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
    where
        T: TermData,
    {
        same_graph_name(*self, g)
    }
}

impl<U> GraphNameMatcher for Option<Term<U>>
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        Some(self.as_ref())
    }
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
    where
        T: TermData,
    {
        same_graph_name(self.as_ref(), g)
    }
}

impl<U> GraphNameMatcher for Term<U>
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        Some(Some(self))
    }
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
    where
        T: TermData,
    {
        same_graph_name(Some(self), g)
    }
}

impl<M> GraphNameMatcher for [M]
where
    M: GraphNameMatcher,
{
    type TermData = M::TermData;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        if self.len() == 1 {
            self[0].constant()
        } else {
            None
        }
    }
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
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
impl<M> GraphNameMatcher for Vec<M>
where
    M: GraphNameMatcher,
{
    type TermData = M::TermData;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        if self.len() == 1 {
            self[0].constant()
        } else {
            None
        }
    }
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
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

impl<F: Fn(Option<&Term<&str>>) -> bool> GraphNameMatcher for F {
    type TermData = &'static str;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        None
    }
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
    where
        T: TermData,
    {
        (self)(g.map(|n| RefTerm::from(n)).as_ref())
    }
}

#[cfg(test)]
mod test {

    use super::*;
    #[test]
    fn test_option_ref_term_as_matcher() {
        let m = Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap());
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }


    #[test]
    fn test_option_term_as_matcher() {
        let m = Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap());
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_term_as_matcher() {
        let m = BoxTerm::new_iri("http://champin.net/#pa").unwrap();
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_any_as_matcher() {
        let m = crate::term::matcher::ANY;
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(m.matches(n2.as_ref()));
    }

    #[test]
    fn test_vec1_as_matcher() {
        let m = vec![Some(
            BoxTerm::new_iri("http://champin.net/#pa").unwrap(),
        )];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m[..]);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_vec2_as_matcher() {
        let m = vec![
            Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap()),
            None,
        ];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_vec0_as_matcher() {
        let m: Vec<Option<BoxTerm>> = vec![];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(!m.matches(n0.as_ref()));
        assert!(!m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_func_as_matcher() {
        let m = |t: Option<&Term<&str>>| match t {
            None => false,
            Some(t) => t.value().starts_with("http://champin"),
        };

        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri2("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }
}
