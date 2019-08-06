//! A term-matcher is something that can be used
//! to discriminate members of a set of terms.
//!
//! See [`Graph::triples_matching`](../../graph/trait.Graph.html#method.triples_matching),
//! [`MutableGraph::remove_matching`](../../graph/trait.MutableGraph.html#method.remove_matching),
//! [`MutableGraph::retain`](../../graph/trait.MutableGraph.html#method.retain).

use super::*;

pub use super::_graph_name_matcher::*;

/// Anything that matches a [term] or a set of [term]s.
///
/// [term]: ../enum.Term.html
///
pub trait TermMatcher {
    type TermData: TermData;
    /// If this matcher matches only one term, return this term, else `None`.
    fn constant(&self) -> Option<&Term<Self::TermData>>;

    /// Check whether this matcher matches `t`.
    fn matches<T>(&self, t: &Term<T>) -> bool
    where
        T: TermData;
}

/// A universal matcher: it matches any term or graph name (even the default graph).
pub const ANY: AnyTerm = AnyTerm {};

pub struct AnyTerm {}

impl TermMatcher for AnyTerm {
    type TermData = &'static str;
    fn constant(&self) -> Option<&Term<Self::TermData>> {
        None
    }
    fn matches<T>(&self, _t: &Term<T>) -> bool
    where
        T: TermData,
    {
        true
    }
}

impl<U> TermMatcher for Term<U>
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<&Term<Self::TermData>> {
        Some(self)
    }
    fn matches<T>(&self, t: &Term<T>) -> bool
    where
        T: TermData,
    {
        t == self
    }
}

impl<U> TermMatcher for [Term<U>]
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<&Term<Self::TermData>> {
        if self.len() == 1 {
            Some(&self[0])
        } else {
            None
        }
    }
    fn matches<T>(&self, t: &Term<T>) -> bool
    where
        T: TermData,
    {
        for term in self {
            if t == term {
                return true;
            }
        }
        false
    }
}

impl<F: Fn(&RefTerm) -> bool> TermMatcher for F {
    type TermData = &'static str;
    fn constant(&self) -> Option<&Term<Self::TermData>> {
        None
    }
    fn matches<T>(&self, t: &Term<T>) -> bool
    where
        T: TermData,
    {
        self(&RefTerm::from(t))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_term_as_matcher() {
        let m = BoxTerm::new_iri("http://champin.net/#pa").unwrap();
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let t1 = RcTerm::new_iri2("http://champin.net/#", "pa").unwrap();
        let t2 = RcTerm::new_iri("http://example.org/").unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &t1);
        assert!(TermMatcher::matches(&m, &t1));
        assert!(!TermMatcher::matches(&m, &t2));
    }

    #[test]
    fn test_any_as_matcher() {
        let m = ANY;
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let t1 = RcTerm::new_iri2("http://champin.net/#", "pa").unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(&m, &t1));
    }

    #[test]
    fn test_vec1_as_matcher() {
        let m = [BoxTerm::new_iri("http://champin.net/#pa").unwrap()];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let t1 = RcTerm::new_iri2("http://champin.net/#", "pa").unwrap();
        let t2 = RcTerm::new_iri("http://example.org/").unwrap();

        let mc = TermMatcher::constant(&m[..]);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &t1);
        assert!(TermMatcher::matches(&m[..], &t1));
        assert!(!TermMatcher::matches(&m[..], &t2));
    }

    #[test]
    fn test_vec2_as_matcher() {
        let m = [
            BoxTerm::new_iri("http://champin.net/#pa").unwrap(),
            BoxTerm::new_iri("http://example.org/").unwrap(),
        ];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let t1 = RcTerm::new_iri2("http://champin.net/#", "pa").unwrap();
        let t2 = RcTerm::new_iri("http://example.org/").unwrap();
        let t3 = RcTerm::new_iri("http://example.org/other").unwrap();

        let mc = TermMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(&m[..], &t1));
        assert!(TermMatcher::matches(&m[..], &t2));
        assert!(!TermMatcher::matches(&m[..], &t3));
    }

    #[test]
    fn test_vec0_as_matcher() {
        let m: [BoxTerm; 0] = [];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let t1 = RcTerm::new_iri2("http://champin.net/#", "pa").unwrap();

        let mc = TermMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(!TermMatcher::matches(&m[..], &t1));
    }

    #[test]
    fn test_func_as_matcher() {
        let t1 = RcTerm::new_iri2("http://champin.net/#", "pa").unwrap();
        let t2 = RcTerm::new_iri("http://example.org/").unwrap();

        let m = |t: &RefTerm| t.value().starts_with("http://champin");
        assert!(TermMatcher::constant(&m).is_none());
        assert!(TermMatcher::matches(&m, &t1));
        assert!(!TermMatcher::matches(&m, &t2));
    }
}
