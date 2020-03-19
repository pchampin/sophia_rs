// this module is transparently re-exported by its sibling `matcher`

use crate::matcher::{AnyOrExactly, AnyTerm};
use crate::*;

/// Generic trait for matching graph names, *i.e.* optional [term]s.
///
/// [term]: ../enum.Term.html
///
pub trait GraphNameMatcher {
    type TermData: TermData;
    /// If this matcher matches only one graph name, return it, else `None`.
    ///
    /// NB: a graph name is already an `Option`, `None` meaning the (unnamed) default graph.
    /// As a consequence, this methods returns *an option of option* :
    /// * `None` means that the matcher does *not* match a single graph name,
    /// * `Some(None)` means that the matcher matches only the default graph,
    /// * `Some(Some(term))` means that the matcher matches a single proper graph name.
    #[allow(clippy::option_option)]
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>>;

    /// Check whether this matcher matches `g`.
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
    where
        T: TermData;
}

impl GraphNameMatcher for AnyTerm {
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

impl<U> GraphNameMatcher for AnyOrExactly<Option<Term<U>>>
where
    U: TermData,
{
    type TermData = U;
    fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
        match self {
            AnyOrExactly::Any => None,
            AnyOrExactly::Exactly(g) => Some(g.as_ref()),
        }
    }
    fn matches<T>(&self, g: Option<&Term<T>>) -> bool
    where
        T: TermData,
    {
        match self {
            AnyOrExactly::Any => true,
            AnyOrExactly::Exactly(gself) => same_graph_name(gself.as_ref(), g),
        }
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

// Also impl'ing on arrays for improving DX
// (&[T;N] is not automatically cast to &[T] in generic functions...).
macro_rules! impl_for_array {
    ($n: expr) => {
        impl<M> GraphNameMatcher for [M; $n]
        where
            M: GraphNameMatcher,
        {
            type TermData = M::TermData;
            fn constant(&self) -> Option<Option<&Term<Self::TermData>>> {
                None
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
    };
}
impl_for_array!(2);
impl_for_array!(3);
impl_for_array!(4);
impl_for_array!(5);
impl_for_array!(6);
impl_for_array!(7);
impl_for_array!(8);
impl_for_array!(9);
impl_for_array!(10);
impl_for_array!(11);
impl_for_array!(12);

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
        (self)(g.map(RefTerm::from).as_ref())
    }
}

#[cfg(test)]
mod test {

    use super::*;

    use crate::matcher::{AnyOrExactly, ANY};
    #[test]
    fn test_option_ref_term_as_matcher() {
        let m = Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap());
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
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
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
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
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
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
        let m = ANY;
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(m.matches(n2.as_ref()));
    }

    #[test]
    fn test_aoe_any_as_matcher() {
        let m = AnyOrExactly::<Option<BoxTerm>>::Any;
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(m.matches(n2.as_ref()));
    }

    #[test]
    fn test_aoe_explicit_as_matcher() {
        let m = AnyOrExactly::Exactly(Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap()));
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_array2_as_matcher() {
        let m = [
            Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap()),
            None,
        ];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_array1_as_matcher() {
        let m = [Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap())];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m[..]);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_array0_as_matcher() {
        let m: [BoxTerm; 0] = [];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m[..]);
        assert!(mc.is_none());
        assert!(!m.matches(n0.as_ref()));
        assert!(!m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_vec_as_matcher() {
        let m = vec![
            Some(BoxTerm::new_iri("http://champin.net/#pa").unwrap()),
            None,
        ];
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_func_as_matcher() {
        let m = |t: Option<&Term<&str>>| match t {
            None => false,
            Some(t) => t.value().starts_with("http://champin"),
        };

        let n0: Option<RcTerm> = None;
        let n1 = Some(RcTerm::new_iri_suffixed("http://champin.net/#", "pa").unwrap());
        let n2 = Some(RcTerm::new_iri("http://example.org/").unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }
}
