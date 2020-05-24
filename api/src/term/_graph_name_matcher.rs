// this module is transparently re-exported by its sibling `matcher`

use crate::term::matcher::{AnyOrExactly, AnyTerm};
use crate::term::*;

/// Generic trait for matching graph names, *i.e.* optional [term]s.
///
/// [term]: ../enum.Term.html
pub trait GraphNameMatcher {
    /// Type of `TTerm` used internally by this matcher.
    type Term: TTerm + ?Sized;

    /// If this matcher matches only one graph name, return it, else `None`.
    ///
    /// NB: a graph name is already an `Option`, `None` meaning the (unnamed) default graph.
    /// As a consequence, this methods returns *an option of option* :
    /// * `None` means that the matcher does *not* match a single graph name,
    /// * `Some(None)` means that the matcher matches only the default graph,
    /// * `Some(Some(term))` means that the matcher matches a single proper graph name.
    #[allow(clippy::option_option)]
    fn constant(&self) -> Option<Option<&Self::Term>>;

    /// Check whether this matcher matches `g`.
    fn matches<T>(&self, g: Option<&T>) -> bool
    where
        T: TTerm + ?Sized;
}

impl GraphNameMatcher for AnyTerm {
    type Term = SimpleIri<'static>;
    // NB: the type above does not really matter,
    // since `constant` below always returns None
    fn constant(&self) -> Option<Option<&SimpleIri<'static>>> {
        None
    }
    fn matches<T>(&self, _g: Option<&T>) -> bool
    where
        T: TTerm + ?Sized,
    {
        true
    }
}

impl<U> GraphNameMatcher for AnyOrExactly<Option<U>>
where
    U: TTerm + Sized,
{
    type Term = U;
    fn constant(&self) -> Option<Option<&U>> {
        match self {
            AnyOrExactly::Any => None,
            AnyOrExactly::Exactly(g) => Some(g.as_ref()),
        }
    }
    fn matches<T>(&self, g: Option<&T>) -> bool
    where
        T: TTerm + ?Sized,
    {
        match self {
            AnyOrExactly::Any => true,
            AnyOrExactly::Exactly(gself) => same_graph_name(gself.as_ref(), g),
        }
    }
}

impl<U> GraphNameMatcher for Option<&U>
where
    U: TTerm + ?Sized,
{
    type Term = U;
    fn constant(&self) -> Option<Option<&U>> {
        Some(*self)
    }
    fn matches<T>(&self, g: Option<&T>) -> bool
    where
        T: TTerm + ?Sized,
    {
        same_graph_name(*self, g)
    }
}

impl<U> GraphNameMatcher for [Option<&U>]
where
    U: TTerm + ?Sized,
{
    type Term = U;
    fn constant(&self) -> Option<Option<&U>> {
        if self.len() == 1 {
            Some(self[0])
        } else {
            None
        }
    }
    fn matches<T>(&self, g: Option<&T>) -> bool
    where
        T: TTerm + ?Sized,
    {
        self.iter().any(|gself| same_graph_name(*gself, g))
    }
}

macro_rules! impl_for_array {
    ($n: expr) => {
        impl<'a, U> GraphNameMatcher for [Option<&U>; $n]
        where
            U: TTerm + ?Sized,
        {
            type Term = U;
            fn constant(&self) -> Option<Option<&Self::Term>> {
                if self.len() == 1 {
                    Some(self[0])
                } else {
                    None
                }
            }
            fn matches<T>(&self, g: Option<&T>) -> bool
            where
                T: TTerm + ?Sized,
            {
                self.iter().any(|gself| same_graph_name(*gself, g))
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

impl<U> GraphNameMatcher for Vec<Option<&U>>
where
    U: TTerm + ?Sized,
{
    type Term = U;
    fn constant(&self) -> Option<Option<&U>> {
        if self.len() == 1 {
            Some(self[0])
        } else {
            None
        }
    }
    fn matches<T>(&self, g: Option<&T>) -> bool
    where
        T: TTerm + ?Sized,
    {
        self.iter().any(|gself| same_graph_name(*gself, g))
    }
}

impl<F> GraphNameMatcher for [F; 1]
where
    F: Fn(Option<&dyn TTerm>) -> bool,
{
    type Term = SimpleIri<'static>;
    // NB: the type above does not really matter,
    // since `constant` below always returns None
    fn constant(&self) -> Option<Option<&SimpleIri<'static>>> {
        None
    }
    fn matches<T>(&self, g: Option<&T>) -> bool
    where
        T: TTerm + ?Sized,
    {
        (self[0])(g.map(T::as_dyn))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::term::matcher::ANY;

    #[test]
    fn test_any_as_matcher() {
        let m = ANY;
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let n0: Option<SimpleIri> = None;
        let n1 = Some(SimpleIri::new("http://champin.net/#", Some("pa")).unwrap());
        let n2 = Some(SimpleIri::new("http://example.org/", None).unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(m.matches(n2.as_ref()));
    }

    #[test]
    fn test_aoe_any_as_matcher() {
        let m = AnyOrExactly::<Option<SimpleIri>>::Any;
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let n0: Option<SimpleIri> = None;
        let n1 = Some(SimpleIri::new("http://champin.net/#", Some("pa")).unwrap());
        let n2 = Some(SimpleIri::new("http://example.org/", None).unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(m.matches(n2.as_ref()));
    }

    #[test]
    fn test_aoe_explicit_as_matcher() {
        let m = AnyOrExactly::Exactly(Some(
            SimpleIri::new("http://champin.net/#pa", None).unwrap(),
        ));
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let n0: Option<SimpleIri> = None;
        let n1 = Some(SimpleIri::new("http://champin.net/#", Some("pa")).unwrap());
        let n2 = Some(SimpleIri::new("http://example.org/", None).unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_option_as_matcher() {
        let g = SimpleIri::new("http://champin.net/#pa", None).unwrap();
        let m = Some(&g);
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let n0: Option<SimpleIri> = None;
        let n1 = Some(SimpleIri::new("http://champin.net/#", Some("pa")).unwrap());
        let n2 = Some(SimpleIri::new("http://example.org/", None).unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_vec_and_slice_as_matcher() {
        let g1 = Some(SimpleIri::new("http://champin.net/#pa", None).unwrap());
        let g2 = None;
        let v = [g1.as_ref(), g2.as_ref()];
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let n0: Option<SimpleIri> = None;
        let n1 = Some(SimpleIri::new("http://champin.net/#", Some("pa")).unwrap());
        let n2 = Some(SimpleIri::new("http://example.org/", None).unwrap());

        let m = &v[..0];
        let mc = GraphNameMatcher::constant(m);
        assert!(mc.is_none());
        assert!(!GraphNameMatcher::matches(m, n0.as_ref()));
        assert!(!GraphNameMatcher::matches(m, n1.as_ref()));
        assert!(!GraphNameMatcher::matches(m, n2.as_ref()));

        let m = &v[..1];
        let mc = GraphNameMatcher::constant(m);
        assert!(mc.is_some());
        assert!(same_graph_name(mc.unwrap(), n1.as_ref()));
        assert!(!GraphNameMatcher::matches(m, n0.as_ref()));
        assert!(GraphNameMatcher::matches(m, n1.as_ref()));
        assert!(!GraphNameMatcher::matches(m, n2.as_ref()));

        let m = &v[..];
        let mc = GraphNameMatcher::constant(m);
        assert!(mc.is_none());
        assert!(GraphNameMatcher::matches(m, n0.as_ref()));
        assert!(GraphNameMatcher::matches(m, n1.as_ref()));
        assert!(!GraphNameMatcher::matches(m, n2.as_ref()));

        let m = v;
        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(GraphNameMatcher::matches(&m, n0.as_ref()));
        assert!(GraphNameMatcher::matches(&m, n1.as_ref()));
        assert!(!GraphNameMatcher::matches(&m, n2.as_ref()));
    }

    #[test]
    fn test_array_as_matcher() {
        let g1 = Some(SimpleIri::new("http://champin.net/#pa", None).unwrap());
        let g2 = None;
        let m = [g1.as_ref(), g2.as_ref()];
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let n0: Option<SimpleIri> = None;
        let n1 = Some(SimpleIri::new("http://champin.net/#", Some("pa")).unwrap());
        let n2 = Some(SimpleIri::new("http://example.org/", None).unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }

    #[test]
    fn test_func_as_matcher() {
        let m = [|t: Option<&dyn TTerm>| match t {
            None => false,
            Some(t) => t.value().starts_with("http://champin"),
        }];

        let n0: Option<SimpleIri> = None;
        let n1 = Some(SimpleIri::new("http://champin.net/#", Some("pa")).unwrap());
        let n2 = Some(SimpleIri::new("http://example.org/", None).unwrap());

        let mc = GraphNameMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(!m.matches(n0.as_ref()));
        assert!(m.matches(n1.as_ref()));
        assert!(!m.matches(n2.as_ref()));
    }
}
