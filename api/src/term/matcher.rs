//! This crate defines generic traits and default implementations for *matchers*,
//! objects that can be used to match zero, one or several terms.
//!
//! For a list of matcher implementations,
//! check [`TermMarcher`'s ](trait.TermMatcher.html#implementors) and
//! [`GraphNameMatcher`'s implementors lists](trait.GraphNameMatcher.html#implementors).
//!
//! For methods using matchers (with examples), see for example
//! [`Graph::triples_matching`](crate::graph::Graph::triples_matching),
//! [`MutableGraph::remove_matching`](crate::graph::MutableGraph::remove_matching),
//! [`MutableGraph::retain_matching`](crate::graph::MutableGraph::retain_matching),
//! [`Dataset::quads_matching`](crate::dataset::Dataset::quads_matching),
//! [`MutableDataset::remove_matching`](crate::dataset::MutableDataset::remove_matching), and
//! [`MutableDataset::retain_matching`](crate::dataset::MutableDataset::retain_matching).
//!

use super::*;

pub use super::_graph_name_matcher::*;

/// Generic trait for matching [TTerm]s.
pub trait TermMatcher {
    /// Type of `TTerm` used internally by this matcher.
    type Term: TTerm + ?Sized;

    /// If this matcher matches only one term, return this term, else `None`.
    fn constant(&self) -> Option<&Self::Term>;

    /// Check whether this matcher matches `t`.
    fn matches<T>(&self, t: &T) -> bool
    where
        T: TTerm + ?Sized;
}

/// A universal matcher: it matches any term or graph name (even the default graph).
pub const ANY: AnyTerm = AnyTerm {};

/// The type of the [`ANY`](constant.ANY.html) singleton matcher.
pub struct AnyTerm {}

impl TermMatcher for AnyTerm {
    type Term = SimpleIri<'static>;
    // NB: the type above does not really matter,
    // since `constant` below always returns None
    fn constant(&self) -> Option<&SimpleIri<'static>> {
        None
    }
    fn matches<T>(&self, _t: &T) -> bool
    where
        T: TTerm + ?Sized,
    {
        true
    }
}

/// A matcher matching either any term, or only a specific owned term.
pub enum AnyOrExactly<T> {
    /// Match any term.
    Any,
    /// Match only this term.
    Exactly(T),
}

impl<T> From<Option<T>> for AnyOrExactly<T>
where
    T: TTerm + Sized,
{
    fn from(other: Option<T>) -> AnyOrExactly<T> {
        match other {
            None => AnyOrExactly::Any,
            Some(t) => AnyOrExactly::Exactly(t),
        }
    }
}

impl<U> TermMatcher for AnyOrExactly<U>
where
    U: TTerm + Sized,
{
    type Term = U;
    fn constant(&self) -> Option<&U> {
        match self {
            AnyOrExactly::Any => None,
            AnyOrExactly::Exactly(t) => Some(t),
        }
    }
    fn matches<T>(&self, t: &T) -> bool
    where
        T: TTerm + ?Sized,
    {
        match self {
            AnyOrExactly::Any => true,
            AnyOrExactly::Exactly(tself) => term_eq(tself, t),
        }
    }
}

/// A matcher matching either any term, or only a specific borrowed term.
pub enum AnyOrExactlyRef<T> {
    /// Match any term.
    Any,
    /// Match only this term.
    Exactly(T),
}

impl<T> From<Option<T>> for AnyOrExactlyRef<T>
where
    T: Sized,
{
    fn from(other: Option<T>) -> AnyOrExactlyRef<T> {
        match other {
            None => AnyOrExactlyRef::Any,
            Some(t) => AnyOrExactlyRef::Exactly(t),
        }
    }
}

impl<'a, U> TermMatcher for AnyOrExactlyRef<&'a U>
where
    U: TTerm + ?Sized,
{
    type Term = U;
    fn constant(&self) -> Option<&U> {
        match self {
            AnyOrExactlyRef::Any => None,
            AnyOrExactlyRef::Exactly(t) => Some(t),
        }
    }
    fn matches<T>(&self, t: &T) -> bool
    where
        T: TTerm + ?Sized,
    {
        match self {
            AnyOrExactlyRef::Any => true,
            AnyOrExactlyRef::Exactly(tself) => term_eq(*tself, t),
        }
    }
}

impl<U> TermMatcher for U
where
    U: TTerm + ?Sized,
{
    type Term = U;
    fn constant(&self) -> Option<&U> {
        Some(self)
    }
    fn matches<T>(&self, t: &T) -> bool
    where
        T: TTerm + ?Sized,
    {
        term_eq(self, t)
    }
}

impl<U> TermMatcher for [&U]
where
    U: TTerm + ?Sized,
{
    type Term = U;
    fn constant(&self) -> Option<&U> {
        if self.len() == 1 {
            Some(self[0])
        } else {
            None
        }
    }
    fn matches<T>(&self, t: &T) -> bool
    where
        T: TTerm + ?Sized,
    {
        self.iter().any(|tself| term_eq(*tself, t))
    }
}

macro_rules! impl_for_array {
    ($n: expr) => {
        impl<U> TermMatcher for [&U; $n]
        where
            U: TTerm + ?Sized,
        {
            type Term = U;
            fn constant(&self) -> Option<&U> {
                if self.len() == 1 {
                    Some(self[0])
                } else {
                    None
                }
            }
            fn matches<T>(&self, t: &T) -> bool
            where
                T: TTerm + ?Sized,
            {
                self.iter().any(|tself| term_eq(*tself, t))
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

impl<U> TermMatcher for Vec<&U>
where
    U: TTerm + ?Sized,
{
    type Term = U;
    fn constant(&self) -> Option<&U> {
        if self.len() == 1 {
            Some(self[0])
        } else {
            None
        }
    }
    fn matches<T>(&self, t: &T) -> bool
    where
        T: TTerm + ?Sized,
    {
        self.iter().any(|tself| term_eq(*tself, t))
    }
}

impl<F> TermMatcher for [F; 1]
where
    F: Fn(&dyn TTerm) -> bool,
{
    type Term = SimpleIri<'static>;
    // NB: the type above does not really matter,
    // since `constant` below always returns None
    fn constant(&self) -> Option<&SimpleIri<'static>> {
        None
    }
    fn matches<T>(&self, t: &T) -> bool
    where
        T: TTerm + ?Sized,
    {
        (self[0])(t.as_dyn())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_any_as_matcher() {
        let m = ANY;
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(&m, &t1));
    }

    #[test]
    fn test_aoe_any_as_matcher() {
        let m = AnyOrExactly::<SimpleIri>::Any;
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(&m, &t1));
    }

    #[test]
    fn test_aoe_exactly_as_matcher() {
        let m = AnyOrExactly::Exactly(SimpleIri::new("http://champin.net/#pa", None).unwrap());
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();
        let t2 = SimpleIri::new("http://example.org/", None).unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &t1);
        assert!(TermMatcher::matches(&m, &t1));
        assert!(!TermMatcher::matches(&m, &t2));
    }

    #[test]
    fn test_aoer_any_as_matcher() {
        let m = AnyOrExactlyRef::<&SimpleIri>::Any;
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(&m, &t1));
    }

    #[test]
    fn test_aoer_exactly_as_matcher() {
        let t = SimpleIri::new("http://champin.net/#pa", None).unwrap();
        let m = AnyOrExactlyRef::Exactly(&t);
        // comparing to a term using a different term data, and differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();
        let t2 = SimpleIri::new("http://example.org/", None).unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &t1);
        assert!(TermMatcher::matches(&m, &t1));
        assert!(!TermMatcher::matches(&m, &t2));
    }

    #[test]
    fn test_term_as_matcher() {
        let m = SimpleIri::new("http://champin.net/#pa", None).unwrap();
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();
        let t2 = SimpleIri::new("http://example.org/", None).unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &t1);
        assert!(TermMatcher::matches(&m, &t1));
        assert!(!TermMatcher::matches(&m, &t2));
    }

    #[test]
    fn test_vec_and_slice_as_matcher() {
        let m1 = SimpleIri::new("http://champin.net/#pa", None).unwrap();
        let m2 = SimpleIri::new("http://example.org/", None).unwrap();
        let v = vec![&m1, &m2];

        // comparing to a term using a differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();
        let t2 = SimpleIri::new("http://example.org/", None).unwrap();
        let t3 = SimpleIri::new("http://example.org/other", None).unwrap();

        let m = &v[..0];
        let mc = TermMatcher::constant(m);
        assert!(mc.is_none());
        assert!(!TermMatcher::matches(m, &t1));
        assert!(!TermMatcher::matches(m, &t2));
        assert!(!TermMatcher::matches(m, &t3));

        let m = &v[..1];
        let mc = TermMatcher::constant(m);
        assert!(mc.is_some());
        assert_eq!(mc.unwrap(), &t1);
        assert!(TermMatcher::matches(m, &t1));
        assert!(!TermMatcher::matches(m, &t2));
        assert!(!TermMatcher::matches(m, &t3));

        let m = &v[..];
        let mc = TermMatcher::constant(m);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(m, &t1));
        assert!(TermMatcher::matches(m, &t2));
        assert!(!TermMatcher::matches(m, &t3));

        let m = v;
        let mc = TermMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(&m, &t1));
        assert!(TermMatcher::matches(&m, &t2));
        assert!(!TermMatcher::matches(&m, &t3));
    }

    #[test]
    fn test_array_as_matcher() {
        let m1 = SimpleIri::new("http://champin.net/#pa", None).unwrap();
        let m2 = SimpleIri::new("http://example.org/", None).unwrap();
        let m = [&m1, &m2];
        // comparing to a term using a differently cut,
        // to make the test less obvious
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();
        let t2 = SimpleIri::new("http://example.org/", None).unwrap();
        let t3 = SimpleIri::new("http://example.org/other", None).unwrap();

        let mc = TermMatcher::constant(&m);
        assert!(mc.is_none());
        assert!(TermMatcher::matches(&m, &t1));
        assert!(TermMatcher::matches(&m, &t2));
        assert!(!TermMatcher::matches(&m, &t3));
    }

    #[test]
    fn test_func_as_matcher() {
        let t1 = SimpleIri::new("http://champin.net/#", Some("pa")).unwrap();
        let t2 = SimpleIri::new("http://example.org/", None).unwrap();

        let m = [|t: &dyn TTerm| t.value().starts_with("http://champin")];
        assert!(TermMatcher::constant(&m).is_none());
        assert!(TermMatcher::matches(&m, &t1));
        assert!(!TermMatcher::matches(&m, &t2));
    }
}
