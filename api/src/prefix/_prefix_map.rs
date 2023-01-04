use super::{AsPrefix, IsPrefix, Prefix};
/// Define the [`PrefixMap`] trait with default implementation.
use mownstr::MownStr;
use sophia_iri::{AsIri, Iri, IsIri};

/// A prefix map associates prefixes to namespaces.
pub trait PrefixMap {
    /// Return the IRI associated to this prefix, if any.
    fn get_namespace<'s>(&'s self, prefix: &str) -> Option<Iri<&'s str>>;
    /// Return a prefix-suffix pair describing the given IRI, if any.
    fn get_prefixed_pair<'s, T: IsIri + 's>(
        &'s self,
        iri: T,
    ) -> Option<(Prefix<&'s str>, MownStr<'s>)> {
        self.get_checked_prefixed_pair(iri, |_| true)
    }
    /// Return a prefix-suffix pair describing the given IRI, if any,
    /// guaranteeing that the suffix will satisfy the given predicate.
    fn get_checked_prefixed_pair<'s, T, F>(
        &'s self,
        iri: T,
        suffix_check: F,
    ) -> Option<(Prefix<&'s str>, MownStr<'s>)>
    where
        T: IsIri + 's,
        F: Fn(&str) -> bool;
    /// Iterate over (prefix, IRI) pairs.
    fn iter<'s>(&'s self) -> Box<dyn Iterator<Item = (Prefix<&'s str>, Iri<&'s str>)> + 's>;
    /// Copies this prefix map as a self-sufficient vector
    #[allow(clippy::type_complexity)]
    fn to_vec(&self) -> Vec<PrefixMapPair> {
        self.iter()
            .map(|(prefix, ns)| (prefix.map_unchecked(Box::from), ns.map_unchecked(Box::from)))
            .collect()
    }
}

/// Type alias for the return type of [`PrefixMap::to_vec`].
pub type PrefixMapPair = (Prefix<Box<str>>, Iri<Box<str>>);

impl<P, N> PrefixMap for [(P, N)]
where
    P: IsPrefix,
    N: IsIri,
{
    fn get_namespace<'s>(&'s self, prefix: &str) -> Option<Iri<&'s str>> {
        for (p, n) in self {
            if p.borrow() == prefix {
                return Some(n.as_iri());
            }
        }
        None
    }
    fn get_checked_prefixed_pair<'s, T, F>(
        &'s self,
        iri: T,
        suffix_check: F,
    ) -> Option<(Prefix<&'s str>, MownStr<'s>)>
    where
        T: IsIri + 's,
        F: Fn(&str) -> bool,
    {
        let iri_str = iri.borrow();
        let mut matched = 0;
        let mut found = None;
        for (p, n) in self {
            let n_str = n.borrow();
            if iri_str.starts_with(n_str) && n_str.len() > matched {
                let maybe_matched = n_str.len();
                let suffix = &iri_str[maybe_matched..];
                if suffix_check(suffix) {
                    matched = maybe_matched;
                    found = Some((p.as_prefix(), suffix));
                }
            }
        }
        found.map(|(p, s)| (p, s.to_owned().into()))
    }
    fn iter<'s>(&'s self) -> Box<dyn Iterator<Item = (Prefix<&'s str>, Iri<&'s str>)> + 's> {
        Box::new(<[(P, N)]>::iter(self).map(|(prefix, ns)| (prefix.as_prefix(), ns.as_iri())))
    }
}

/*
#[cfg(test)]
#[allow(clippy::bool_assert_comparison, clippy::unused_unit)] // test_case! generated warnings
mod test {
    use super::super::Prefix;
    use super::*;
    use crate::term::SimpleIri;
    use sophia_iri::Iri;
    use test_case::test_case;

    fn make_map() -> Vec<(Prefix<'static>, Iri<'static>)> {
        vec![
            (
                Prefix::new_unchecked("s"),
                Iri::new_unchecked("http://schema.org/"),
            ),
            (
                Prefix::new_unchecked("a"),
                Iri::new_unchecked("http://example.org/a/"),
            ),
            (
                Prefix::new_unchecked("ab"),
                Iri::new_unchecked("http://example.org/a/b#"),
            ),
            (
                Prefix::new_unchecked(""),
                Iri::new_unchecked("http://example.org/"),
            ),
        ]
    }

    #[test_case("s", Some("http://schema.org/"); "s")]
    #[test_case("a", Some("http://example.org/a/"); "a")]
    #[test_case("ab", Some("http://example.org/a/b#"); "ab")]
    #[test_case("", Some("http://example.org/"); "empty")]
    #[test_case("sa", None; "sa")]
    fn get_namespace(prefix: &str, expected: Option<&str>) {
        let expected = expected.map(Iri::new_unchecked);
        let map = make_map();
        let got = map.get_namespace(prefix);
        assert_eq!(got, expected);
    }

    #[test_case("http://something.else.com/", None, None; "something else")]
    #[test_case("http://schema.org/Person", None, Some(("s", "Person")); "s:Person")]
    #[test_case("http://example.org/", None, Some(("", "")); "single colon")]
    #[test_case("http://example.org/", Some("a/c"), Some(("a", "c")); "a:c")]
    #[test_case("http://example.org/", Some("a/b#c"), Some(("ab", "c")); "b:c")]
    #[test_case("http://example.org/a#", Some("c"), Some(("", "a#c")); ":a#c")]
    fn get_prefixed_pair(ns: &str, sf: Option<&str>, expected: Option<(&str, &str)>) {
        let expected = expected.map(|(pf, sf)| (Prefix::new_unchecked(pf), MownStr::from(sf)));
        let map = make_map();
        let iri = SimpleIri::new_unchecked(ns, sf);
        let got = map.get_prefixed_pair(&iri);
        assert_eq!(got, expected);
    }

    #[test_case("http://something.else.com/", None, None; "something else")]
    #[test_case("http://schema.org/Person", None, Some(("s", "Person")); "s:Person")]
    #[test_case("http://example.org/", None, Some(("", "")); "single colon")]
    #[test_case("http://example.org/", Some("a/c"), Some(("a", "c")); "a:c")]
    #[test_case("http://example.org/", Some("a/b#c"), Some(("ab", "c")); "b:c")]
    #[test_case("http://example.org/a#", Some("c"), None; ":a#c")]
    fn get_checked_prefixed_pair(ns: &str, sf: Option<&str>, expected: Option<(&str, &str)>) {
        let expected = expected.map(|(pf, sf)| (Prefix::new_unchecked(pf), MownStr::from(sf)));
        let map = make_map();
        let iri = SimpleIri::new_unchecked(ns, sf);
        let suffix_check = |txt: &str| txt.chars().all(|c| c.is_alphabetic());
        let got = map.get_checked_prefixed_pair(&iri, suffix_check);
        assert_eq!(got, expected);
    }
}
 */
