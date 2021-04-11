use super::{AsPrefix, IsPrefix, Prefix};
/// Define the [`PrefixMap`] trait with default implementation.
use crate::term::{TTerm, TermKind};
use mownstr::MownStr;
use sophia_iri::{AsIri, Iri, IsIri};

/// A prefix map associates prefixes to namespaces.
pub trait PrefixMap {
    /// Return the IRI associated to this prefix, if any.
    fn get_namespace(&self, prefix: &str) -> Option<Iri>;
    /// Return a prefix-suffix pair describing the given IRI, if any.
    ///
    /// If `iri` is another kind of term, implementations MUST return None.
    fn get_prefixed_pair<'s, T: TTerm>(&'s self, iri: &'s T) -> Option<(Prefix, MownStr<'s>)> {
        self.get_checked_prefixed_pair(iri, |_| true)
    }
    /// Return a prefix-suffix pair describing the given IRI, if any,
    /// guaranteeing that the suffix will satisfy the given predicate.
    ///
    /// If `iri` is another kind of term, implementations MUST return None.
    fn get_checked_prefixed_pair<'s, T, F>(
        &'s self,
        iri: &'s T,
        suffix_check: F,
    ) -> Option<(Prefix, MownStr<'s>)>
    where
        T: TTerm,
        F: Fn(&str) -> bool;
}

impl<P, N> PrefixMap for [(P, N)]
where
    P: IsPrefix,
    N: IsIri,
{
    fn get_namespace(&self, prefix: &str) -> Option<Iri> {
        for (p, n) in self {
            if p.borrow() == prefix {
                return Some(n.as_iri());
            }
        }
        None
    }
    fn get_checked_prefixed_pair<'s, T, F>(
        &'s self,
        iri: &'s T,
        suffix_check: F,
    ) -> Option<(Prefix, MownStr<'s>)>
    where
        T: TTerm,
        F: Fn(&str) -> bool,
    {
        if iri.kind() != TermKind::Iri {
            return None;
        }
        let iri_val = iri.value();
        let mut matched = 0;
        let mut found = None;
        for (p, n) in self {
            let n_str = n.borrow();
            if iri_val.starts_with(n_str) && n_str.len() > matched {
                let maybe_matched = n.borrow().len();
                let suffix = iri.value_raw().slice(maybe_matched..);
                if suffix_check(&suffix) {
                    matched = maybe_matched;
                    found = Some((p.as_prefix(), suffix));
                }
            }
        }
        found
    }
}

#[cfg(test)]
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
