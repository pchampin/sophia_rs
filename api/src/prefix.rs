//! RDF-related languages (e.g. Turtle, SPARQL) often use prefixes to shorten IRIs.
//! This crate provides generic traits to handle prefix maps.

use crate::term::{SimpleIri, TTerm, TermKind};
use mownstr::MownStr;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

/// A prefix map associates prefixes (`&'a str`) to namespaces.
pub trait PrefixMap<'a> {
    /// The type of term returned by this prefix map.
    type Term: TTerm + 'a;
    /// Return the IRI associated to this prefix, if any.
    ///
    /// It must be guaranteed that the returned term is indeed an IRI.
    fn get_namespace(&self, prefix: &str) -> Option<&Self::Term>;
    /// Return a prefix-suffix pair describing the given IRI, if any.
    ///
    /// If `iri` is another kind of term, implementations MUST return None.
    fn get_prefixed_pair<'s, T: TTerm>(&'s self, iri: &'s T) -> Option<(&'s str, MownStr<'s>)>;
}

impl<'a, PF> PrefixMap<'a> for HashMap<PF, SimpleIri<'a>>
where
    PF: Borrow<str> + Eq + Hash + 'a,
{
    type Term = SimpleIri<'a>;

    fn get_namespace(&self, prefix: &str) -> Option<&Self::Term> {
        self.get(prefix)
    }
    fn get_prefixed_pair<'s, T: TTerm>(&'s self, iri: &'s T) -> Option<(&'s str, MownStr<'s>)> {
        match iri.kind() {
            TermKind::Iri => {
                let raw_value = iri.value_raw();
                let len = raw_value.len();
                self.iter()
                    .filter_map(|(prefix, ns)| {
                        let ns = ns.value_raw();
                        if raw_value.starts_with(ns.bytes()) {
                            Some((prefix.borrow(), raw_value.slice(ns.len()..)))
                        } else {
                            None
                        }
                    })
                    .max_by_key(|(_, suffix)| len-suffix.len())
            }
            _ => None,
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case("http://something.else.com/", None, None; "something else")]
    #[test_case("http://schema.org/Person", None, Some(("s", "Person")); "s:Person")]
    #[test_case("http://example.org/", None, Some(("", "")); "single colon")]
    #[test_case("http://example.org/a/c", None, Some(("a", "c")); "a:c")]
    #[test_case("http://example.org/a/b#c", None, Some(("ab", "c")); "b:c")]
    #[test_case("http://example.org/a#c", None, Some(("", "a#c")); ":a#c")]
    fn get_prefixed_pair(ns: &str, sf: Option<&str>, expected: Option<(&str, &str)>) {

        let mut map = HashMap::new();
        map.insert("s", SimpleIri::new_unchecked("http://schema.org/", None));
        map.insert("ab", SimpleIri::new_unchecked("http://example.org/", Some("a/b#")));
        map.insert("a", SimpleIri::new_unchecked("http://example.org/", Some("a/")));
        map.insert("", SimpleIri::new_unchecked("http://example.org/", None));

        let expected = expected.map(|(pf, sf)| (pf, MownStr::from(sf)));
        let iri = SimpleIri::new_unchecked(ns, sf);
        assert_eq!(map.get_prefixed_pair(&iri), expected);
    }

}
