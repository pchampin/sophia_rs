//! Provide a naive implementation of TTerm for test purposes.
use super::*;
use crate::ns::rdf;
use std::fmt;
use std::hash;

/// A naive implementation of TTerm, with no check whatsoever.
#[derive(Clone, Copy, Debug)]
pub struct TestTerm<T> {
    kind: TermKind,
    value: T,
    extra1: Option<T>,
    extra2: Option<T>,
}

impl<'a, T> TestTerm<T>
where
    T: From<&'a str>,
{
    pub fn iri(value: &'a str) -> Self {
        TestTerm {
            kind: TermKind::Iri,
            value: value.into(),
            extra1: None,
            extra2: None,
        }
    }
    pub fn iri2(ns: &'a str, suffix: &'a str) -> Self {
        TestTerm {
            kind: TermKind::Iri,
            value: ns.into(),
            extra1: Some(suffix.into()),
            extra2: None,
        }
    }
    pub fn bnode(value: &'a str) -> Self {
        TestTerm {
            kind: TermKind::BlankNode,
            value: value.into(),
            extra1: None,
            extra2: None,
        }
    }
    pub fn var(value: &'a str) -> Self {
        TestTerm {
            kind: TermKind::Variable,
            value: value.into(),
            extra1: None,
            extra2: None,
        }
    }
    pub fn lit_dt(value: &'a str, datatype: SimpleIri<'a>) -> Self {
        let (extra1, extra2) = datatype.destruct();
        TestTerm {
            kind: TermKind::Literal,
            value: value.into(),
            extra1: Some(extra1.into()),
            extra2: extra2.map(From::from),
        }
    }
    pub fn lit_lang(value: &'a str, tag: &'a str) -> Self {
        TestTerm {
            kind: TermKind::Literal,
            value: value.into(),
            extra1: None,
            extra2: Some(tag.into()),
        }
    }
}

impl<T> TTerm for TestTerm<T>
where
    T: AsRef<str>,
{
    fn kind(&self) -> TermKind {
        self.kind
    }
    fn value_raw(&self) -> (&str, Option<&str>) {
        match self.kind {
            TermKind::Iri => (
                &self.value.as_ref(),
                self.extra1.as_ref().map(|s| s.as_ref()),
            ),
            _ => (&self.value.as_ref(), None),
        }
    }
    fn datatype(&self) -> Option<SimpleIri> {
        if self.kind == TermKind::Literal {
            Some(match self.extra1.as_ref() {
                None => rdf::langString,
                Some(ns) => {
                    SimpleIri::new_unchecked(ns.as_ref(), self.extra2.as_ref().map(|s| s.as_ref()))
                }
            })
        } else {
            None
        }
    }
    fn language(&self) -> Option<&str> {
        if self.kind == TermKind::Literal && self.extra1.is_none() {
            self.extra2.as_ref().map(|s| s.as_ref())
        } else {
            None
        }
    }
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<T> fmt::Display for TestTerm<T>
where
    T: AsRef<str>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        term_format(self, f)
    }
}

impl<T, U> PartialEq<T> for TestTerm<U>
where
    T: TTerm + ?Sized,
    U: AsRef<str>,
{
    fn eq(&self, other: &T) -> bool {
        term_eq(self, other)
    }
}

impl<T> Eq for TestTerm<T> where T: AsRef<str> + Sized {}

impl<T, U> PartialOrd<T> for TestTerm<U>
where
    T: TTerm + ?Sized,
    U: AsRef<str>,
{
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        Some(term_cmp(self, other))
    }
}

impl<T> hash::Hash for TestTerm<T>
where
    T: AsRef<str>,
{
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        term_hash(self, state)
    }
}

impl From<SimpleIri<'static>> for TestTerm<&'static str> {
    fn from(other: SimpleIri<'static>) -> Self {
        let (ns, suffix) = other.destruct();
        match suffix {
            None => TestTerm::iri(ns),
            Some(sf) => TestTerm::iri2(ns, sf),
        }
    }
}

impl<T> CopyTerm for TestTerm<T>
where
    T: for<'x> From<&'x str>,
{
    fn copy<U>(term: &U) -> Self
    where
        U: TTerm + ?Sized,
    {
        let raw = term.value_raw();
        match term.kind() {
            TermKind::Iri => match raw.1 {
                None => TestTerm::iri(raw.0),
                Some(sf) => TestTerm::iri2(raw.0, sf),
            },
            TermKind::BlankNode => TestTerm::bnode(raw.0),
            TermKind::Variable => TestTerm::var(raw.0),
            TermKind::Literal => match term.language() {
                None => TestTerm::lit_dt(raw.0, term.datatype().unwrap()),
                Some(tag) => TestTerm::lit_lang(raw.0, tag),
            },
        }
    }
}
