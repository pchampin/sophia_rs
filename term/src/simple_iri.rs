//! Minimal implementation of [`TTerm`](../trait.TTerm.html),
//! for representing datatype IRIs of literals.
use super::*;
use sophia_iri::error::{InvalidIri, Result};
use sophia_iri::is_valid_suffixed_iri_ref;
use std::fmt;
use std::hash;

/// See [module documentation](./index.html)
#[derive(Clone, Copy, Eq, Ord)]
pub struct SimpleIri<'a> {
    ns: &'a str,
    suffix: Option<&'a str>,
}

impl<'a> SimpleIri<'a> {
    /// Build a SimpleIri, checking that it is a valid IRI reference.
    pub fn new(ns: &'a str, suffix: Option<&'a str>) -> Result<Self> {
        if is_valid_suffixed_iri_ref(ns, suffix) {
            Ok(Self { ns, suffix })
        } else {
            Err(InvalidIri(format!("{}{}", ns, suffix.unwrap_or(""))))
        }
    }

    /// Build a SimpleIri from its raw components.
    ///
    /// # Pre-condition
    /// It is the user's responsibility to check that `ns` and `suffix`
    /// concatenate to a valid IRI.
    ///
    /// Note that this is nonetheless checked in `debug` mode.
    pub fn new_unchecked(ns: &'a str, suffix: Option<&'a str>) -> Self {
        debug_assert!(suffix.map(|txt| txt.len()).unwrap_or(1) > 0);
        debug_assert!(is_valid_suffixed_iri_ref(ns, suffix));

        Self { ns, suffix }
    }

    /// Destruct this simple IRI into its components
    pub fn destruct(self) -> (&'a str, Option<&'a str>) {
        (self.ns, self.suffix)
    }
}

impl<'a> TTerm for SimpleIri<'a> {
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }
    fn value_raw(&self) -> (&str, Option<&str>) {
        (self.ns, self.suffix)
    }
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<'a> fmt::Display for SimpleIri<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        term_format(self, f)
    }
}

impl<'a, T> PartialEq<T> for SimpleIri<'a>
where
    T: TTerm + ?Sized,
{
    fn eq(&self, other: &T) -> bool {
        term_eq(self, other)
    }
}

impl<'a, T> PartialOrd<T> for SimpleIri<'a>
where
    T: TTerm + ?Sized,
{
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        Some(term_cmp(self, other))
    }
}

impl<'a> hash::Hash for SimpleIri<'a> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        term_hash(self, state)
    }
}
