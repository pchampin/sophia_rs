//! A `DataCache` can be used to create terms while preventing the proliferation of duplicate string.
//!
//! This is especially useful for [`RcTerm`s](../index.html) and [`ArcTerm`s](../index.html),
//! for which two implementations of `DataCache` are provided.

use crate::{
    data::{FromData, IntoData, TermData},
    Result,
};
use sophia_api::term::RawValue;
use std::fmt;
use std::rc;
use std::sync;
use weak_table::{traits::WeakKey, WeakHashSet};

use super::*;

/// `TermData` that can be used in a `DataCache`.
pub trait CacheableData: TermData {
    /// The weak-key that is stored in the cache.
    type Weak: WeakKey<Strong = Self, Key = str>;
}

impl CacheableData for Rc<str> {
    type Weak = rc::Weak<str>;
}

impl CacheableData for Arc<str> {
    type Weak = sync::Weak<str>;
}

/// A `DataCache` does ref-counting on lexical data required to build terms
/// like IRIs, namespaces and literals.
///
/// Each time a new term is created or when a term is cloned the cache looks up
/// its memory if there is already a ref-counted version available. This reuse
/// makes copying of terms, especially suffixed IRIs, cheaper and saves memory.
#[derive(Clone)]
pub struct DataCache<CD>(WeakHashSet<CD::Weak>)
where
    CD: CacheableData;

/// A `DataCache` for single-threaded environments.
pub type RcDataCache = DataCache<rc::Rc<str>>;

/// A `DataCache` for multi-threaded environments.
pub type ArcDataCache = DataCache<sync::Arc<str>>;

impl<CD> fmt::Debug for DataCache<CD>
where
    CD: CacheableData + Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "DataCache {{")?;
        writeln!(f, "  len : {}", self.0.len())?;
        writeln!(f, "  load: {:3} %", self.0.load_factor() * 100f32)?;
        write!(f, "  elements: [ ")?;
        for e in &self.0 {
            write!(f, "{:?}, ", e)?;
        }
        write!(f, " ]")
    }
}

impl<CD> Default for DataCache<CD>
where
    CD: CacheableData,
{
    fn default() -> Self {
        DataCache(WeakHashSet::default())
    }
}

impl<CD: CacheableData> DataCache<CD> {
    /// Build an IRI from the raw value of the term.
    fn iri_from_raw(&mut self, rv: RawValue) -> Iri<CD>
    where
        CD: for<'a> From<&'a str>,
    {
        match rv {
            RawValue(ns, Some(suffix)) => self.iri_suffixed(ns, suffix),
            RawValue(iri, None) => self.iri(iri),
        }
    }

    /// Number of cached data.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Get a reference to cached `TermData`. If it was not cached already a
    /// new instance is cached.
    pub fn get_term_data<TD>(&mut self, txt: TD) -> CD
    where
        TD: TermData + IntoData<CD>,
    {
        if let Some(term_data) = self.0.get(txt.as_ref()) {
            term_data
        } else {
            let term_data = txt.into_data();
            self.0.insert(Clone::clone(&term_data));
            term_data
        }
    }

    /// Removes unused elements from the cache.
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }

    /// Build a new IRI.
    pub fn iri<TD>(&mut self, iri: TD) -> Result<Term<CD>>
    where
        TD: TermData + IntoData<CD>,
    {
        Term::new_iri(self.get_term_data(iri))
    }

    /// Build a new IRI from namespace and suffix.
    pub fn iri_suffixed<TD1, TD2>(&mut self, ns: TD1, suffix: TD2) -> Result<Term<CD>>
    where
        TD1: TermData + IntoData<CD>,
        TD2: TermData + IntoData<CD>,
    {
        Term::new_iri_suffixed(self.get_term_data(ns), self.get_term_data(suffix))
    }

    /// Build a new blank node with the given ID.
    pub fn bnode<TD>(&mut self, id: TD) -> Result<Term<CD>>
    where
        TD: TermData + IntoData<CD>,
    {
        Term::new_bnode(self.get_term_data(id))
    }

    /// Build a new, typed literal.
    pub fn literal_dt<TD, T>(&mut self, txt: TD, dt: T) -> Result<Term<CD>>
    where
        TD: TermData,
        T: TTerm,
        CD: for<'a> FromData<&'a str> + FromData<TD> + for<'a> From<&'a str>,
    {
        if TermKind::Iri == dt.kind() {
            Term::new_literal_dt(self.get_term_data(txt), self.iri_from_raw(dt.value_raw()))
        } else {
            Err(TermError::NotAnIri(dt.value().to()))
        }
    }

    /// Build a new, language-tagged literal.
    pub fn literal_lang<TD1, TD2>(&mut self, txt: TD1, lang: TD2) -> Result<Term<CD>>
    where
        TD1: TermData + IntoData<CD>,
        TD2: TermData + IntoData<CD>,
    {
        Term::new_literal_lang(self.get_term_data(txt), self.get_term_data(lang))
    }

    /// Build a new variable with the given ID.
    pub fn variable<TD>(&mut self, id: TD) -> Result<Term<CD>>
    where
        TD: TermData + IntoData<CD>,
    {
        Term::new_variable(self.get_term_data(id))
    }

    /// Clone any `Term`.
    pub fn clone_term<TD: TermData>(&mut self, other: &Term<TD>) -> Term<CD>
    where
        CD: for<'a> FromData<&'a str>,
    {
        other.clone_map(|td| self.get_term_data(td))
    }

    /// Clone anything that implements the `TTerm` trait.
    pub fn clone_dyn_term(&mut self, other: &dyn TTerm) -> Term<CD>
    where
        CD: for<'a> FromData<&'a str> + for<'a> FromData<MownStr<'a>> + for<'a> From<&'a str>,
    {
        // A `TTerm` trait is supposed to be correct, therefore, no further checks.
        match other.kind() {
            TermKind::Iri => self.iri_from_raw(other.value_raw()).into(),
            TermKind::BlankNode => self.bnode(other.value()).expect("Valid `&dyn TTerm`"),
            TermKind::Literal => {
                if let Some(lang) = other.language() {
                    self.literal_lang(other.value(), lang)
                        .expect("Valid `&dyn TTerm`")
                } else {
                    let dt = other.datatype().expect("Must be typed");
                    self.literal_dt(other.value(), dt)
                        .expect("Valid `&dyn TTerm`")
                }
            }
            TermKind::Variable => self.variable(other.value()).expect("Valid `&dyn TTerm`"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn cache_cnt() -> Result<(), Box<dyn std::error::Error>> {
        let ns = "http://example.org/ns/";
        let mut cache = RcDataCache::default();
        let _a = cache.iri_suffixed(ns, "a")?;
        let _b = cache.iri_suffixed(ns, "b")?;
        assert_eq!(3, cache.len());

        let iri = SimpleIri::new(ns, Some("dt"))?;
        let _lit = cache.literal_dt("txt", iri)?;
        assert_eq!(5, cache.len());

        Ok(())
    }
}
