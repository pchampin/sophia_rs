mod _kind;
pub use self::_kind::*;

use crate::term::{IriData, Normalization, Result, Term, TermData, TermError};
use language_tag::LangTag;
use std::fmt;
use std::hash::Hash;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, Hash, Eq)]
pub struct LiteralData<T: TermData> {
    pub(crate) text: T,
    pub(crate) kind: LiteralKind<T>,
}

impl<T: TermData> LiteralData<T> {
    /// Returns a new simple literal, i.e. with datatype `xsd:string`.
    pub fn new<U>(text: U) -> Self
    where
        T: From<U> + From<&'static str>,
    {
        if let Term::Iri(xsd_string) = &crate::ns::xsd::string {
            let copy_iri = IriData::from_with(&xsd_string, T::from);
            Self {
                text: text.into(),
                kind: Datatype(copy_iri),
            }
        } else {
            unreachable!("matching constant `xsd::string`")
        }
    }

    /// Return a new literal with the given value and language tag.
    ///
    /// May fail if the language tag is not a valid BCP47 language tag.
    pub fn new_lang<U, V>(txt: U, lang: V) -> Result<Self>
    where
        T: From<U> + From<V>,
        V: AsRef<str>,
    {
        LangTag::from_str(lang.as_ref()).map_err(|err| TermError::InvalidLanguageTag {
            tag: lang.as_ref().to_string(),
            err,
        })?;
        Ok(Self {
            text: T::from(txt),
            kind: Lang(T::from(lang)),
        })
    }

    /// Return a literal.
    ///
    /// # Safety
    /// This function requires that `lang` is a valid language tag.
    pub unsafe fn new_lang_unchecked<U, V>(txt: U, lang: V) -> Self
    where
        T: From<U> + From<V>,
    {
        Self {
            text: T::from(txt),
            kind: Lang(T::from(lang)),
        }
    }

    /// Return a new literal with the given value and datatype.
    ///
    /// May fail if `dt` is not a valid datatype.
    pub fn new_dt<U>(txt: U, dt: Term<T>) -> Result<Self>
    where
        T: From<U>,
    {
        match dt {
            Term::Iri(iri) => Ok(Self {
                text: T::from(txt),
                kind: Datatype(iri),
            }),
            _ => Err(TermError::InvalidDatatype(dt.value())),
        }
    }

    /// Return a typed literal.
    ///
    /// # Panics
    /// Panics if `dt` is not an IRI.
    pub fn new_dt_unchecked<U>(txt: U, dt: Term<T>) -> Self
    where
        T: From<U> + fmt::Debug,
    {
        if let Term::Iri(dt) = dt {
            Self {
                text: T::from(txt),
                kind: Datatype(dt),
            }
        } else {
            panic!(format!(
                "new_dt_unchecked expects Term::Iri as dt, got {:?}",
                dt
            ))
        }
    }

    /// Copy form another literal with the given factory.
    pub fn from_with<'a, U, F>(other: &'a LiteralData<U>, mut factory: F) -> Self
    where
        U: TermData,
        F: FnMut(&'a str) -> T,
    {
        LiteralData {
            text: factory(other.text.as_ref()),
            kind: LiteralKind::from_with(&other.kind, factory),
        }
    }

    /// Copy from another literal with the given factory, applying the given
    /// normalization policy.
    pub fn normalized_with<U, F>(
        other: &'_ LiteralData<U>,
        mut factory: F,
        norm: Normalization,
    ) -> Self
    where
        U: TermData,
        F: FnMut(&str) -> T,
    {
        LiteralData {
            text: factory(other.text.as_ref()),
            kind: LiteralKind::normalized_with(&other.kind, factory, norm),
        }
    }

    /// Return whether this literal's datatype's IRI is absolue.
    ///
    /// If the literal is not typed this function is always true.
    pub fn is_absolute(&self) -> bool {
        if let Datatype(iri) = &self.kind {
            iri.is_absolute()
        } else {
            true
        }
    }

    /// Grants read access to the literal's text.
    pub fn text(&self) -> &T {
        &self.text
    }

    /// Grants read access to the literal's kind.
    pub fn kind(&self) -> &LiteralKind<T> {
        &self.kind
    }
}

impl<T, U> PartialEq<LiteralData<U>> for LiteralData<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &LiteralData<U>) -> bool {
        self.text.as_ref() == other.text.as_ref() && self.kind == other.kind
    }
}

impl<T> fmt::Display for LiteralData<T>
where
    T: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.text.as_ref())
    }
}
