//! I define the [`LanguageTag`] wrapper type,
//! which guarantees that the underlying `str`
//! is a valid [BCP47](https://tools.ietf.org/search/bcp47) language tag.
//!
//! A [`LanguageTag`] can be combined to a `&str` with the `*` operator,
//! to produce an RDF [language tagged string](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string)
//! implementing the [`Term`](crate::term::Term) trait:
//!
//! ```
//! # use sophia_api::term::{LanguageTag, Term};
//! let fr = LanguageTag::new_unchecked("fr");
//! let message = "Bonjour le monde" * fr;
//! assert!(message.is_literal());
//! assert_eq!(message.lexical_form().unwrap(), "Bonjour le monde");
//! assert_eq!(message.language_tag().unwrap(), fr);
//! ```

use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::Borrow;
use std::cmp::{Ordering, PartialOrd};
use std::fmt::Debug;
use thiserror::Error;

lazy_static! {
    /// Regular expression approximating the grammar defined in BCP47.
    /// (it is actually more permissive).
    ///
    /// # Captures
    ///
    /// This regular expression matches the whole input (`^...$`),
    /// therefore, it can not be used to capture language tags in an arbitrary string.
    static ref LANG_TAG: Regex = Regex::new(r#"(?x)
      ^
      [A-Za-z][A-Za-z0-9]*
      (-[A-Za-z0-9]+)*
      $
    "#).unwrap();
}

/// This wrapper guarantees that the underlying `str`
/// is a valid [BCP47](https://tools.ietf.org/search/bcp47) language tag.
///
/// NB: it is actually more permissive than BCP47.
#[derive(Clone, Copy, Debug)]
pub struct LanguageTag<T: Borrow<str>>(T);

impl<T: Borrow<str>> LanguageTag<T> {
    /// Build a new [`LanguageTag`] from `tag`,
    /// returning an error if it is not a valid BCP47 language tag.
    pub fn new(tag: T) -> Result<Self, InvalidLanguageTag> {
        if LANG_TAG.is_match(tag.borrow()) {
            Ok(LanguageTag(tag))
        } else {
            Err(InvalidLanguageTag(tag.borrow().to_string()))
        }
    }

    /// Build a new [`LanguageTag`] from `tag`.
    /// It does not check that the value returned by the function is valid.
    /// If it is not, it may result in undefined behaviour.
    pub fn new_unchecked(tag: T) -> Self {
        assert!(LANG_TAG.is_match(tag.borrow()));
        LanguageTag(tag)
    }

    /// Returns the wrapped value, consuming `self`.
    pub fn unwrap(self) -> T {
        self.0
    }

    /// Gets a reference to the underlying `str`.
    pub fn as_str(&self) -> &str {
        self.0.borrow()
    }

    /// Convert reference to a `LanguageTag<&str>`
    pub fn as_ref(&self) -> LanguageTag<&str> {
        LanguageTag(self.0.borrow())
    }

    /// Map a [`LanguageTag`]`<T>` to a [`LanguageTag`]`<U>`
    /// by applying a function to the wrapped value.
    ///
    /// It does not check that the value returned by the function is valid.
    /// If it is not, it may result in undefined behaviour.
    pub fn map_unchecked<F, U>(self, f: F) -> LanguageTag<U>
    where
        F: FnOnce(T) -> U,
        U: Borrow<str>,
    {
        LanguageTag(f(self.0))
    }
}

impl LanguageTag<&'static str> {
    /// Construct a `LanguageTag<&'static>`
    /// without checking that the inner value is valid.
    /// If it is not, it may result in undefined behaviour.
    pub const fn new_unchecked_const(inner: &'static str) -> Self {
        Self(inner)
    }
}

impl<T: Borrow<str>> std::ops::Deref for LanguageTag<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: Borrow<str>> AsRef<T> for LanguageTag<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T: Borrow<str>> AsRef<str> for LanguageTag<T> {
    fn as_ref(&self) -> &str {
        self.0.borrow()
    }
}

impl<T: Borrow<str>> Borrow<T> for LanguageTag<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

impl<T: Borrow<str>> Borrow<str> for LanguageTag<T> {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl<T: Borrow<str>, U: Borrow<str>> PartialEq<LanguageTag<T>> for LanguageTag<U> {
    fn eq(&self, other: &LanguageTag<T>) -> bool {
        self.as_str().eq_ignore_ascii_case(other.as_str())
    }
}

impl<T: Borrow<str>> PartialEq<str> for LanguageTag<T> {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<T: Borrow<str>> Eq for LanguageTag<T> {}

impl<T: Borrow<str>> PartialOrd for LanguageTag<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Borrow<str>> PartialOrd<str> for LanguageTag<T> {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        let iter1 = self.as_str().chars().map(|c| c.to_ascii_lowercase());
        let iter2 = other.chars().map(|c| c.to_ascii_lowercase());
        iter1.partial_cmp(iter2)
    }
}

impl<T: Borrow<str>> Ord for LanguageTag<T> {
    fn cmp(&self, other: &LanguageTag<T>) -> Ordering {
        let iter1 = self.as_str().chars().map(|c| c.to_ascii_lowercase());
        let iter2 = other.as_str().chars().map(|c| c.to_ascii_lowercase());
        iter1.cmp(iter2)
    }
}

impl<T: Borrow<str>> std::hash::Hash for LanguageTag<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str()
            .chars()
            .map(|c| c.to_ascii_lowercase())
            .for_each(|c| c.hash(state));
    }
}

/// This error is raised when trying to parse an invalid language tag.
#[derive(Debug, Error)]
#[error("The given language tag '{0}' does not comply with BCP47")]
pub struct InvalidLanguageTag(pub String);

impl<'a> std::ops::Mul<LanguageTag<&'a str>> for &'a str {
    type Output = super::SimpleTerm<'a>;

    fn mul(self, rhs: LanguageTag<&'a str>) -> Self::Output {
        super::SimpleTerm::LiteralLanguage(self.into(), rhs.map_unchecked(mownstr::MownStr::from))
    }
}

#[cfg(test)]
mod test {
    use crate::term::Term;

    use super::*;
    use test_case::test_case;

    #[test_case("en")]
    #[test_case("fr")]
    #[test_case("fr-FR")]
    #[test_case("fr-ca")]
    #[test_case("fr-056")]
    #[test_case("ja-Hani")]
    #[test_case("ja-Hira")]
    #[test_case("abc-de-fg-hi")]
    #[test_case("x-abc-de-fg-hi")]
    fn valid(tag: &str) {
        assert!(LanguageTag::new(tag).is_ok());
    }

    #[test_case(""; "empty")]
    #[test_case(" "; "space")]
    #[test_case("éh")]
    #[test_case("a.")]
    fn invalid(tag: &str) {
        assert!(LanguageTag::new(tag).is_err());
    }

    #[test_case("fr", "fr"; "all_lower")]
    #[test_case("fr-ca", "fr-ca"; "all_lower_with_country")]
    #[test_case("fr", "FR"; "language_differ")]
    #[test_case("en-us", "en-US"; "country_differ")]
    fn case_insensitive_eq(tag1: &str, tag2: &str) {
        let ltag1 = LanguageTag::new_unchecked(tag1);
        let ltag2 = LanguageTag::new_unchecked(tag2);
        assert_eq!(ltag1, ltag2); // LanguageTag == LanguageTag
        assert_eq!(&ltag1, tag2); // &LanguageTag == &str
    }

    #[test_case("EN", "FR"; "all_upper")]
    #[test_case("en", "fr"; "all_lower")]
    #[test_case("en", "FR"; "lower_upper")]
    #[test_case("EN", "fr"; "upper_lower")]
    #[test_case("en-UK", "en-US"; "counry_all_upper")]
    #[test_case("en-uk", "en-us"; "counry_all_lower")]
    #[test_case("en-uk", "en-US"; "counry_lower_upper")]
    #[test_case("en-UK", "en-us"; "counry_upper_lower")]
    fn case_insensitive_cmp(tag1: &str, tag2: &str) {
        let ltag1 = LanguageTag::new_unchecked(tag1);
        let ltag2 = LanguageTag::new_unchecked(tag2);
        assert!(ltag1 <= ltag2); // LanguageTag == LanguageTag
        assert!(&ltag1 <= tag2); // &LanguageTag == &str
    }

    #[test]
    fn test_product() {
        let en = LanguageTag::new("en").unwrap();
        let frfr = LanguageTag::new("fr-FR").unwrap();
        let t1 = "chat" * en;
        assert!(t1.is_literal());
        assert_eq!(t1.lexical_form().unwrap(), "chat");
        assert_eq!(t1.language_tag().unwrap(), en);
        let t2 = "chat" * frfr;
        assert!(t2.is_literal());
        assert_eq!(t2.lexical_form().unwrap(), "chat");
        assert_eq!(t2.language_tag().unwrap(), frfr);
        let t3 = "cat" * en;
        assert!(t3.is_literal());
        assert_eq!(t3.lexical_form().unwrap(), "cat");
        assert_eq!(t3.language_tag().unwrap(), en);
    }
}
