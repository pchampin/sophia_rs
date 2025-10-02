//! I define the [`LanguageTag`] wrapper type,
//! which guarantees that the underlying `str`
//! is a valid [BCP47](https://tools.ietf.org/search/bcp47) language tag.

use regex::Regex;
use std::borrow::Borrow;
use std::cmp::{Ordering, PartialOrd};
use std::fmt::Debug;
use std::sync::LazyLock;
use thiserror::Error;

/// This wrapper guarantees that the underlying `str`
/// is a valid [BCP47](https://tools.ietf.org/search/bcp47) language tag.
///
/// NB: it is actually slightly more permissive than BCP47,
/// as it does not check that the different subtags are registered (language, country...) codes.
///
/// A [`LanguageTag`] can be combined to a `&str` with the `*` operator,
/// to produce an RDF [language tagged string](https://www.w3.org/TR/rdf11-concepts/#dfn-language-tagged-string)
/// implementing the [`Term`](crate::term::Term) trait:
///
/// ```
/// # use sophia_api::{ns::rdf, term::{LanguageTag, Term}};
/// let fr = LanguageTag::new_unchecked("fr");
/// let message = "Bonjour le monde" * fr;
/// assert!(message.is_literal());
/// assert_eq!(message.lexical_form().unwrap(), "Bonjour le monde");
/// assert_eq!(message.datatype().unwrap(), rdf::langString.iri().unwrap());
/// assert_eq!(message.language_tag().unwrap(), fr);
/// ```
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
        super::SimpleTerm::LiteralLanguage(
            self.into(),
            rhs.map_unchecked(mownstr::MownStr::from),
            None,
        )
    }
}

pub(crate) static LANG_TAG: LazyLock<Regex> = LazyLock::new(|| Regex::new(LANG_TAG_SRC).unwrap());

/// Match a valid BCP47 language tag
pub static LANG_TAG_SRC: &str = r"(?xi-u)^
(
  (?:
    (?: #language
      (?:
        [A-Z]{2,3}
        (?: #extlang
          (?:
            -[A-Z]{3}
          ){0,3}
        )
      )
    |
      [A-Z]{4,8}
    )
    (?: #script
      -[A-Z]{4}
    )?
    (?: #region
      -
      (?:
        [A-Z]{2}
      |
        [0-9]{3}
      )
    )?
    (?: #variant
      -
      (?:
        [A-Z0-9]{5,8}
      |
        [0-9][A-Z0-9]{3}
      )
    )*
    (?: #extension
      -[0-9A-WY-Z]
      (?:
        -[A-Z0-9]{2,8}
      )+
    )*
    (?: #privateUse
      -X
      (?:
        -[A-Z0-9]{1,8}
      )+
    )?
  )
|
  (?: #privateUse
    X
    (?:
      -[A-Z0-9]{1,8}
    )+
  )
|
  (?: #grandfathered
    en-GB-oed|i-ami|i-bnn|i-default|i-enochian|i-hak|i-klingon|i-lux|i-mingo|i-navajo|i-pwn|i-tao|i-tay|i-tsu|sgn-BE-FR|sgn-BE-NL|sgn-CH-DE
    # NB regular grandfathered tags are not included,
    # as they will be matched by the normal case
  )
)$";

#[cfg(test)]
mod test {
    use std::iter::once;

    use crate::term::Term;

    use super::*;
    use test_case::test_case;

    #[test]
    fn valid() {
        for mut tag in valid_tags() {
            assert!(LanguageTag::new(tag.as_str()).is_ok(), "{tag}");
            tag.make_ascii_uppercase();
            assert!(LanguageTag::new(tag.as_str()).is_ok(), "{tag}");
        }
        for mut txt in private_uses(3) {
            let tag = &txt[1..];
            assert!(LanguageTag::new(tag).is_ok(), "{tag}");
            txt.make_ascii_uppercase();
            let tag = &txt[1..];
            assert!(LanguageTag::new(tag).is_ok(), "{tag}");
        }
        for tag in GRANDFATHERED_TAGS {
            assert!(LanguageTag::new(*tag).is_ok(), "{tag}");
            assert!(LanguageTag::new(tag.to_ascii_uppercase()).is_ok(), "{tag}");
            assert!(LanguageTag::new(tag.to_ascii_lowercase()).is_ok(), "{tag}");
        }
    }

    #[test]
    fn invalid() {
        for tag in valid_tags() {
            for invalid_suffix in ["a@", "abcdefghi"] {
                let txt = format!("{tag}-{invalid_suffix}");
                assert!(LanguageTag::new(txt.as_str()).is_err(), "{txt}");
            }
        }
        for txt in INVALID_TAGS {
            assert!(LanguageTag::new(*txt).is_err(), "{txt}");
        }
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
    #[test_case("en-UK", "en-US"; "country_all_upper")]
    #[test_case("en-uk", "en-us"; "country_all_lower")]
    #[test_case("en-uk", "en-US"; "country_lower_upper")]
    #[test_case("en-UK", "en-us"; "country_upper_lower")]
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

    // below are utility functions used to generate valid (and invalid) tags for testing

    fn valid_tags() -> impl Iterator<Item = String> {
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            for language in languages() {
                for script in once("").chain(scripts()) {
                    for region in once("").chain(regions()) {
                        for variant in once("".to_string()).chain(variants(1)) {
                            for extension in once("".to_string()).chain(extensions(1)) {
                                for private_use in once("".to_string()).chain(private_uses(1)) {
                                    let tag = format!(
                                        "{language}{script}{region}{variant}{extension}{private_use}"
                                    );
                                    tx.send(tag).unwrap();
                                }
                            }
                        }
                    }
                }
            }
            for variant in variants(2) {
                let tag = format!("en{variant}");
                tx.send(tag).unwrap();
            }
            for extension in extensions(2) {
                let tag = format!("en{extension}");
                tx.send(tag).unwrap();
            }
            for private_use in private_uses(2) {
                let tag = format!("en{private_use}");
                tx.send(tag).unwrap();
            }
        });
        rx.into_iter()
    }

    fn languages() -> impl Iterator<Item = String> {
        ["en", "eng"]
            .into_iter()
            .flat_map(|language| langexts().map(move |exts| format!("{language}{exts}")))
            .chain(["dial", "diale", "dialec", "dialect", "dialects"].map(Into::into))
    }

    fn langexts() -> impl Iterator<Item = &'static str> {
        ["", "-ext", "-ext-ext", "-ext-ext-ext"].into_iter()
    }

    fn scripts() -> impl Iterator<Item = &'static str> {
        ["-latn"].into_iter()
    }

    fn regions() -> impl Iterator<Item = &'static str> {
        ["-uk", "-826"].into_iter()
    }
    fn variants(max: u8) -> impl Iterator<Item = String> {
        debug_assert!(max >= 1);
        (1..=max).flat_map(variant_parts)
    }

    fn variant_parts(n: u8) -> Box<dyn Iterator<Item = String>> {
        match n {
            0 => Box::new(once("".to_string())),
            n => Box::new(variant_parts(n - 1).flat_map(|prefix| {
                ["varia", "variaa", "variant", "variants", "0var"]
                    .map(move |suffix| format!("{prefix}-{suffix}"))
            })),
        }
    }

    fn extensions(max: u8) -> impl Iterator<Item = String> {
        debug_assert!(max >= 1);
        (1..=max).flat_map(move |i| extension_parts(i, max))
    }

    fn extension_parts(n: u8, max: u8) -> Box<dyn Iterator<Item = String>> {
        match n {
            0 => Box::new(once("".to_string())),
            n => Box::new(extension_parts(n - 1, max).flat_map(move |prefix| {
                (1..=max)
                    .flat_map(extension_part_parts)
                    .map(move |suffix| format!("{prefix}-{suffix}"))
            })),
        }
    }

    fn extension_part_parts(n: u8) -> Box<dyn Iterator<Item = String>> {
        match n {
            0 => Box::new(["a", "1"].into_iter().map(ToString::to_string)),
            n => Box::new(extension_part_parts(n - 1).flat_map(|prefix| {
                [
                    "ab", "abc", "abcd", "abcde", "abcdefg", "abcdefgh", "12", "123", "1234",
                    "12345", "1234567", "12345678", "1b", "1b3", "1b3d", "1b3d5", "1b3d5f7",
                    "1b3d5f7h",
                ]
                .map(|suffix| format!("{prefix}-{suffix}"))
            })),
        }
    }

    fn private_uses(max: u8) -> impl Iterator<Item = String> {
        debug_assert!(max >= 1);
        (1..=max).flat_map(private_use_parts)
    }

    fn private_use_parts(n: u8) -> Box<dyn Iterator<Item = String>> {
        match n {
            0 => Box::new(once("-x".to_string())),
            n => Box::new(private_use_parts(n - 1).flat_map(|prefix| {
                [
                    "a", "ab", "abc", "abcd", "abcde", "abcdefg", "abcdefgh", "1", "12", "123",
                    "1234", "12345", "1234567", "12345678", "1b", "1b3", "1b3d", "1b3d5",
                    "1b3d5f7", "1b3d5f7h",
                ]
                .map(|suffix| format!("{prefix}-{suffix}"))
            })),
        }
    }

    /// An array of valid TAGs
    pub const GRANDFATHERED_TAGS: &[&str] = &[
        // irregular grandfathered
        "en-GB-oed",
        "i-ami",
        "i-bnn",
        "i-default",
        "i-enochian",
        "i-hak",
        "i-klingon",
        "i-lux",
        "i-mingo",
        "i-navajo",
        "i-pwn",
        "i-tao",
        "i-tay",
        "i-tsu",
        "sgn-BE-FR",
        "sgn-BE-NL",
        "sgn-CH-DE",
        // regular grandfathered
        "art-lojban",
        "cel-gaulish",
        "no-bok",
        "no-nyn",
        "zh-guoyu",
        "zh-hakka",
        "zh-min",
        "zh-min-nan",
        "zh-xiang",
    ];

    /// An array of valid TAGs
    pub const INVALID_TAGS: &[&str] = &[
        "",          // empty
        " ",         // space
        "12",        // invalid characters
        "a@",        // invalid characters
        "éh",        // invalid characters
        "a",         // too short
        "abcdefghi", // too long
        // wrong ordering
        "ab-ab-abc",
        "ab-ab-abcd",
        "ab-123-abc",
        "ab-123-abcd",
        "ab-abcd-abc",
        "ab-1bcd-ab",
        "ab-1bcd-abc",
        "ab-1bcd-123",
        "ab-1bcd-abcd",
        "ab-abcde-ab",
        "ab-abcde-abc",
        "ab-abcde-123",
        "ab-abcde-abcd",
        "ab-a-b",
        "abcd-abc",
    ];
}
