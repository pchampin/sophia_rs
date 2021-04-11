//! Traits and wrapper for Turtle/SPARQL prefixes.
use regex::Regex;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::ops::Deref;
use thiserror::Error;

lazy_static::lazy_static! {
    /// Match an absolute IRI reference.
    pub(crate) static ref PN_PREFIX: Regex = Regex::new(r"(?x)^
        # PN_CHAR_BASE
        [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}]
        (
            # [ PN_CHARS | '.' ]*
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}.-]*
            # PN_CHARS
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}-]
        )?
    $").unwrap();
}

/// Check whether a `str` is a valid Turtle/SPARQL prefix (matches ON_PREFIX)
pub fn is_valid_prefix(txt: &str) -> bool {
    txt.is_empty() || PN_PREFIX.is_match(txt)
}

//

/// Marker trait guaranteeing that the underlying `str` is a valid Turtle/SPARQL prefix.
pub trait IsPrefix: Borrow<str> {}

//

/// Automatic trait for [`IsPrefix`], providing cheap conversion to [`Prefix`].
pub trait AsPrefix {
    /// Extract an [`Prefix`] wrapping the underlying `str`.
    fn as_prefix(&self) -> Prefix;
}

impl<T: IsPrefix> AsPrefix for T {
    fn as_prefix(&self) -> Prefix {
        Prefix::new_unchecked(self.borrow())
    }
}

//

/// A `str` satisfying the `PN_PREFIX?` rule in Turtle/SPARQL.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Prefix<'a>(&'a str);

impl<'a> Prefix<'a> {
    /// Build new `Prefix` from trusted `str`
    pub fn new_unchecked(prefix: &'a str) -> Self {
        Prefix(prefix)
    }

    /// Build new `Prefix` from `str`, checking that it is valid.
    pub fn new(prefix: &'a str) -> Result<Self, InvalidPrefix> {
        if is_valid_prefix(prefix) {
            Err(InvalidPrefix(prefix.to_string()))
        } else {
            Ok(Prefix(prefix))
        }
    }

    /// Convert to an `PrefixBox`
    pub fn boxed(self) -> PrefixBox {
        PrefixBox::new_unchecked(Box::from(self.0))
    }
}

impl<'a> IsPrefix for Prefix<'a> {}

impl<'a> AsRef<str> for Prefix<'a> {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl<'a> Borrow<str> for Prefix<'a> {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl<'a> Deref for Prefix<'a> {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl<'a> PartialEq<str> for Prefix<'a> {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'a> PartialEq<Prefix<'a>> for str {
    fn eq(&self, other: &Prefix<'a>) -> bool {
        self == other.0
    }
}

impl<'a> PartialOrd<str> for Prefix<'a> {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

impl<'a> PartialOrd<Prefix<'a>> for str {
    fn partial_cmp(&self, other: &Prefix<'a>) -> Option<Ordering> {
        self.partial_cmp(other.0)
    }
}

//

/// A `Box<str>` satisfying the `PN_PREFIX?` rule in Turtle/SPARQL.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PrefixBox(Box<str>);

impl PrefixBox {
    /// Build new `Prefix` from trusted `Box<str>`
    pub fn new_unchecked(prefix: Box<str>) -> Self {
        PrefixBox(prefix)
    }

    /// Build new `Prefix` from `Box<str>`, checking that it is valid.
    ///
    /// NB: to build a new PrefixBox from a `&str` `txt`, the recommended way is:
    ///   `Prefix::new(txt)?.boxed()`
    /// as this will wait until the data is checked to allocate it.
    pub fn new(prefix: Box<str>) -> Result<Self, InvalidPrefix> {
        if is_valid_prefix(&prefix) {
            Err(InvalidPrefix(prefix.to_string()))
        } else {
            Ok(PrefixBox(prefix))
        }
    }
}

impl IsPrefix for PrefixBox {}

impl Borrow<str> for PrefixBox {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for PrefixBox {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for PrefixBox {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl PartialEq<str> for PrefixBox {
    fn eq(&self, other: &str) -> bool {
        &*self.0 == other
    }
}

impl PartialEq<PrefixBox> for str {
    fn eq(&self, other: &PrefixBox) -> bool {
        self == &*other.0
    }
}

impl PartialOrd<str> for PrefixBox {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        (*self.0).partial_cmp(other)
    }
}

impl PartialOrd<PrefixBox> for str {
    fn partial_cmp(&self, other: &PrefixBox) -> Option<Ordering> {
        self.partial_cmp(&*other.0)
    }
}

//

/// This error is raised when trying to parse an invalid IRI.
#[derive(Debug, Error)]
#[error("The given prefix '{0}' does not match PN_PREFIX?")]
pub struct InvalidPrefix(pub String);

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case(""; "empty")]
    #[test_case("a")]
    #[test_case("foo")]
    #[test_case("é.hê"; "with dot and accents")]
    fn valid_prefix(p: &str) {
        assert!(is_valid_prefix(p));
    }

    #[test_case(" "; "space")]
    #[test_case("1a")]
    #[test_case("a."; "ending with dot")]
    fn invalid_prefix(p: &str) {
        assert!(!is_valid_prefix(p));
    }
}
