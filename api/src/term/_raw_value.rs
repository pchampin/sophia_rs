//! Implementation of the so-called [`RawValue`](./struct.RawValue.html).

use mownstr::MownStr;
use std::hash::{Hash, Hasher};

/// A raw value is a string possibly split into two parts.
///
/// The second part of the raw value is intended for some implementations of
/// IRIs, storing both a "namespace" and a "suffix". For other kinds of term,
/// the second part is by default `None`.
#[derive(Debug, Copy, Clone, Eq)]
pub struct RawValue<'a>(&'a str, Option<&'a str>);

impl<'a> RawValue<'a> {
    /// A new raw value from namespace and suffix.
    ///
    /// To create a raw value without suffix use `From<&str>` implementation.
    pub fn new(ns: &'a str, suffix: &'a str) -> Self {
        RawValue(ns, Some(suffix))
    }
    /// If this represents an IRI, check if its an absolute one.
    ///
    /// _Note:_ This function uses a heuristic that if the string contains a
    /// colon `:` this might be a schema and therefore an absolute IRI. In
    /// fact, there is no check that `self` represents a valid IRI.
    pub fn is_absolute(&self) -> bool {
        match str_absolute(self.0) {
            Absolute::Maybe => self
                .1
                .map(|txt| str_absolute(txt) == Absolute::Yes)
                .unwrap_or(false),
            abs => abs != Absolute::No,
        }
    }
    /// Iterator over all bytes of the raw value.
    pub fn bytes(&'a self) -> impl 'a + Iterator<Item = u8> {
        self.0.bytes().chain(self.1.iter().flat_map(|s| s.bytes()))
    }
    pub fn len(&self) -> usize {
        self.0.len() + self.1.map(str::len).unwrap_or_default()
    }
}

impl<'a> From<RawValue<'a>> for MownStr<'a> {
    fn from(raw: RawValue<'a>) -> Self {
        match raw.1 {
            None => MownStr::from(raw.0),
            Some(suffix) => {
                let mut s = String::with_capacity(raw.0.len() + suffix.len());
                s.push_str(raw.0);
                s.push_str(suffix);
                s.into()
            }
        }
    }
}

impl<'a> PartialEq for RawValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.bytes().zip(other.bytes()).all(|(s, o)| s == o)
    }
}

impl<'a> Hash for RawValue<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.0.as_bytes());
        if let Some(txt) = self.1 {
            state.write(txt.as_bytes());
        }
        state.write_u8(0xff); // this is what <str as Hash>::hash() does
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Absolute {
    No,
    Maybe,
    Yes,
}

#[inline]
fn str_absolute(txt: &str) -> Absolute {
    if txt.is_empty() {
        return Absolute::Maybe;
    }
    for b in txt.bytes() {
        if b == b':' {
            return Absolute::Yes;
        }
        if !(b'A' <= b && b <= b'Z'
            || b'a' <= b && b <= b'z'
            || b'0' <= b && b <= b'9'
            || b == b'.'
            || b == b'+'
            || b == b'-')
        {
            return Absolute::No;
        }
    }
    Absolute::Maybe
}
