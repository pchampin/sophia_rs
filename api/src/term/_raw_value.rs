//! Implementation of the so-called [`RawValue`](./struct.RawValue.html).

use mownstr::MownStr;
use std::hash::{Hash, Hasher};

/// A raw value is a string possibly split into two parts.
///
/// The second part of the raw value is intended for some implementations of
/// IRIs, storing both a "namespace" and a "suffix". For other kinds of term,
/// the second part is by default `None`.
#[derive(Debug, Copy, Clone, Eq)]
pub struct RawValue<'a>(pub &'a str, pub Option<&'a str>);

impl<'a> RawValue<'a> {
    /// A new raw value from namespace and suffix.
    ///
    /// To create a raw value without suffix use `From<&str>` implementation.
    pub fn new(ns: &'a str, suffix: &'a str) -> Self {
        RawValue(ns, Some(suffix))
    }
    /// If this represents an IRI, check if its an absolute one.
    ///
    /// _Note:_ This function merely checks that the resulting string starts
    /// with an IRI scheme. It does *not* further check the validity of the
    /// full IRI, which is supposed to be done by the TTerm implementation.
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
    /// Returns the length of the string, including the length of the second
    /// if present.
    pub fn len(&self) -> usize {
        self.0.len() + self.1.map(str::len).unwrap_or_default()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
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

impl<'a> From<&'a str> for RawValue<'a> {
    fn from(s: &'a str) -> Self {
        RawValue(s, None)
    }
}

impl<'a> PartialEq for RawValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && match (self, other) {
                (RawValue(s, None), RawValue(o, None)) => s == o,
                _ => self.bytes().zip(other.bytes()).all(|(s, o)| s == o),
            }
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

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case("hello", None, "hello" => true ; "only ns")]
    #[test_case("hello", None, "Hello" => false ; "not equal")]
    #[test_case("hel", Some("lo"), "hello" => true ; "mixed")]
    #[test_case("", Some("hello"), "hello" => true ; "only suffix")]
    fn bytes(ns: &str, sf: Option<&str>, full: &str) -> bool {
        let raw = RawValue(ns, sf);
        let res = raw.bytes().zip(full.bytes()).all(|(r, f)| r == f);
        res
    }

    #[test_case("hello", None, 5 => true ; "only ns")]
    #[test_case("hel", Some("lo"), 5 => true ; "mixed")]
    #[test_case("", Some("hello"), 5 => true ; "only suffix")]
    fn len(ns: &str, sf: Option<&str>, len: usize) -> bool {
        RawValue(ns, sf).len() == len
    }

    #[test_case("hello", None => false ; "rel only ns")]
    #[test_case("hel", Some("lo") => false ; "rel mixed")]
    #[test_case("http://example.org/hello", None => true ; "abs only ns")]
    #[test_case("http://example.org/", Some("hello") => true ; "abs mixed")]
    #[test_case("ht", Some("tp://example.org/") => true ; "abs split schema")]
    #[test_case("/test:number:thing/", Some("hello") => false ; "rel with colon")]
    fn absolute(ns: &str, sf: Option<&str>) -> bool {
        RawValue(ns, sf).is_absolute()
    }

    #[test_case("hello", None, "hello", None => true ; "only ns")]
    #[test_case("hello", None, "Hello", None => false ; "not equal")]
    #[test_case("hel", Some("lo"), "hello", None => true ; "mixed")]
    #[test_case("", Some("hello"), "hello", None => true ; "only suffix")]
    #[test_case("h", Some("ello"), "hell", Some("o") => true ; "full mixed")]
    fn eq(ns1: &str, sf1: Option<&str>, ns2: &str, sf2: Option<&str>) -> bool {
        RawValue(ns1, sf1) == RawValue(ns2, sf2)
    }
}
