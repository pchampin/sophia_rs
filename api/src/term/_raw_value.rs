//! Implementation of the so-called [`RawValue`](./struct.RawValue.html).

use mownstr::MownStr;
use std::hash::{Hash, Hasher};

/// A raw value is a string possibly split into two parts.
///
/// The second part of the raw value is intended for some implementations of
/// IRIs, storing both a "namespace" and a "suffix". For other kinds of term,
/// the second part must be `None`.
#[derive(Debug, Copy, Clone, Eq)]
pub struct RawValue<'a>(pub &'a str, pub Option<&'a str>);

impl<'a> RawValue<'a> {
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
            Absolute::Yes => true,
            Absolute::No => false,
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
    pub fn starts_with<B>(&self, bytes: B) -> bool
    where
        B: IntoIterator<Item = u8>,
    {
        let mut me = self.bytes();
        let mut other = bytes.into_iter();
        loop {
            match (me.next(), other.next()) {
                (_, None) => return true,
                (None, Some(_)) => return false,
                (Some(a), Some(b)) if a != b => return false,
                _ => continue,
            }
        }
    }
    pub fn slice<R>(&self, range: R) -> MownStr<'a>
    where
        R: std::ops::RangeBounds<usize> + std::slice::SliceIndex<str, Output = str>,
    {
        use std::ops::Bound::*;
        match self.1 {
            None => self.0[range].into(),
            Some(suffix) if self.0.is_empty() => suffix[range].into(),
            Some(suffix) => {
                let total_len = self.0.len() + suffix.len();
                let start = match range.start_bound() {
                    Included(n) => *n,
                    Excluded(n) => *n + 1,
                    Unbounded => 0,
                };
                let end = match range.end_bound() {
                    Included(n) => *n + 1,
                    Excluded(n) => *n,
                    Unbounded => total_len,
                };
                if end < start {
                    panic!("can not slice RawValue with end < start");
                }
                let cut_point = self.0.len();
                if end <= cut_point {
                    self.0[start..end].into()
                } else if cut_point <= start {
                    suffix[start - cut_point..end - cut_point].into()
                } else {
                    let mut buf = String::with_capacity(end - start);
                    buf.push_str(&self.0[start..]);
                    buf.push_str(&suffix[..end - cut_point]);
                    buf.into()
                }
            }
        }
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

impl<'a> From<(&'a str, Option<&'a str>)> for RawValue<'a> {
    fn from(t: (&'a str, Option<&'a str>)) -> Self {
        RawValue(t.0, t.1)
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

    #[test_case("hello", None, "hell" => true ; "only ns")]
    #[test_case("hello", None, "heaven" => false ; "not starts with")]
    #[test_case("hel", Some("lo"), "hell" => true ; "mixed")]
    #[test_case("", Some("hello"), "hell" => true ; "only suffix")]
    #[test_case("hel", Some("lo"), "helli" => false ; "tricky")]
    fn starts_with_str(ns: &str, sf: Option<&str>, txt: &str) -> bool {
        let raw = RawValue(ns, sf);
        raw.starts_with(txt.bytes())
    }

    #[test_case("hel", Some("lo"), "he", Some("ll") => true ; "tricky")]
    #[test_case("hel", Some("lo"), "he", Some("lo") => false ; "tricky2")]
    fn starts_with_raw(ns: &str, sf: Option<&str>, ns2: &str, sf2: Option<&str>) -> bool {
        let raw = RawValue(ns, sf);
        let raw2 = RawValue(ns2, sf2);
        raw.starts_with(raw2.bytes())
    }

    #[allow(unused_parens)]
    #[test_case("hello", None, 1..4 => "ell" ; "only ns")]
    #[test_case("hello", None, ..4 => "hell" ; "only ns open start")]
    #[test_case("hello", None, (1..) => "ello" ; "only ns open end")]
    #[test_case("hello", None, 1..=3 => "ell" ; "only ns inclusive")]
    #[test_case("", Some("hello"), 1..4 => "ell" ; "only suffix")]
    #[test_case("", Some("hello"), ..4 => "hell" ; "only suffix open start")]
    #[test_case("", Some("hello"), (1..) => "ello" ; "only suffix open end")]
    #[test_case("", Some("hello"), 1..=3 => "ell" ; "only suffix inclusive")]
    #[test_case("hello", Some(" world"), 1..4 => "ell" ; "in ns")]
    #[test_case("hello", Some(" world"), ..4 => "hell" ; "in ns open start")]
    #[test_case("hello", Some(" world"), 1..=3 => "ell" ; "in ns inclusive")]
    #[test_case("hello", Some(" world"), 6..9 => "wor" ; "in suffix")]
    #[test_case("hello", Some(" world"), (6..) => "world" ; "in suffix open end")]
    #[test_case("hello", Some(" world"), 6..=8 => "wor" ; "in suffix inclusive")]
    #[test_case("hel", Some("lo"), 1..4 => "ell" ; "across cut")]
    #[test_case("hel", Some("lo"), ..4 => "hell" ; "across cut open start")]
    #[test_case("hel", Some("lo"), (1..) => "ello" ; "across cut open end")]
    #[test_case("hel", Some("lo"), 1..=3 => "ell" ; "across cut inclusive")]
    fn slice<'a, R>(ns: &'a str, sf: Option<&'a str>, range: R) -> MownStr<'a>
    where
        R: std::ops::RangeBounds<usize> + std::slice::SliceIndex<str, Output = str>,
    {
        let raw = RawValue(ns, sf);
        raw.slice(range)
    }
}
