//! IRIs for identifying resources like specified in
//! [RDF](https://www.w3.org/TR/rdf11-primer/#section-IRI).
//!
//! IRIs themselves are specified in
//! [RFC3987](https://tools.ietf.org/html/rfc3987).
//!
//! # Naming
//!
//! > According to RFC3987, an IRI is always absolute. An IRI reference, on the
//! > other hand, can be absolute or relative (and an absolute IRI reference
//! > happens to be an IRI).
//!
//! In most cases when the documentation talks about IRIs it actually
//! refers to IRI references. Only if IRI reference, absolute IRI or relative
//! IRI are explicitly mentioned does the difference matter.
//!

mod _regex;
pub use self::_regex::*;
mod _join;
pub use self::_join::*;

use super::{Result, Term, TermData, TermError};
use crate::mown_str::MownStr;
use crate::ns::Namespace;
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io;

/// According to [RFC3987](https://tools.ietf.org/html/rfc3987#section-2.2):
/// `gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "@"`
pub const GEN_DELIMS: &[char] = &[':', '/', '?', '#', '[', ']', '@'];

/// Normalization policies are used to ensure that
/// IRIs are represented in a given format.
///
/// They are applied by copying terms with
/// [`Term::normalized`](../enum.Term.html#method.normalized_with).
#[derive(Clone, Copy)]
pub enum Normalization {
    /// IRIs are represented as a single string (`ns`) with an empty `suffix`.
    NoSuffix,
    /// IRIs are represented with a prefix `ns` extending to the last
    /// [`gen-delim`](./constant.GEN_DELIMS.html) and a `suffix` containing the
    /// remaining characters.
    LastGenDelim,
}

/// Representation of an IRI.
///
/// May be encountered when pattern-matching on [`Term`](../enum.Term.html)s
/// of the [`Iri`](../enum.Term.html#variant.Iri) variant.
/// For that purpose, note that `Iri`
///  - can be directly compared to a `&str` with the `==` operator;
///  - can be directly compared to a [`Term`](../enum.Term.html) with the `==` operator;
///  - provides some identical methods to what `&str` provides (see below);
///  - can otherwise be converted to a `String` with `to_string`;
///
/// # Contract
///
/// Each `Iri` represents a valid IRI reference according to the
/// [RFC3987](https://tools.ietf.org/html/rfc3987) either relative or absolute.
/// For building static IRIs an unsafe API is exposed. These do not do anything
/// actual unsafe. Instead, they do not perform validity checks. It is the
/// obligation of the user to ensure that invocations of `*_unchecked()`
/// methods produce valid output. Note that the creation of invalid IRIs may
/// lead to unexpected errors in other places.
///
#[derive(Clone, Copy, Debug, Eq)]
pub struct Iri<TD: TermData> {
    /// The namespace of the IRI.
    ///
    /// If no suffix is provided `ns` contains the whole IRI.
    pub(crate) ns: TD,
    /// A possible suffix of the IRI.
    ///
    /// IRIs with namespace and suffix are typical derived from CURIs.
    pub(crate) suffix: Option<TD>,
    /// Determine if its an absolute or relative IRI.
    ///
    /// Including this is an optimization as it requires the allocation of a
    /// new string to check this property (see
    /// [`new_suffixed`](#method.new_suffixed.html)).
    pub(crate) absolute: bool,
}

impl<TD> Iri<TD>
where
    TD: TermData,
{
    /// Return a new IRI-term from a given IRI.
    ///
    /// It is checked if the input is either an absolute or relative IRI. If it
    /// is none of them an error is returned.
    pub fn new<U>(iri: U) -> Result<Self>
    where
        U: AsRef<str>,
        TD: From<U>,
    {
        let absolute = is_absolute_iri_ref(iri.as_ref());
        if absolute || is_relative_iri_ref(iri.as_ref()) {
            Ok(Iri {
                ns: iri.into(),
                suffix: None,
                absolute,
            })
        } else {
            Err(TermError::InvalidIri(iri.as_ref().to_string()))
        }
    }

    /// Return a new IRI-term from a given namespace and suffix.
    ///
    /// It is checked if the input is either an absolute or relative IRI. If it
    /// is none of them an error is returned.
    ///
    /// # Performance
    ///
    /// In order to check if the concatenation of namespace and suffix results
    /// in a valid IRI a new String is allocated and namespace and suffix are
    /// copied into it.
    pub fn new_suffixed<U, V>(ns: U, suffix: V) -> Result<Self>
    where
        U: AsRef<str>,
        V: AsRef<str>,
        TD: From<U> + From<V>,
    {
        let full = format!("{}{}", ns.as_ref(), suffix.as_ref());
        let absolute = is_absolute_iri_ref(&full);
        if absolute || is_relative_iri_ref(&full) {
            Ok(Iri {
                ns: ns.into(),
                suffix: Some(suffix.into()),
                absolute,
            })
        } else {
            Err(TermError::InvalidIri(full))
        }
    }

    /// Create a new IRI-term from a given IRI without checking its validity.
    ///
    /// As it is not checked if absolute or relative this property must be
    /// entered as well.
    ///
    /// # Pre-condition
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    ///
    /// However, in `debug` builds assertions that perform checks are enabled.
    pub fn new_unchecked<U>(iri: U, absolute: bool) -> Self
    where
        TD: From<U>,
    {
        let ns: TD = iri.into();
        debug_assert!(is_valid_iri_ref(ns.as_ref()), "invalid IRI {:?}", ns.as_ref());
        debug_assert_eq!(absolute, is_absolute_iri_ref(ns.as_ref()));
        Iri {
            ns,
            suffix: None,
            absolute,
        }
    }

    /// Create a new IRI-term from a given namespace and suffix.
    ///
    /// As it is not checked if absolute or relative this property must be
    /// entered as well.
    ///
    /// # Pre-condition
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    ///
    /// However, in `debug` builds assertions that perform checks are enabled.
    pub fn new_suffixed_unchecked<U, V>(ns: U, suffix: V, absolute: bool) -> Self
    where
        TD: From<U> + From<V>,
    {
        let ns: TD = ns.into();
        let sf: TD = suffix.into();
        #[cfg(debug_assertions)]
        {
            let iri = format!("{}{}", ns.as_ref(), sf.as_ref());
            debug_assert!(is_valid_iri_ref(&iri), "invalid IRI {:?}", iri);
            debug_assert_eq!(absolute, is_absolute_iri_ref(&iri));
        }
        Iri {
            ns,
            suffix: Some(sf),
            absolute,
        }
    }

    /// The namespace of the IRI.
    ///
    /// If the IRI has no suffix this is the whole IRI.
    pub fn ns(&self) -> &TD {
        &self.ns
    }

    /// The suffix of the IRI.
    pub fn suffix(&self) -> &Option<TD> {
        &self.suffix
    }

    /// Borrow the inner contents of the IRI.
    pub fn as_ref(&self) -> Iri<&TD> {
        Iri {
            ns: &self.ns,
            suffix: self.suffix.as_ref(),
            absolute: self.absolute,
        }
    }

    /// Borrow the inner contents of the IRI as `&str`.
    pub fn as_ref_str(&self) -> Iri<&str> {
        Iri {
            ns: self.ns.as_ref(),
            suffix: self.suffix.as_ref().map(|td| td.as_ref()),
            absolute: self.absolute,
        }
    }

    /// Create a new IRI by applying `f` to the `TermData` of `self`.
    pub fn map<F, TD2>(self, f: F) -> Iri<TD2>
    where
        F: FnMut(TD) -> TD2,
        TD2: TermData,
    {
        let mut f = f;
        Iri {
            ns: f(self.ns),
            suffix: self.suffix.map(f),
            absolute: self.absolute,
        }
    }

    /// Maps the IRI using the `Into` trait.
    pub fn map_into<TD2>(self) -> Iri<TD2>
    where
        TD: Into<TD2>,
        TD2: TermData,
    {
        self.map(Into::into)
    }

    /// Clone self while transforming the inner `TermData` with the given
    /// factory.
    ///
    /// This is done in one step in contrast to calling `clone().map(factory)`.
    pub fn clone_map<'a, U, F>(&'a self, factory: F) -> Iri<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        let mut factory = factory;
        Iri {
            ns: factory(self.ns.as_ref()),
            suffix: self.suffix.as_ref().map(|td| factory(td.as_ref())),
            absolute: self.absolute,
        }
    }

    /// Apply `clone_map()` using the `Into` trait.
    pub fn clone_into<'src, U>(&'src self) -> Iri<U>
    where
        U: TermData + From<&'src str>,
    {
        self.clone_map(Into::into)
    }

    /// The length of this IRI.
    pub fn len(&self) -> usize {
        self.ns.as_ref().len() + self.suffix_as_str().len()
    }

    /// Checks if the IRI is empty
    #[deprecated(note = "An empty IRI is not a valid one")]
    pub fn is_empty(&self) -> bool {
        self.ns.as_ref().is_empty() && self.suffix_as_str().is_empty()
    }

    /// Iterate over the bytes representing this IRI.
    pub fn bytes(&self) -> impl '_ + Iterator<Item = u8> {
        self.ns.as_ref().bytes().chain(self.suffix_as_str().bytes())
    }

    /// Iterate over the characters representing this IRI.
    pub fn chars(&self) -> impl '_ + Iterator<Item = char> {
        self.ns.as_ref().chars().chain(self.suffix_as_str().chars())
    }

    /// Whether this IRI is absolute or relative.
    pub fn is_absolute(&self) -> bool {
        self.absolute
    }

    /// Whether this IRI is is composed of a whole IRI (`false`) or a namespace
    /// and a suffix (`true`).
    pub fn has_suffix(&self) -> bool {
        self.suffix.is_some()
    }

    /// Transforms the IRI according to the given policy.
    ///
    /// If the policy already applies the IRI is returned unchanged.
    pub fn clone_normalized_with<F, U>(&self, policy: Normalization, factory: F) -> Iri<U>
    where
        F: FnMut(&str) -> U,
        U: TermData,
    {
        match policy {
            Normalization::NoSuffix => self.clone_no_suffix(factory),
            Normalization::LastGenDelim => self.clone_suffixed_at_last_gen_delim(factory),
        }
    }

    /// If the given IRI has a suffix it is appended to the namespace and a new
    /// IRI is returned else the IRI is returned unchanged.
    pub fn clone_no_suffix<F, U>(&self, factory: F) -> Iri<U>
    where
        // TODO: New string is created anyway -> save copy by a second factory? (FnMut(String) -> U)
        F: FnMut(&str) -> U,
        U: TermData,
    {
        match &self.suffix {
            Some(s) => {
                let mut full = String::with_capacity(self.len());
                full.push_str(self.ns.as_ref());
                full.push_str(s.as_ref());
                // Okay as derived from existing IRI.
                let mut factory = factory;
                Iri::new_unchecked(factory(&full), self.absolute)
            }
            None => self.clone_map(factory),
        }
    }

    /// Separates the IRI into namespace and suffix at the last hash `#` or
    /// slash `/` in the IRI.
    ///
    /// 1. If this already applies the IRI is returned unchanged.
    /// 1. If the IRI none of the separators contains the no suffix policy is
    /// applied.
    /// 1. In every other case a new IRI is allocated and the policy is
    ///   applied.
    pub fn clone_suffixed_at_last_gen_delim<F, U>(&self, factory: F) -> Iri<U>
    where
        F: FnMut(&str) -> U,
        U: TermData,
    {
        let mut factory = factory;
        let ns = self.ns.as_ref();
        match &self.suffix {
            Some(suf) => {
                let suf = suf.as_ref();
                if let Some(pos) = suf.rfind(GEN_DELIMS) {
                    // case: suffix with separator
                    let mut new_ns = String::with_capacity(ns.len() + pos + 1);
                    new_ns.push_str(ns);
                    new_ns.push_str(&suf[..=pos]);
                    Iri {
                        ns: factory(&new_ns),
                        suffix: Some(factory(&suf[pos + 1..])),
                        absolute: self.absolute,
                    }
                } else if ns.ends_with(GEN_DELIMS) {
                    // case: ns does end with separator
                    self.clone_map(factory)
                } else if let Some(pos) = ns.rfind(GEN_DELIMS) {
                    // case: ns does not end with separator but contains one
                    let mut new_suffix = String::with_capacity(ns.len() - pos - 1 + suf.len());
                    new_suffix.push_str(&ns[pos + 1..]);
                    new_suffix.push_str(suf);
                    Iri {
                        ns: factory(&ns[..=pos]),
                        suffix: Some(factory(&new_suffix)),
                        absolute: self.absolute,
                    }
                } else {
                    // case: neither contains a separator
                    self.clone_no_suffix(factory)
                }
            }
            None => {
                match ns.rfind(GEN_DELIMS) {
                    Some(pos) => {
                        // case: no suffix
                        Iri {
                            ns: factory(&ns[..=pos]),
                            suffix: Some(factory(&ns[pos + 1..])),
                            absolute: self.absolute,
                        }
                    }
                    None => {
                        // case: no separators
                        self.clone_map(factory)
                    }
                }
            }
        }
    }

    /// Checks if the IRI matches a namespace.
    ///
    /// If it does the remaining suffix is returned as iterator of `char`s.
    /// This prevents an additional allocation.
    ///
    /// In case the `ns` and the IRI are the same, the function succeeds but
    /// returns an empty `Iterator`.
    pub fn match_ns<'s, U>(&'s self, ns: &Iri<U>) -> Option<impl 's + Iterator<Item = char>>
    where
        U: TermData,
    {
        if self.len() < ns.len() {
            None
        } else {
            let mut chars_s = self.chars();
            for b_ns in ns.chars() {
                match chars_s.next() {
                    Some(b_s) if b_s != b_ns => return None,
                    _ => {}
                }
            }
            Some(chars_s)
        }
    }

    /// Parses the components of the IRI.
    ///
    /// This is necessary to use the IRI as a base to resolve other IRIs.
    ///
    /// # Auxiliary buffer
    ///
    /// The buffer parameter is only required in the situation where
    /// the internal representation of this `Iri` makes it unsuitable
    /// for building an `IriParsed`
    /// (typically because it is split in an `ns` and a `suffix` part).
    /// In those situations, the buffer will be used to store the full IRI,
    /// and will be borrowed by the returned `IriParsed`.
    ///
    /// Otherwise, the buffer will not be used,
    /// the data will be borrowed directly from `self`.
    ///
    /// ## Implementation detail
    ///
    /// Currently, the buffer is used whenever the IRI is internally stored in two parts
    /// (ns and suffix), i.e. when it was created with `new_suffixed`.
    ///
    /// Technically a suffixed IRI could parsed successfully when the suffix is
    /// separating the IRI at a component. However, detecting this requires
    /// more effort. Maybe this feature will be implemented (by you?) in a
    /// future release of `sophia`.
    pub fn parse_components<'s>(&'s self, buffer: &'s mut String) -> IriParsed<'s> {
        let data = match &self.suffix {
            None => self.ns.as_ref(),
            Some(s) => {
                buffer.reserve(self.ns.as_ref().len() + s.as_ref().len() - buffer.capacity());
                buffer.push_str(self.ns.as_ref());
                buffer.push_str(s.as_ref());
                buffer.as_str()
            }
        };
        IriParsed::new(data).expect("Iri must contain a valid IRI reference")
    }

    /// Writes the IRI to the `fmt::Write` using the NTriples syntax.
    ///
    /// This means the IRI is in angled brackets and no prefix is used.
    pub fn write_fmt<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        write!(w, "<{}{}>", self.ns.as_ref(), self.suffix_as_str())
    }

    /// Writes the blank node to the `io::Write` using the N3 syntax.
    pub fn write_io<W>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        w.write_all(b"<")?;
        w.write_all(self.ns.as_ref().as_bytes())?;
        w.write_all(self.suffix_as_str().as_bytes())?;
        w.write_all(b">")
    }

    /// Return this IRI as text.
    pub fn value(&self) -> MownStr {
        match &self.suffix {
            None => self.ns.as_ref().into(),
            Some(s) => format!("{}{}", self.ns.as_ref(), s.as_ref()).into(),
        }
    }

    /// Returns either the suffix if existent or an empty string.
    fn suffix_as_str(&self) -> &str {
        match &self.suffix {
            Some(suffix) => suffix.as_ref(),
            None => "",
        }
    }
}

impl Iri<&'static str> {
    /// Build an IRI from its raw components.
    ///
    /// This constructor is used by the [`namespace!`](../macro.namespace.html) macro,
    /// but should not be used directly.
    ///
    /// # Pre-condition
    ///
    /// The resulting IRI may be invalid.
    pub const fn from_raw_parts_unchecked(
        ns: &'static str,
        suffix: Option<&'static str>,
        absolute: bool,
    ) -> Self {
        Iri {
            ns,
            suffix,
            absolute,
        }
    }
}

impl<TD> fmt::Display for Iri<TD>
where
    TD: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_fmt(f)
    }
}

impl<T, U> PartialEq<Iri<U>> for Iri<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Iri<U>) -> bool {
        self.len() == other.len() && !self.bytes().zip(other.bytes()).any(|(bs, bo)| bs != bo)
    }
}

impl<TD> PartialEq<str> for Iri<TD>
where
    TD: TermData,
{
    fn eq(&self, other: &str) -> bool {
        let ns = self.ns.as_ref();
        let sf = self.suffix_as_str();
        let ns_len = ns.len();
        let sf_len = sf.len();

        ns_len + sf_len == other.len()
            && ns == &other[..ns_len]
            // prevents panic if not suffixed
            && (sf.is_empty() || sf == &other[ns_len..])
    }
}

impl<T, U> PartialEq<Term<U>> for Iri<T>
where
    T: TermData,
    U: TermData,
{
    #[inline]
    fn eq(&self, other: &Term<U>) -> bool {
        match other {
            Term::Iri(other_iri) => other_iri == self,
            _ => false,
        }
    }
}

impl<TD> Hash for Iri<TD>
where
    TD: TermData,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.ns.as_ref().as_bytes());
        state.write(self.suffix_as_str().as_bytes());
        state.write_u8(0xff);
    }
}

impl<TD> From<Namespace<TD>> for Iri<TD>
where
    TD: TermData,
{
    fn from(ns: Namespace<TD>) -> Self {
        // Already checked if its a valid IRI
        Iri {
            absolute: is_absolute_iri_ref(ns.0.as_ref()),
            ns: ns.0,
            suffix: None,
        }
    }
}

impl<TD> TryFrom<Term<TD>> for Iri<TD>
where
    TD: TermData,
{
    type Error = TermError;

    fn try_from(term: Term<TD>) -> Result<Self, Self::Error> {
        match term {
            Term::Iri(iri) => Ok(iri),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "IRI".to_owned(),
            }),
        }
    }
}

impl<'a, T, U> TryFrom<&'a Term<U>> for Iri<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    type Error = TermError;

    fn try_from(term: &'a Term<U>) -> Result<Self, Self::Error> {
        match term {
            Term::Iri(iri) => Ok(iri.clone_map(T::from)),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "IRI".to_owned(),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[cfg(debug_assertions)]
    mod debug {
        use super::*;

        #[test]
        #[should_panic]
        fn check_unchecked_negative() {
            let _ = Iri::<&'static str>::new_unchecked("[]", true);
        }

        #[test]
        fn check_unchecked_positive() {
            let _ = Iri::<&'static str>::new_unchecked("foo", false);
        }

        #[test]
        #[should_panic]
        fn check_unchecked_suffixed_negative() {
            let _ = Iri::<&'static str>::new_suffixed_unchecked("foo#", "[]", false);
        }

        #[test]
        fn check_unchecked_suffixed_positive() {
            let _ = Iri::<&'static str>::new_suffixed_unchecked("foo#", "bar", false);
        }
    }

    #[test]
    fn map() {
        let input = Iri::new_suffixed("some/iri/", "example").unwrap();
        let expect: Iri<&str> = Iri::new("SOME/IRI/EXAMPLE").unwrap();

        let mut cnt = 0;
        let mut invoked = 0;

        let cl = input.clone_map(|s: &str| {
            cnt += s.len();
            invoked += 1;
            s.to_ascii_uppercase()
        });
        assert_eq!(cl, expect);
        assert_eq!(cnt, "some/iri/example".len());
        assert_eq!(invoked, 2);

        cnt = 0;
        invoked = 0;
        let mapped = input.map(|s: &str| {
            cnt += s.len();
            invoked += 1;
            s.to_ascii_uppercase()
        });
        assert_eq!(mapped, expect);
        assert_eq!(cnt, "some/iri/example".len());
        assert_eq!(invoked, 2);

        assert_eq!(
            cl.map_into::<Box<str>>(),
            mapped.clone_into::<std::sync::Arc<str>>()
        );
    }

    #[test]
    fn display() {
        let iri = Iri::<&'static str>::new_suffixed("foo#", "bar").unwrap();
        assert!(!iri.is_absolute());
        assert_eq!(iri.to_string(), "<foo#bar>");
        assert_eq!(iri.value(), "foo#bar");
    }

    #[test_case("http://example.org/", "http://example.org/#test" => Some("#test".to_string()) ; "simple")]
    #[test_case("../", "../" => Some("".to_string()) ; "same")]
    #[test_case("../test", "../" => None ; "inverse")]
    #[test_case("../test#", "../test#foo?bar" => Some("foo?bar".to_string()) ; "extended suffix")]
    #[test_case("../test#fo", "../test#foo" => Some("o".to_string()) ; "incomplete suffix")]
    fn match_ns(ns: &str, iri: &str) -> Option<String> {
        let ns: Iri<&str> = Iri::new(ns).unwrap();
        let iri: Iri<&str> = Iri::new(iri).unwrap();
        iri.match_ns(&ns).map(|chars| chars.collect())
    }

    #[test]
    fn convert_to_mown_does_not_allocate() {
        use crate::mown_str::MownStr;
        let iri1 = Iri::<Box<str>>::new_suffixed("http://example.org/", "foo").unwrap();
        let iri2 = iri1.clone_map(Into::into);
        let Iri { ns, suffix, .. } = iri2;
        if let MownStr::Own(_) = ns {
            assert!(false, "ns has been allocated");
        }
        if let Some(MownStr::Own(_)) = suffix {
            assert!(false, "suffix has been allocated");
        }
    }

    pub const POSITIVE_IRIS: &[(
        &str,
        (
            bool,
            Option<&str>,
            Option<&str>,
            &[&str],
            Option<&str>,
            Option<&str>,
        ),
    )] = &[
        ("http:", (true, Some("http"), None, &[], None, None)),
        (
            "http://example.org",
            (true, Some("http"), Some("example.org"), &[], None, None),
        ),
        (
            "http://127.0.0.1",
            (true, Some("http"), Some("127.0.0.1"), &[], None, None),
        ),
        (
            "http://[::]",
            (true, Some("http"), Some("[::]"), &[], None, None),
        ),
        (
            "http://%0D",
            (true, Some("http"), Some("%0D"), &[], None, None),
        ),
        (
            "http://example.org/",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                None,
                None,
            ),
        ),
        (
            "http://éxample.org/",
            (
                true,
                Some("http"),
                Some("éxample.org"),
                &["", ""],
                None,
                None,
            ),
        ),
        (
            "http://user:pw@example.org:1234/",
            (
                true,
                Some("http"),
                Some("user:pw@example.org:1234"),
                &["", ""],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/bar/baz",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "bar", "baz"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/bar/",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "bar", ""],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/bar/bàz",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "bar", "bàz"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/foo/.././/bar",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "foo", "..", ".", "", "bar"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/!$&'()*+,=:@/foo%0D",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", "!$&'()*+,=:@", "foo%0D"],
                None,
                None,
            ),
        ),
        (
            "http://example.org/?abc",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("abc"),
                None,
            ),
        ),
        (
            "http://example.org/?!$&'()*+,=:@/?\u{E000}",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("!$&'()*+,=:@/?\u{E000}"),
                None,
            ),
        ),
        (
            "http://example.org/#def",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                None,
                Some("def"),
            ),
        ),
        (
            "http://example.org/?abc#def",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("abc"),
                Some("def"),
            ),
        ),
        (
            "tag:abc/def",
            (true, Some("tag"), None, &["abc", "def"], None, None),
        ),
        ("tag:", (true, Some("tag"), None, &[], None, None)),
        ("foo", (false, None, None, &["foo"], None, None)),
        ("..", (false, None, None, &[".."], None, None)),
        (
            "//example.org",
            (false, None, Some("example.org"), &[], None, None),
        ),
        ("?", (false, None, None, &[], Some(""), None)),
        ("#", (false, None, None, &[], None, Some(""))),
        ("?#", (false, None, None, &[], Some(""), Some(""))),
        (
            "http://example.org/#Andr%C3%A9",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                None,
                Some("Andr%C3%A9"),
            ),
        ),
        (
            "http://example.org/?Andr%C3%A9",
            (
                true,
                Some("http"),
                Some("example.org"),
                &["", ""],
                Some("Andr%C3%A9"),
                None,
            ),
        ),
        (
            "?Andr%C3%A9#Andr%C3%A9",
            (
                false,
                None,
                None,
                &[],
                Some("Andr%C3%A9"),
                Some("Andr%C3%A9"),
            ),
        ),
    ];

    pub const NEGATIVE_IRIS: &[&str] = &[
        "http://[/",
        "http://a/[",
        "http://a/]",
        "http://a/|",
        "http://a/ ",
        "http://a/\u{E000}",
        "[",
        "]",
        "|",
        " ",
        "\u{E000}",
    ];

    pub const RELATIVE_IRIS: &[(&str, &str)] = &[
        // all relative iris are resolved against http://a/b/c/d;p?q
        // normal examples from https://tools.ietf.org/html/rfc3986#section-5.4.1
        ("g:h", "g:h"),
        ("g", "http://a/b/c/g"),
        ("./g", "http://a/b/c/g"),
        ("g/", "http://a/b/c/g/"),
        ("/g", "http://a/g"),
        ("//g", "http://g"),
        ("?y", "http://a/b/c/d;p?y"),
        ("g?y", "http://a/b/c/g?y"),
        ("#s", "http://a/b/c/d;p?q#s"),
        ("g#s", "http://a/b/c/g#s"),
        ("g?y#s", "http://a/b/c/g?y#s"),
        (";x", "http://a/b/c/;x"),
        ("g;x", "http://a/b/c/g;x"),
        ("g;x?y#s", "http://a/b/c/g;x?y#s"),
        ("", "http://a/b/c/d;p?q"),
        (".", "http://a/b/c/"),
        ("./", "http://a/b/c/"),
        ("..", "http://a/b/"),
        ("../", "http://a/b/"),
        ("../g", "http://a/b/g"),
        ("../..", "http://a/"),
        ("../../", "http://a/"),
        ("../../g", "http://a/g"),
        // abnormal example from https://tools.ietf.org/html/rfc3986#section-5.4.2
        ("../../../g", "http://a/g"),
        ("../../../../g", "http://a/g"),
        ("/./g", "http://a/g"),
        ("/../g", "http://a/g"),
        ("g.", "http://a/b/c/g."),
        (".g", "http://a/b/c/.g"),
        ("g..", "http://a/b/c/g.."),
        ("..g", "http://a/b/c/..g"),
        ("./../g", "http://a/b/g"),
        ("./g/.", "http://a/b/c/g/"),
        ("g/./h", "http://a/b/c/g/h"),
        ("g/../h", "http://a/b/c/h"),
        ("g;x=1/./y", "http://a/b/c/g;x=1/y"),
        ("g;x=1/../y", "http://a/b/c/y"),
        ("g?y/./x", "http://a/b/c/g?y/./x"),
        ("g?y/../x", "http://a/b/c/g?y/../x"),
        ("g#s/./x", "http://a/b/c/g#s/./x"),
        ("g#s/../x", "http://a/b/c/g#s/../x"),
    ];
}
