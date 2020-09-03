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

mod _join;
pub use self::_join::*;

use super::*;
use mownstr::MownStr;
use sophia_api::{ns::Namespace, term::RawValue};
pub use sophia_iri::resolve::*; // prefixed with "pub" to ease transition from older versions of Sophia
pub use sophia_iri::*; // prefixed with "pub" to ease transition from older versions of Sophia
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

/// An IRI reference.
///
/// # Contract
///
/// Each `Iri` represents a valid IRI reference according to the
/// [RFC3987](https://tools.ietf.org/html/rfc3987) either relative or absolute.
/// This is checked by standard constructors (`new` and `new_suffixed`).
/// On the other hand,
/// it is the obligation of the user to ensure that invocations of `*_unchecked()`
/// methods produce valid output. Note that the creation of invalid IRIs may
/// lead to unexpected errors in other places.
///
#[derive(Clone, Copy, Debug, Eq, Ord)]
pub struct Iri<TD: TermData> {
    /// The namespace of the IRI.
    ///
    /// If no suffix is provided `ns` contains the whole IRI.
    pub(crate) ns: TD,
    /// A possible suffix of the IRI.
    ///
    /// IRIs with namespace and suffix are typical derived from CURIs.
    pub(crate) suffix: Option<TD>,
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
        if is_absolute_iri_ref(iri.as_ref()) || is_relative_iri_ref(iri.as_ref()) {
            Ok(Iri {
                ns: iri.into(),
                suffix: None,
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
        if is_absolute_iri_ref(&full) || is_relative_iri_ref(&full) {
            let suffix = if !suffix.as_ref().is_empty() {
                Some(suffix.into())
            } else {
                None
            };
            Ok(Iri {
                ns: ns.into(),
                suffix,
            })
        } else {
            Err(TermError::InvalidIri(full))
        }
    }

    /// Create a new IRI-term from a given IRI without checking its validity.
    ///
    /// # Pre-condition
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    ///
    /// However, in `debug` builds assertions that perform checks are enabled.
    pub fn new_unchecked<U>(iri: U) -> Self
    where
        TD: From<U>,
    {
        let ns: TD = iri.into();
        debug_assert!(
            is_valid_iri_ref(ns.as_ref()),
            "invalid IRI {:?}",
            ns.as_ref()
        );
        Iri { ns, suffix: None }
    }

    /// Create a new IRI-term from a given namespace and suffix.
    ///
    /// # Pre-conditions
    ///
    /// It is expected that
    ///
    /// * the resulting IRI is valid per RFC3987,
    /// * `suffix` is not the empty string
    ///   (otherwise, [`new_unchecked`](#method.new_unchecked) should be used instead).
    ///
    /// This is a contract that is generally assumed.
    /// Breaking it could result in unexpected behavior.
    /// However in `debug` mode, assertions that perform checks are enabled.
    pub fn new_suffixed_unchecked<U, V>(ns: U, suffix: V) -> Self
    where
        TD: From<U> + From<V>,
    {
        let ns: TD = ns.into();
        let sf: TD = suffix.into();
        #[cfg(debug_assertions)]
        {
            let iri = format!("{}{}", ns.as_ref(), sf.as_ref());
            debug_assert!(is_valid_iri_ref(&iri), "invalid IRI {:?}", iri);
            debug_assert!(!sf.as_ref().is_empty());
        }
        Iri {
            ns,
            suffix: Some(sf),
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
        }
    }

    /// Borrow the inner contents of the IRI as `&str`.
    pub fn as_ref_str(&self) -> Iri<&str> {
        Iri {
            ns: self.ns.as_ref(),
            suffix: self.suffix.as_ref().map(|td| td.as_ref()),
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

    /// Whether this IRI is is composed of a whole IRI (`false`) or a namespace
    /// and a suffix (`true`).
    pub fn has_suffix(&self) -> bool {
        self.suffix.is_some()
    }

    /// Return an IRI equivalent to this one,
    /// internally represented with all its data in `ns`, and an empty `suffix`.
    ///
    /// # Performances
    /// The returned IRI will borrow data from this one as much as possible,
    /// but strings may be allocated in case a concatenation is required.
    pub fn normalized(&self, policy: Normalization) -> Iri<MownStr> {
        match policy {
            Normalization::NoSuffix => self.normalized_no_suffix(),
            Normalization::LastGenDelim => self.normalized_suffixed_at_last_gen_delim(),
        }
    }

    /// Return an IRI equivalent to this one,
    /// internally represented with all its data in `ns`, and an empty `suffix`.
    ///
    /// # Performances
    /// If this IRI has an empty suffix, the returned IRI simply borrows its `ns`.
    /// Otherwise, a new string is allocated for the returned IRI.
    pub fn normalized_no_suffix(&self) -> Iri<MownStr> {
        match &self.suffix {
            Some(s) => {
                let mut full = String::with_capacity(self.len());
                full.push_str(self.ns.as_ref());
                full.push_str(s.as_ref());
                // Okay as derived from existing IRI.
                Iri {
                    ns: MownStr::from(full),
                    suffix: None,
                }
            }
            None => self.as_ref_str().map_into(),
        }
    }

    /// Return an IRI equivalent to this one,
    /// internally represented with `ns` extending to the last gen-delims characters
    /// (i.e. ":" / "/" / "?" / "#" / "[" / "]" / "@"`).
    /// and `suffix` containing the rest.
    ///
    /// # Performances
    /// The returned IRI will borrow data from this one as much as possible,
    /// but strings may be allocated in case a concatenation is required.
    pub fn normalized_suffixed_at_last_gen_delim(&self) -> Iri<MownStr> {
        let ns = self.ns.as_ref();
        match &self.suffix {
            Some(suf) => {
                let suf = suf.as_ref();
                if let Some(pos) = suf.rfind(GEN_DELIMS) {
                    // case: suffix with separator in it
                    let mut new_ns = String::with_capacity(ns.len() + pos + 1);
                    new_ns.push_str(ns);
                    new_ns.push_str(&suf[..=pos]);
                    let suffix = if pos < suf.len() - 1 {
                        Some(MownStr::from(&suf[pos + 1..]))
                    } else {
                        None
                    };
                    Iri {
                        ns: MownStr::from(new_ns),
                        suffix,
                    }
                } else if ns.ends_with(GEN_DELIMS) {
                    // case: ns does end with separator
                    self.as_ref_str().map_into()
                } else if let Some(pos) = ns.rfind(GEN_DELIMS) {
                    // case: ns does not end with separator but contains one
                    let mut new_suffix = String::with_capacity(ns.len() - pos - 1 + suf.len());
                    new_suffix.push_str(&ns[pos + 1..]);
                    new_suffix.push_str(suf);
                    Iri {
                        ns: MownStr::from(&ns[..=pos]),
                        suffix: Some(MownStr::from(new_suffix)),
                    }
                } else {
                    // case: neither contains a separator
                    let mut full = String::with_capacity(self.len());
                    full.push_str(self.ns.as_ref());
                    full.push_str(suf.as_ref());
                    // Okay as derived from existing IRI.
                    Iri {
                        ns: MownStr::from(full),
                        suffix: None,
                    }
                }
            }
            None => {
                match ns[..ns.len()].rfind(GEN_DELIMS) {
                    Some(pos) if !ns[pos + 1..].is_empty() => {
                        // case: no suffix, ns must be split
                        Iri {
                            ns: MownStr::from(&ns[..=pos]),
                            suffix: Some(MownStr::from(&ns[pos + 1..])),
                        }
                    }
                    _ => {
                        // case: no suffix, borrow ns as is
                        self.as_ref_str().map_into()
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

    /// Writes the IRI to the `io::Write` using the N3 syntax.
    pub fn write_io<W>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        w.write_all(b"<")?;
        w.write_all(self.ns.as_ref().as_bytes())?;
        w.write_all(self.suffix_as_str().as_bytes())?;
        w.write_all(b">")
    }

    /// Returns either the suffix if existent or an empty string.
    fn suffix_as_str(&self) -> &str {
        match &self.suffix {
            Some(suffix) => suffix.as_ref(),
            None => "",
        }
    }
}

impl<TD: TermData> TTerm for Iri<TD> {
    fn kind(&self) -> TermKind {
        TermKind::Iri
    }
    fn value_raw(&self) -> RawValue {
        RawValue(
            self.ns.as_ref(),
            (&self.suffix).as_ref().map(|td| td.as_ref()),
        )
    }
    fn as_dyn(&self) -> &dyn TTerm {
        self
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

impl<TD, TE> PartialEq<TE> for Iri<TD>
where
    TD: TermData,
    TE: TTerm + ?Sized,
{
    fn eq(&self, other: &TE) -> bool {
        term_eq(self, other)
    }
}

impl<TD, TE> PartialOrd<TE> for Iri<TD>
where
    TD: TermData,
    TE: TTerm + ?Sized,
{
    fn partial_cmp(&self, other: &TE) -> Option<std::cmp::Ordering> {
        Some(term_cmp(self, other))
    }
}

impl<TD> Hash for Iri<TD>
where
    TD: TermData,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        term_hash(self, state)
    }
}

impl<'a, TD> From<SimpleIri<'a>> for Iri<TD>
where
    TD: TermData + From<&'a str>,
{
    fn from(iri: SimpleIri<'a>) -> Self {
        let (ns, suffix) = iri.destruct();
        let ns = ns.into();
        let suffix = suffix.map(TD::from);
        Iri { ns, suffix }
    }
}

impl<'a> From<Iri<&'a str>> for SimpleIri<'a> {
    fn from(iri: Iri<&'a str>) -> Self {
        SimpleIri::new_unchecked(iri.ns, iri.suffix)
    }
}

impl<TD> From<Namespace<TD>> for Iri<TD>
where
    TD: TermData,
{
    fn from(ns: Namespace<TD>) -> Self {
        // Already checked if its a valid IRI
        Iri {
            ns: ns.destruct(),
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
            _ => Err(TermError::UnsupportedKind(term.to_string())),
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
            _ => Err(TermError::UnsupportedKind(term.to_string())),
        }
    }
}

impl<TD> TryCopyTerm for Iri<TD>
where
    TD: TermData + for<'x> From<&'x str>,
{
    type Error = TermError;

    fn try_copy<T>(term: &T) -> Result<Self, Self::Error>
    where
        T: TTerm + ?Sized,
    {
        if term.kind() == TermKind::Iri {
            let RawValue(ns, suffix) = term.value_raw();
            Ok(match suffix {
                None => Self::new_unchecked(ns),
                Some(suffix) => Self::new_suffixed_unchecked(ns, suffix),
            })
        } else {
            Err(TermError::UnsupportedKind(term_to_string(term)))
        }
    }
}

impl<TD> std::convert::TryFrom<Iri<TD>> for sophia_api::ns::Namespace<TD>
where
    TD: TermData,
{
    type Error = TermError;

    /// Requires that the given `Iri` has no suffix. This can be enforced with
    /// the [`clone_no_suffix()`](../iri/struct.Iri.html#method.clone_no_suffix)
    /// method.
    fn try_from(iri: Iri<TD>) -> Result<Self, Self::Error> {
        if iri.suffix().is_some() {
            Err(TermError::IsSuffixed)
        } else {
            Ok(sophia_api::ns::Namespace::new_unchecked(iri.ns))
        }
    }
}

impl<'a, TD: TermData + 'a> std::borrow::Borrow<dyn TTerm + 'a> for Iri<TD> {
    fn borrow(&self) -> &(dyn TTerm + 'a) {
        self
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
            let _ = Iri::<&'static str>::new_unchecked("[]");
        }

        #[test]
        fn check_unchecked_positive() {
            let _ = Iri::<&'static str>::new_unchecked("foo");
        }

        #[test]
        #[should_panic]
        fn check_unchecked_suffixed_negative() {
            let _ = Iri::<&'static str>::new_suffixed_unchecked("foo#", "[]");
        }

        #[test]
        fn check_unchecked_suffixed_positive() {
            let _ = Iri::<&'static str>::new_suffixed_unchecked("foo#", "bar");
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
        let iri1 = Iri::<Box<str>>::new_suffixed("http://example.org/", "foo").unwrap();
        let iri2: Iri<MownStr> = iri1.clone_into();
        let Iri { ns, suffix, .. } = iri2;
        assert!(ns.is_borrowed(), "ns has been allocated");
        if let Some(suffix) = suffix {
            assert!(suffix.is_borrowed(), "suffix has been allocated");
        }
    }

    #[test]
    fn eq_different_cut() {
        let i1 = Iri::<&str>::new("http://champin.net/#pa").unwrap();
        let i2 = Iri::<&str>::new_suffixed("http://champin.net/#", "pa").unwrap();
        let i3 = Iri::<&str>::new_suffixed("http://champin.net/", "#pa").unwrap();
        let i4 = Iri::<&str>::new_suffixed("http://champin.", "net/#pa").unwrap();
        assert_eq!(i1, i2);
        assert_eq!(h(&i1), h(&i2));
        assert_eq!(i1, i3);
        assert_eq!(h(&i1), h(&i3));
        assert_eq!(i1, i4);
        assert_eq!(h(&i1), h(&i4));
        assert_eq!(i2, i3);
        assert_eq!(h(&i2), h(&i3));
        assert_eq!(i2, i4);
        assert_eq!(h(&i2), h(&i4));
        assert_eq!(i3, i4);
        assert_eq!(h(&i3), h(&i4));
    }

    fn h<H: std::hash::Hash>(x: &H) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        x.hash(&mut hasher);
        hasher.finish()
    }
}
