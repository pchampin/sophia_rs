//! IRIs for identifying resources like specified in
//! [RDF](https://www.w3.org/TR/rdf11-primer/#section-blank-node).
//!
//! IRIs themselves are specified in
//! [RFC3987](https://tools.ietf.org/html/rfc3987).
//!

use std::borrow::Cow;
use std::fmt;
use std::hash::{Hash, Hasher};

use super::iri_rfc3987::{is_absolute_iri_ref, is_relative_iri_ref};
use super::{Result, TermData, TermError};

/// Normalization policies are used to ensure that
/// IRIs are represented in a given format.
///
/// They are applied by copying terms with
/// [`Term::normalized`](enum.Term.html#method.normalized_with).
#[derive(Clone, Copy)]
pub enum Normalization {
    /// IRIs are represented as a single string (`ns`) with an empty `suffix`.
    NoSuffix,
    /// IRIs are represented with a prefix `ns` extending to the last hash (`#`) or slash (`/`),
    /// and a `suffix` containing the remaining characters.
    LastHashOrSlash,
}

/// Representation of an IRI.
///
/// May be encountered when pattern-matching on [`Term`](enum.Term.html)s
/// of the [`Iri`](enum.Term.html#variant.Iri) variant.
/// For that purpose, note that `Iri`
///  - can be directly compared to a `&str` with the `==` operator;
///  - can be directly compared to a [`Term`](enum.Term.html) with the `==` operator;
///  - provides some identical methods to what `&str` provides (see below);
///  - can otherwise be converted to a `String` with `to_string`;
///
/// # Contract
///
/// Each `Iri` represents a valid IRI according to the
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
    /// # Safety
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    pub unsafe fn new_unchecked<U>(iri: U, absolute: bool) -> Self
    where
        TD: From<U>,
    {
        Iri {
            ns: iri.into(),
            suffix: None,
            absolute,
        }
    }

    /// Create a new IRI-term from a given namespace and suffix.
    ///
    /// As it is not checked if absolute or relative this property must be
    /// entered as well.
    ///
    /// # Safety
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    pub unsafe fn new_suffixed_unchecked<U, V>(ns: U, suffix: V, absolute: bool) -> Self
    where
        TD: From<U> + From<V>,
    {
        Iri {
            ns: ns.into(),
            suffix: Some(suffix.into()),
            absolute,
        }
    }

    /// The length in this IRI.
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

    /// Copy self while transforming the inner `TermData` with the given
    /// factory.
    pub fn copy_with<'a, U, F>(&'a self, mut factory: F) -> Iri<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        let suffix = self.suffix.as_ref().map(|s| factory(s.as_ref()));
        Iri {
            ns: factory(self.ns.as_ref()),
            suffix,
            absolute: self.absolute,
        }
    }

    /// Transforms the IRI according to the given policy.
    ///
    /// If the policy already applies the IRI is returned unchanged.
    pub fn normalized(&self, policy: Normalization) -> Cow<'_, Self>
    where
        TD: From<String>,
    {
        match policy {
            Normalization::NoSuffix => self.no_suffix(),
            Normalization::LastHashOrSlash => self.suffixed_at_last_hash_or_slash(),
        }
    }

    /// If the given IRI has a suffix it is appended to the namespace and a new
    /// IRI is returned else the IRI is returned unchanged.
    pub fn no_suffix(&self) -> Cow<'_, Self>
    where
        TD: From<String>,
    {
        match &self.suffix {
            Some(s) => {
                let mut full = String::with_capacity(self.len());
                full.push_str(self.ns.as_ref());
                full.push_str(s.as_ref());
                // Okay as derived from existing IRI.
                let iri = unsafe { Self::new_unchecked(full, self.absolute) };
                Cow::Owned(iri)
            }
            None => Cow::Borrowed(self),
        }
    }

    /// Separates the IRI into namespace and suffix at the last hash `#` or
    /// slash `/` in the IRI.
    ///
    /// 1. If this already applies the IRI is returned unchanged.
    /// 1. If the IRI none of the separators contains it is returned unchanged.
    /// 1. In every other case a new IRI is allocated and the policy is
    ///   applied.
    pub fn suffixed_at_last_hash_or_slash(&self) -> Cow<'_, Self>
    where
        TD: From<String>,
    {
        const SEPERATORS: &[char] = &['#', '/'];

        let ns = self.ns.as_ref();
        match &self.suffix {
            Some(suf) => {
                let suf = suf.as_ref();
                if let Some(pos) = suf.rfind(SEPERATORS) {
                    // case: suffix with separator
                    let mut new_ns = String::with_capacity(ns.len() + pos + 1);
                    new_ns.push_str(ns);
                    new_ns.push_str(&suf[..=pos]);
                    let iri = Iri {
                        ns: new_ns.into(),
                        suffix: Some(suf[pos + 1..].to_string().into()),
                        absolute: self.absolute,
                    };
                    Cow::Owned(iri)
                } else if ns.ends_with(SEPERATORS) {
                    // case: ns does end with separator
                    Cow::Borrowed(self)
                } else if let Some(pos) = ns.rfind(SEPERATORS) {
                    // case: ns does not end with separator but contains one
                    let mut new_suffix = String::with_capacity(ns.len() - pos - 1 + suf.len());
                    new_suffix.push_str(&ns[pos + 1..]);
                    new_suffix.push_str(suf);
                    let iri = Iri {
                        ns: ns[..=pos].to_string().into(),
                        suffix: Some(new_suffix.into()),
                        absolute: self.absolute,
                    };
                    Cow::Owned(iri)
                } else {
                    // case: neither contains a separator
                    Cow::Borrowed(self)
                }
            }
            None => {
                match ns.rfind(SEPERATORS) {
                    Some(pos) => {
                        // case: no suffix
                        let iri = Iri {
                            ns: ns[..=pos].to_string().into(),
                            suffix: Some(ns[pos + 1..].to_string().into()),
                            absolute: self.absolute,
                        };
                        Cow::Owned(iri)
                    }
                    None => {
                        // case: no separators
                        Cow::Borrowed(self)
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
    /// # Safety
    ///
    /// The resulting IRI may be invalid.
    pub const unsafe fn from_raw_parts(
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
            && (sf.is_empty() || sf == &other[ns_len + 1..])
    }
}

// TODO when integrated into Term
// impl<T, U> PartialEq<Term<U>> for Iri<T>
// where
//     T: AsRef<str>,
//     U: TermData,
// {
//     #[inline]
//     fn eq(&self, other: &Term<U>) -> bool {
//         match other {
//             Iri(other_iri) => other_iri == self,
//             _ => false,
//         }
//     }
// }

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

impl<TD> fmt::Display for Iri<TD>
where
    TD: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ns = self.ns.as_ref();
        let suffix = self.suffix_as_str();
        write!(f, "{}{}", ns, suffix)
    }
}

// TODO
// impl<'a> IriRefStructure<'a> {
//     pub fn join_iri<T>(&self, iri_term: &Iri<T>) -> Iri<T>
//     where
//         T: AsRef<str> + Clone + From<String>,
//     {
//         let parsed_ns = IriRefStructure::new(iri_term.ns.as_ref()).unwrap();
//         let abs_ns = T::from(self.join(&parsed_ns).to_string());
//         Iri {
//             ns: abs_ns,
//             suffix: iri_term.suffix.clone(),
//             absolute: true,
//         }
//     }
// }

#[cfg(test)]
mod test {
    // TODO test match_ns()
}
