// this module is transparently re-exported by its parent `term`

use std::hash::{Hash, Hasher};
use std::io::{Result as IoResult, Write};

use self::iri_rfc3987::{is_absolute_iri, is_relative_iri};
use super::*;

/// Internal representation of an IRI.
///
/// May be encountered when pattern-matching on [`Term`](enum.Term.html)s
/// of the [`Iri`](enum.Term.html#variant.Iri) variant.
/// For that purpose, note that `IriData`
///  - can be directly compared to a `&str` with the `==` operator;
///  - can be directly compared to a [`Term`](enum.Term.html) with the `==` operator;
///  - provides some identical methods to what `&str` provides (see below);
///  - can otherwise be converted to a `String` with [`to_string`](#method.to_string);
///
/// See [module documentation](index.html)
/// for more detail.
#[derive(Clone, Copy, Debug, Eq)]
pub struct IriData<T: AsRef<str>> {
    pub(crate) ns: T,
    pub(crate) suffix: Option<T>,
    pub(crate) absolute: bool,
}

impl<T> IriData<T>
where
    T: AsRef<str>,
{
    /// The length of this IRI.
    pub fn len(&self) -> usize {
        self.ns.as_ref().len() + self.suffix_borrow().len()
    }

    /// Iterate over the bytes representing this IRI.
    pub fn bytes<'a>(&'a self) -> impl Iterator<Item = u8> + 'a {
        self.ns.as_ref().bytes().chain(self.suffix_borrow().bytes())
    }

    /// Iterate over the characters representing this IRI.
    pub fn chars<'a>(&'a self) -> impl Iterator<Item = char> + 'a {
        self.ns.as_ref().chars().chain(self.suffix_borrow().chars())
    }

    /// Construct a copy of this IRI as a `String`.
    pub fn to_string(&self) -> String {
        let ns = self.ns.as_ref();
        let suffix = self.suffix_borrow();
        let mut ret = String::with_capacity(ns.len() + suffix.len());
        ret.push_str(ns);
        ret.push_str(suffix);
        ret
    }

    /// Whether this IRI is absolute or relative.
    pub fn is_absolute(&self) -> bool {
        self.absolute
    }

    /// Write this IRI to `w`.
    pub fn write_to<W>(&self, w: &mut W) -> IoResult<()>
    where
        W: Write,
    {
        w.write_all(self.ns.as_ref().as_bytes())?;
        if let Some(ref suffix) = self.suffix {
            w.write_all(suffix.as_ref().as_bytes())?;
        }
        Ok(())
    }

    pub(crate) fn new(ns: T, suffix: Option<T>) -> Result<IriData<T>> {
        let mut ret = IriData {
            ns,
            suffix,
            absolute: false,
        };
        let val = ret.to_string();
        ret.absolute = is_absolute_iri(&val);
        if ret.absolute || is_relative_iri(&val) {
            Ok(ret)
        } else {
            Err(ErrorKind::InvalidIri("IRI is invalid".to_string()).into())
        }
    }

    pub(crate) unsafe fn new_unchecked(
        ns: T,
        suffix: Option<T>,
        absolute: Option<bool>,
    ) -> IriData<T> {
        match absolute {
            Some(absolute) => IriData {
                ns,
                suffix,
                absolute,
            },
            None => IriData::new(ns, suffix).unwrap(),
        }
    }

    pub(crate) fn from_with<'a, U, F>(other: &'a IriData<U>, mut factory: F) -> IriData<T>
    where
        U: AsRef<str>,
        F: FnMut(&'a str) -> T,
    {
        let ns = factory(other.ns.as_ref());
        let suffix = match other.suffix {
            Some(ref suffix) => Some(factory(suffix.as_ref())),
            None => None,
        };
        IriData {
            ns,
            suffix,
            absolute: other.absolute,
        }
    }

    pub(crate) fn normalized_with<'a, U, F>(
        other: &'a IriData<U>,
        factory: F,
        norm: Normalization,
    ) -> IriData<T>
    where
        U: AsRef<str>,
        F: FnMut(&str) -> T,
    {
        match norm {
            Normalization::NoSuffix => Self::no_suffix_with(other, factory),
            Normalization::LastHashOrSlash => Self::last_hash_or_slash_with(other, factory),
        }
    }

    fn no_suffix_with<'a, U, F>(other: &'a IriData<U>, mut factory: F) -> IriData<T>
    where
        U: AsRef<str>,
        F: FnMut(&str) -> T,
    {
        let ns = match other.suffix {
            Some(_) => factory(&other.to_string()),
            None => factory(other.ns.as_ref()),
        };
        IriData {
            ns,
            suffix: None,
            absolute: other.absolute,
        }
    }

    fn last_hash_or_slash_with<'a, U, F>(other: &'a IriData<U>, mut factory: F) -> IriData<T>
    where
        U: AsRef<str>,
        F: FnMut(&str) -> T,
    {
        let sep = ['#', '/'];
        let ns = other.ns.as_ref();
        let absolute = other.absolute;
        if let Some(ref suffix) = other.suffix {
            let suffix = suffix.as_ref();
            if let Some(spos) = suffix.rfind(&sep[..]) {
                let mut new_ns = String::with_capacity(ns.len() + spos + 1);
                new_ns.push_str(ns);
                new_ns.push_str(&suffix[..spos + 1]);
                IriData {
                    ns: factory(&new_ns),
                    suffix: Some(factory(&suffix[spos + 1..])),
                    absolute,
                }
            } else if let Some(npos) = ns.rfind(&sep[..]) {
                let mut new_suffix = String::with_capacity(ns.len() - npos - 1 + suffix.len());
                new_suffix.push_str(&ns[npos + 1..]);
                new_suffix.push_str(suffix);
                IriData {
                    ns: factory(&ns[..npos + 1]),
                    suffix: Some(factory(&new_suffix)),
                    absolute,
                }
            } else {
                IriData {
                    ns: factory(&other.to_string()),
                    suffix: None,
                    absolute,
                }
            }
        } else {
            if let Some(npos) = ns.rfind(&sep[..]) {
                IriData {
                    ns: factory(&ns[..npos + 1]),
                    suffix: Some(factory(&ns[npos + 1..])),
                    absolute,
                }
            } else {
                IriData {
                    ns: factory(ns),
                    suffix: None,
                    absolute,
                }
            }
        }
    }

    fn suffix_borrow(&self) -> &str {
        match self.suffix {
            Some(ref suffix) => suffix.as_ref(),
            None => "",
        }
    }
}

impl<T, U> PartialEq<IriData<U>> for IriData<T>
where
    T: AsRef<str>,
    U: AsRef<str>,
{
    fn eq(&self, other: &IriData<U>) -> bool {
        let s_ns = self.ns.as_ref();
        let s_sf = self.suffix_borrow();
        let o_ns = other.ns.as_ref();
        let o_sf = other.suffix_borrow();
        (s_ns.len() + s_sf.len()) == (o_ns.len() + o_sf.len()) && {
            let mut eq = true;
            let it1 = s_ns.chars().chain(s_sf.chars());
            let it2 = o_ns.chars().chain(o_sf.chars());
            for (c1, c2) in it1.zip(it2) {
                if c1 != c2 {
                    eq = false;
                    break;
                }
            }
            eq
        }
    }
}

impl<'a, T> PartialEq<&'a str> for IriData<T>
where
    T: AsRef<str>,
{
    fn eq(&self, other: &&'a str) -> bool {
        let s_ns = self.ns.as_ref();
        let s_sf = self.suffix_borrow();
        (s_ns.len() + s_sf.len()) == (other.len()) && {
            let mut eq = true;
            let it1 = s_ns.chars().chain(s_sf.chars());
            let it2 = other.chars();
            for (c1, c2) in it1.zip(it2) {
                if c1 != c2 {
                    eq = false;
                    break;
                }
            }
            eq
        }
    }
}

impl<T, U> PartialEq<Term<U>> for IriData<T>
where
    T: AsRef<str>,
    U: AsRef<str> + Clone + Eq + Hash,
{
    #[inline]
    fn eq(&self, other: &Term<U>) -> bool {
        match other {
            Iri(other_iri) => other_iri == self,
            _ => false,
        }
    }
}

impl<T> Hash for IriData<T>
where
    T: AsRef<str>,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write(self.ns.as_ref().as_bytes());
        state.write(self.suffix_borrow().as_bytes());
        state.write_u8(0xff);
    }
}

/// Noramlization policies are used to ensure that
/// IRIs are represented in a given format.
///
/// They are applied by copying terms with
/// [`Term::noramlized_with`](enum.Term.html#method.noramlized_with).
#[derive(Clone, Copy)]
pub enum Normalization {
    /// IRIs are represented as a single string (`ns`) with an empty `suffix`.
    NoSuffix,
    /// IRIs are represented with a prefix `ns` extending to the last hash (`#`) or slash (`/`),
    /// and a `suffix` containing the remaining characters.
    LastHashOrSlash,
}

impl<'a> ParsedIri<'a> {
    pub fn join_iri<T>(&self, iri_term: &IriData<T>) -> IriData<T>
    where
        T: AsRef<str> + Clone + From<String>,
    {
        let parsed_ns = ParsedIri::new(iri_term.ns.as_ref()).unwrap();
        let abs_ns = T::from(self.join(&parsed_ns).to_string());
        IriData {
            ns: abs_ns,
            suffix: iri_term.suffix.clone(),
            absolute: true,
        }
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the ::term::test module).
}
