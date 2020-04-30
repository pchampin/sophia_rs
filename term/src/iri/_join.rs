//! Implementation of IRIs as per
//! [\[RFC 3987\]](https://tools.ietf.org/html/rfc3987).
//!
//! This module is transparently reexported by its parent module.
//!

use super::{Iri, IRELATIVE_REF_REGEX, IRI_REGEX};
use crate::ns::Namespace;
use crate::{Literal, MownTerm, Result, Term, TermData, TermError};
use mownstr::MownStr;
use std::fmt;

/// Resolve some kind of IRI with `self` as the base.
pub trait Resolve<S, T> {
    /// Resolve relative IRI(s) somewhat contained in `other` with `self` as
    /// the base IRI.
    fn resolve(&self, other: S) -> T;
}

/// Keeps track of the different components of an IRI reference.
///
/// NB: this type does not store the actual text of the IRI reference,
/// it borrows it from one (or possibly several) external `str`s.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct IriParsed<'a> {
    scheme: Option<&'a str>,
    authority: Option<&'a str>,
    /// NB: path complies with the following rules:
    /// - does not contain the separators ('/')
    /// - its first element is "" if the path starts with '/'
    /// - its last element is "" if the path ends with a '/'
    path: Vec<&'a str>,
    query: Option<&'a str>,
    fragment: Option<&'a str>,
}

impl<'a> IriParsed<'a> {
    /// Parse the given `str` as an IRI reference,
    /// and return its inner structure (or fail with a `TermError`).
    pub fn new(txt: &'a str) -> Result<IriParsed<'a>> {
        let mut pi = IriParsed::default();
        let path: Option<&str>;
        if let Some(cap) = IRI_REGEX.captures(txt) {
            pi.scheme = cap.get(1).map(|m| m.as_str());
            pi.authority = cap.get(2).map(|m| m.as_str());
            pi.query = cap.get(6).map(|m| m.as_str());
            pi.fragment = cap.get(7).map(|m| m.as_str());
            path = cap
                .get(3)
                .or_else(|| cap.get(4))
                .or_else(|| cap.get(5))
                .map(|m| m.as_str())
                .filter(|s| !s.is_empty());
        } else if let Some(cap) = IRELATIVE_REF_REGEX.captures(txt) {
            pi.authority = cap.get(1).map(|m| m.as_str());
            pi.query = cap.get(5).map(|m| m.as_str());
            pi.fragment = cap.get(6).map(|m| m.as_str());
            path = cap
                .get(2)
                .or_else(|| cap.get(3))
                .or_else(|| cap.get(4))
                .map(|m| m.as_str())
                .filter(|s| !s.is_empty());
        } else {
            return Err(TermError::InvalidIri(txt.to_owned()));
        }
        if let Some(path) = path {
            path.split('/').for_each(|i| pi.path.push(i))
        }
        Ok(pi)
    }

    /// Return `true` if this IRI reference is absolute.
    pub fn is_absolute(&self) -> bool {
        self.scheme.is_some()
    }

    /// Resolve `other` using this IRI reference as the base.
    ///
    /// NB: the resulting `IriParsed` may borrow parts from both parts.
    pub fn join(&self, other: &IriParsed<'a>) -> IriParsed<'a> {
        let (scheme, authority, query, fragment);
        let mut path;
        if other.scheme.is_some() {
            scheme = other.scheme;
            authority = other.authority;
            path = other.path.clone();
            query = other.query;
        } else {
            scheme = self.scheme;
            if other.authority.is_some() {
                authority = other.authority;
                path = other.path.clone();
                query = other.query;
            } else {
                authority = self.authority;
                if other.path.is_empty() {
                    path = self.path.clone();
                    query = other.query.or(self.query);
                } else {
                    if other.path[0] == "" {
                        path = other.path.clone();
                    } else {
                        path = self.merged_path(&other.path);
                    }
                    query = other.query;
                }
            }
        }
        remove_dot_segments(&mut path);
        fragment = other.fragment;
        IriParsed {
            scheme,
            authority,
            path,
            query,
            fragment,
        }
    }

    /// Appends the given path to `self`'s own path.
    fn merged_path(&self, path: &[&'a str]) -> Vec<&'a str> {
        if self.authority.is_some() && self.path.is_empty() {
            // resulting path must have a leading '/'
            std::iter::once("").chain(path.iter().cloned()).collect()
        } else {
            self.path
                .iter()
                .take(self.path.len() - 1)
                .cloned()
                .chain(path.iter().cloned())
                .collect()
        }
    }
}

impl<'a, 'b> Resolve<&'a str, Result<MownStr<'a>>> for IriParsed<'b> {
    /// Resolve an IRI given as `String`.
    ///
    /// Fails if `other` is not a valid IRI.
    fn resolve(&self, other: &'a str) -> Result<MownStr<'a>> {
        let other_parsed = IriParsed::new(other)?;
        if other_parsed.is_absolute() {
            Ok(other.into())
        } else {
            Ok(self.join(&other_parsed).to_string().into())
        }
    }
}

impl<'a> Resolve<&'a IriParsed<'a>, IriParsed<'a>> for IriParsed<'a> {
    /// Just a call to `IriParsed::join()`
    fn resolve(&self, other: &'a IriParsed<'a>) -> IriParsed<'a> {
        self.join(other)
    }
}

impl<'a, 'b> Resolve<Iri<&'a str>, Iri<MownStr<'a>>> for IriParsed<'b> {
    /// Resolve the given IRI.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if `other` is suffixed.
    fn resolve(&self, other: Iri<&'a str>) -> Iri<MownStr<'a>> {
        if other.absolute {
            return other.map_into();
        }
        let mut buffer = String::new();
        let parsed = other.parse_components(&mut buffer);
        let joined = self.join(&parsed);
        Iri::new_unchecked(joined.to_string(), joined.is_absolute())
    }
}

impl<'a, 'b, TD> Resolve<&'a Iri<TD>, Iri<MownStr<'a>>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve the given IRI.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if `other` is suffixed.
    fn resolve(&self, other: &'a Iri<TD>) -> Iri<MownStr<'a>> {
        self.resolve(other.as_ref_str())
    }
}

impl<'a, 'b, TD> Resolve<&'a Namespace<TD>, Namespace<MownStr<'a>>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve the IRI of the given `Namespace`.
    fn resolve(&self, other: &'a Namespace<TD>) -> Namespace<MownStr<'a>> {
        let iri = other.0.as_ref();
        let resolved: MownStr = self.resolve(iri).expect("Is valid as from Namespace");
        Namespace(resolved)
    }
}

impl<'a, 'b, TD> Resolve<&'a Literal<TD>, Literal<MownStr<'a>>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve the data type's IRI if it is relative.
    ///
    /// Note that this only affects datatyped literals;
    /// language-tagged literals are absolute by construction.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if `other.dt()` is suffixed.
    fn resolve(&self, other: &'a Literal<TD>) -> Literal<MownStr<'a>> {
        if other.is_absolute() {
            other.clone_into()
        } else {
            Literal::new_dt(other.txt().as_ref(), self.resolve(other.dt()))
        }
    }
}

impl<'a, 'b, TD> Resolve<&'a Term<TD>, MownTerm<'a>> for IriParsed<'b>
where
    TD: TermData,
{
    /// Resolve IRIs and the IRIs of typed literals.
    ///
    /// # Performance
    ///
    /// May allocate an intermediate IRI if an IRI is suffixed.
    fn resolve(&self, other: &'a Term<TD>) -> MownTerm<'a> {
        match other {
            Term::Iri(iri) => Resolve::<_, Iri<MownStr>>::resolve(self, iri).into(),
            Term::Literal(lit) => Resolve::<_, Literal<MownStr>>::resolve(self, lit).into(),
            term => term.clone_into(),
        }
    }
}

impl fmt::Display for IriParsed<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(scheme) = self.scheme {
            write!(f, "{}:", scheme)?;
        }
        if let Some(authority) = self.authority {
            write!(f, "//{}", authority)?;
        }
        if !self.path.is_empty() {
            write!(f, "{}", self.path[0])?;
            for p in &self.path[1..] {
                write!(f, "/{}", p)?;
            }
        }
        if let Some(query) = self.query {
            write!(f, "?{}", query)?;
        }
        if let Some(fragment) = self.fragment {
            write!(f, "#{}", fragment)?;
        }
        Ok(())
    }
}

fn remove_dot_segments(path: &mut Vec<&str>) {
    if path.is_empty() {
        return;
    }
    let mut i = 0;
    let last = path[path.len() - 1];
    if last == "." || last == ".." {
        path.push("");
    }
    while i < path.len() {
        if path[i] == "." {
            path.remove(i);
        } else if path[i] == ".." {
            if i != 0 && (i != 1 || path[0] != "") {
                path.remove(i - 1);
                i -= 1;
            }
            path.remove(i);
        } else {
            i += 1;
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::test::{NEGATIVE_IRIS, POSITIVE_IRIS, RELATIVE_IRIS};
    use super::*;
    use crate::RefTerm;

    #[test]
    fn positive() {
        for (txt, parsed) in POSITIVE_IRIS {
            let rpi = IriParsed::new(txt);
            assert!(rpi.is_ok(), format!("<{}> → {:?}", txt, rpi));
            let pi = rpi.unwrap();
            assert_eq!(pi.is_absolute(), parsed.0);
            assert_eq!(pi.scheme, parsed.1);
            assert_eq!(pi.authority, parsed.2);
            assert_eq!(&pi.path[..], parsed.3);
            assert_eq!(pi.query, parsed.4);
            assert_eq!(pi.fragment, parsed.5);
            assert_eq!(&pi.to_string(), txt);
        }
    }

    #[test]
    fn negative() {
        for txt in NEGATIVE_IRIS {
            let rpi = IriParsed::new(txt);
            assert!(rpi.is_err(), format!("<{}> → {:?}", txt, rpi));
        }
    }

    #[test]
    fn relative() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = IriParsed::new(rel).unwrap();
            let got = base.join(&rel);
            assert_eq!(&got.to_string(), abs);
        }
    }

    #[test]
    fn resolve_str() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let got = base.resolve(*rel).unwrap();
            assert_eq!(got, *abs);
        }
    }

    #[test]
    fn resolve_bad_str() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for txt in NEGATIVE_IRIS {
            assert!(base.resolve(*txt).is_err());
        }
    }

    #[test]
    fn resolve_iri_parsed() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = IriParsed::new(rel).unwrap();
            let got = base.resolve(&rel);
            assert_eq!(&got.to_string(), abs);
        }
    }

    #[test]
    fn resolve_namespace() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = Namespace::<&str>::new(*rel).unwrap();
            let got = base.resolve(&rel);
            assert_eq!(&got.as_ref(), abs);
        }
    }

    #[test]
    fn resolve_iri() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = Iri::<&str>::new(*rel).unwrap();
            let got = base.resolve(&rel);
            assert_eq!(&got.value(), abs);
        }
    }

    #[test]
    fn resolve_literal() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = Iri::<&str>::new(*rel).unwrap();
            let lit = Literal::<&str>::new_dt("hello", rel);
            let got = base.resolve(&lit);
            assert_eq!(&got.dt().value(), abs);
        }
    }

    #[test]
    fn resolve_iri_term() {
        let base = IriParsed::new("http://a/b/c/d;p?q").unwrap();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = RefTerm::new_iri(*rel).unwrap();
            let got = base.resolve(&rel);
            assert_eq!(&got.value(), abs);
        }
    }
}
