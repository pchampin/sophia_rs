//! Implementation of IRI resolution as per
//! [\[RFC 3987\]](https://tools.ietf.org/html/rfc3987).
//!
//! This module is based on <https://docs.rs/oxiri/>.
//!
//! NB: compared to [`Iri`] and [`IriRef`],
//! [`BaseIri`] and [`BaseIriRef`] are slower to build,
//! because they analyse the internal structure of the IRI,
//! in order to allow for efficient resolution of relative IRIs.

use super::{Iri, IriRef, IsIri, IsIriRef};
use std::borrow::Borrow;
use std::ops::Deref;

pub use oxiri::IriParseError;
pub use oxiri::{Iri as Oxiri, IriRef as OxiriRef};

/// A `BaseIri` is an absolute IRI against which relative IRIs can be resolved.
/// It stores the internal structure of the IRI,
/// to allow for efficient resolution of relative IRIs against itself.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BaseIri<T>(Oxiri<T>);

impl<T: Deref<Target = str>> IsIriRef for BaseIri<T> {}
impl<T: Deref<Target = str>> IsIri for BaseIri<T> {}

impl<T: Deref<Target = str>> BaseIri<T> {
    /// Creates a new `BaseIri` if `iri` is a valid IRI,
    /// otherwise returns an [`IriParseError`].
    pub fn new(iri: T) -> Result<Self, IriParseError> {
        Oxiri::parse(iri).map(BaseIri)
    }

    /// Resolves `iri` against this `BaseIri`.
    pub fn resolve<R: Resolvable<String>>(&self, iri: R) -> R::OutputAbs {
        R::output_abs(self.0.resolve(iri.borrow()).map(Oxiri::into_inner))
    }

    /// Resolves `iri` against this `BaseIri`, using `buf` to store the result.
    pub fn resolve_into<'a, R: Resolvable<&'a str>>(
        &self,
        iri: R,
        buf: &'a mut String,
    ) -> R::OutputAbs {
        R::output_abs(self.0.resolve_into(iri.borrow(), buf).map(|()| &buf[..]))
    }
}

impl<T: Deref<Target = str>> Borrow<str> for BaseIri<T> {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl<T: Deref<Target = str>> Deref for BaseIri<T> {
    type Target = Oxiri<T>;
    fn deref(&self) -> &Oxiri<T> {
        &self.0
    }
}

//

/// A `BaseIriRef` is an absolute or relative IRI reference,
/// against which relative IRIs can be resolved.
/// It stores the internal structure of the IRI,
/// to allow for efficient resolution of relative IRIs against itself.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BaseIriRef<T>(OxiriRef<T>);

impl<T: Deref<Target = str>> IsIriRef for BaseIriRef<T> {}

impl<T: Deref<Target = str>> BaseIriRef<T> {
    /// Creates a new `BaseIriRef` if `iri` is a valid IRI,
    /// otherwise returns an [`IriParseError`].
    pub fn new(iri: T) -> Result<Self, IriParseError> {
        OxiriRef::parse(iri).map(BaseIriRef)
    }

    /// Resolves `iri` against this `BaseIriRef`.
    pub fn resolve<R: Resolvable<String>>(&self, iri: R) -> R::OutputRel {
        R::output_rel(self.0.resolve(iri.borrow()).map(OxiriRef::into_inner))
    }

    /// Resolves `iri` against this `BaseIriRef`, using `buf` to store the result.
    pub fn resolve_into<'a, R: Resolvable<&'a str>>(
        &self,
        iri: R,
        buf: &'a mut String,
    ) -> R::OutputRel {
        R::output_rel(self.0.resolve_into(iri.borrow(), buf).map(|()| &buf[..]))
    }

    /// Convert this to a [`BaseIri`].
    ///
    /// # Precondition
    /// This [`BaseIriRef`] must be [absolute](OxiriRef::is_absolute)
    pub fn to_base_iri(self) -> BaseIri<T> {
        assert!(self.is_absolute());
        BaseIri(Oxiri::try_from(self.0).unwrap())
    }
}

impl<T: Deref<Target = str>> Borrow<str> for BaseIriRef<T> {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl<T: Deref<Target = str>> Deref for BaseIriRef<T> {
    type Target = OxiriRef<T>;
    fn deref(&self) -> &OxiriRef<T> {
        &self.0
    }
}

//

/// A trait for anything that can be resolved against a
/// [`BaseIri`](BaseIri::resolve) or a [`BaseIriRef`](BaseIriRef::resolve).
pub trait Resolvable<T: Borrow<str>>: Borrow<str> {
    /// The output type when joining to an absolute base.
    type OutputAbs;
    /// The output type when joining to an relative base.
    type OutputRel;
    /// Method for producing the `Self::OutputAbs` from a raw result.
    fn output_abs(res: Result<T, IriParseError>) -> Self::OutputAbs;
    /// Method for producing the `Self::OutputRel` from a raw result.
    fn output_rel(res: Result<T, IriParseError>) -> Self::OutputRel;
}

impl<T: Borrow<str>> Resolvable<T> for &str {
    type OutputAbs = Result<Iri<T>, IriParseError>;
    type OutputRel = Result<IriRef<T>, IriParseError>;
    fn output_abs(res: Result<T, IriParseError>) -> Self::OutputAbs {
        res.map(|iri| Iri::new_unchecked(iri))
    }
    fn output_rel(res: Result<T, IriParseError>) -> Self::OutputRel {
        res.map(|iri| IriRef::new_unchecked(iri))
    }
}

impl<T: Borrow<str>, U: IsIriRef> Resolvable<T> for U {
    type OutputAbs = Iri<T>;
    type OutputRel = IriRef<T>;
    fn output_abs(res: Result<T, IriParseError>) -> Self::OutputAbs {
        Iri::new_unchecked(res.unwrap())
    }
    fn output_rel(res: Result<T, IriParseError>) -> Self::OutputRel {
        IriRef::new_unchecked(res.unwrap())
    }
}

//

#[cfg(test)]
mod test {
    use super::*;
    use crate::AsIriRef;
    use crate::test::*;

    #[test]
    fn positive() {
        for (txt, parsed) in POSITIVE_IRIS {
            let bir = BaseIriRef::new(*txt).unwrap();
            assert_eq!(bir.is_absolute(), parsed.0);
            assert_eq!(bir.scheme(), parsed.1);
            assert_eq!(bir.authority(), parsed.2);
            assert_eq!(bir.path(), parsed.3);
            assert_eq!(bir.query(), parsed.4);
            assert_eq!(bir.fragment(), parsed.5);
            assert_eq!(bir.to_string(), *txt);

            assert_eq!(bir, IriRef::new(*txt).unwrap().to_base());
            assert_eq!(bir, IriRef::new(*txt).unwrap().as_base());

            let rbi = BaseIri::new(*txt);
            if parsed.0 {
                assert!(rbi.is_ok(), "<{txt}> → {rbi:?}");
                let bi = rbi.unwrap();
                assert_eq!(bi.scheme(), parsed.1.unwrap());
                assert_eq!(bi.authority(), parsed.2);
                assert_eq!(bi.path(), parsed.3);
                assert_eq!(bi.query(), parsed.4);
                assert_eq!(bi.fragment(), parsed.5);
                assert_eq!(bi.to_string(), *txt);

                assert_eq!(bi.as_iri_ref(), bir.as_iri_ref());
                assert_eq!(bi, Iri::new(*txt).unwrap().to_base());
                assert_eq!(bi, Iri::new(*txt).unwrap().as_base());
            } else {
                assert!(rbi.is_err(), "<{txt}> → {rbi:?}");
            }
        }
    }

    #[test]
    fn negative() {
        for txt in NEGATIVE_IRIS {
            let rpir = BaseIriRef::new(*txt);
            assert!(rpir.is_err(), "<{txt}> → {rpir:?}");
            let rpi = BaseIri::new(*txt);
            assert!(rpi.is_err(), "<{txt}> → {rpi:?}");
        }
    }

    #[test]
    fn relative() {
        for (rel, abs) in RELATIVE_IRIS {
            let rbir = BaseIriRef::new(*rel);
            assert!(rbir.is_ok(), "<{rel}> → {rbir:?}");

            let rbi = BaseIri::new(*rel);
            if rel != abs {
                assert!(rbi.is_err(), "<{rel}> → {rbi:?}");
            } else {
                assert!(rbi.is_ok(), "<{rel}> → {rbi:?}");
                assert_eq!(rbir.unwrap().as_iri_ref(), rbi.unwrap().as_iri_ref());
            }
        }
    }

    #[test]
    fn resolve_iri_parsed() {
        let base1 = BaseIriRef::new("http://a/b/c/d;p?q").unwrap();
        let base2: BaseIri<_> = base1.clone().to_base_iri();
        let mut buf = String::new();
        for (rel, abs) in RELATIVE_IRIS {
            let rel = IriRef::new(*rel).unwrap();
            let got1a = base1.resolve(rel);
            assert_eq!(&got1a, *abs);
            buf.clear();
            let got1b = base1.resolve_into(rel, &mut buf);
            assert_eq!(&got1b, *abs);
            let got2 = base2.resolve(rel);
            assert_eq!(&got2, *abs);
            buf.clear();
            let got2b = base2.resolve_into(rel, &mut buf);
            assert_eq!(&got2b, *abs);
        }
    }

    #[test]
    fn resolve_str() {
        let base1 = BaseIriRef::new("http://a/b/c/d;p?q").unwrap();
        let base2: BaseIri<_> = base1.clone().to_base_iri();
        let mut buf = String::new();
        for (rel, abs) in RELATIVE_IRIS {
            let got1a = base1.resolve(*rel).unwrap();
            assert_eq!(&got1a, *abs);
            buf.clear();
            let got1b = base1.resolve_into(*rel, &mut buf).unwrap();
            assert_eq!(&got1b, *abs);
            let got2a = base2.resolve(*rel).unwrap();
            assert_eq!(&got2a, *abs);
            buf.clear();
            let got2b = base2.resolve_into(*rel, &mut buf).unwrap();
            assert_eq!(&got2b, *abs);
        }
    }

    #[test]
    fn resolve_bad_str() {
        let base1 = BaseIriRef::new("http://a/b/c/d;p?q").unwrap();
        let base2: BaseIri<_> = base1.clone().to_base_iri();
        let mut buf = String::new();
        for txt in NEGATIVE_IRIS {
            println!("{}", *txt);
            assert!(base1.resolve(*txt).is_err());
            assert!(base2.resolve(*txt).is_err());
            assert!(base1.resolve_into(*txt, &mut buf).is_err());
            assert!(base2.resolve_into(*txt, &mut buf).is_err());
        }
    }
}
