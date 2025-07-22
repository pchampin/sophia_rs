//! I provide generic wrappers around `Borrow<str>` types,
//! guaranteeing that their underlying string is a valid IRI or IRI reference.
use super::resolve::{BaseIri, BaseIriRef};
use super::{InvalidIri, IsIri, IsIriRef, Result, is_absolute_iri_ref, is_valid_iri_ref, wrap};
use std::borrow::Borrow;
use std::fmt::Display;

wrap! { Iri borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `IRI` rule in  RFC-3987
    /// (i.e. an absolute IRI with an optional fragment).
    pub fn new(iri: T) -> Result<Self, InvalidIri> {
        if is_absolute_iri_ref(iri.borrow()) {
            Ok(Iri(iri))
        } else {
            Err(InvalidIri(iri.borrow().to_string()))
        }
    }

    /// Resolve a relative IRI reference against this one.
    ///
    /// NB: when resolving multiple IRI references against the same base,
    /// it is preferable to first turn it into a [`BaseIri`],
    /// with the [`Iri::as_base`] or [`Iri::to_base`] methods.
    pub fn resolve<U: IsIriRef>(&self, rel: U) -> Iri<String> {
        self.as_base().resolve(rel)
    }

    /// Borrow this IRI as a [`BaseIri`]
    /// providing more efficient and flexible resolution methods than [`Iri::resolve`].
    pub fn as_base(&self) -> BaseIri<&str> {
        BaseIri::new(self.0.borrow()).unwrap()
    }

    /// Turn this IRI into a [`BaseIri`]
    /// providing more efficient and flexible resolution methods than [`Iri::resolve`].
    pub fn to_base(self) -> BaseIri<T>
    where
        T: std::ops::Deref<Target = str>,
    {
        BaseIri::new(self.0).unwrap()
    }

    /// Turn this IRI into an [`IriRef`]
    pub fn to_iri_ref(self) -> IriRef<T> {
        IriRef::new_unchecked(self.0)
    }
}

impl<T: Borrow<str>> IsIriRef for Iri<T> {}
impl<T: Borrow<str>> IsIri for Iri<T> {}

impl<T: Borrow<str>> Display for Iri<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow())
    }
}

//

wrap! { IriRef borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `irelative-ref` rule in  RFC-3987
    /// (i.e. an absolute or relative IRI reference).
    pub fn new(iri: T) -> Result<Self, InvalidIri> {
        if is_valid_iri_ref(iri.borrow()) {
            Ok(IriRef(iri))
        } else {
            Err(InvalidIri(iri.borrow().to_string()))
        }
    }

    /// Resolve a relative IRI reference against this one.
    ///
    /// NB: when resolving multiple IRI references against the same base,
    /// it is preferable to first turn it into a [`BaseIriRef`],
    /// with the [`IriRef::as_base`] or [`IriRef::to_base`] methods.
    pub fn resolve<U: IsIriRef>(&self, rel: U) -> IriRef<String> {
        self.as_base().resolve(rel)
    }

    /// Borrow this IRI as a [`BaseIriRef`]
    /// providing more efficient and flexible resolution methods than [`IriRef::resolve`].
    pub fn as_base(&self) -> BaseIriRef<&str> {
        BaseIriRef::new(self.0.borrow()).unwrap()
    }

    /// Turn this IRI into a [`BaseIriRef`]
    /// providing more efficient and flexible resolution methods than [`IriRef::resolve`].
    pub fn to_base(self) -> BaseIriRef<T>
    where
        T: std::ops::Deref<Target = str>,
    {
        BaseIriRef::new(self.0).unwrap()
    }
}

impl<T: Borrow<str>> IsIriRef for IriRef<T> {}

impl<T: Borrow<str>> Display for IriRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow())
    }
}

//

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    #[test]
    fn iri() {
        for (txt, (abs, ..)) in POSITIVE_IRIS {
            assert!(Iri::new(*txt).is_ok() == *abs);
        }
        for txt in NEGATIVE_IRIS {
            assert!(Iri::new(*txt).is_err());
        }
    }

    #[test]
    fn iri_box() {
        for (txt, (abs, ..)) in POSITIVE_IRIS {
            assert!(Iri::new(Box::from(txt as &str)).is_ok() == *abs);
        }
        for txt in NEGATIVE_IRIS {
            assert!(Iri::new(Box::from(txt as &str)).is_err());
        }
    }

    #[test]
    fn iri_ref() {
        for (txt, _) in POSITIVE_IRIS {
            assert!(IriRef::new(*txt).is_ok());
        }
        for txt in NEGATIVE_IRIS {
            assert!(IriRef::new(*txt).is_err());
        }
        for (txt, _) in RELATIVE_IRIS {
            assert!(IriRef::new(*txt).is_ok());
        }
    }

    #[test]
    fn iri_ref_box() {
        for (txt, _) in POSITIVE_IRIS {
            assert!(IriRef::new(Box::from(txt as &str)).is_ok());
        }
        for txt in NEGATIVE_IRIS {
            assert!(IriRef::new(Box::from(txt as &str)).is_err());
        }
        for (txt, _) in RELATIVE_IRIS {
            assert!(IriRef::new(Box::from(txt as &str)).is_ok());
        }
    }

    #[test]
    fn heterogeneous_comparison() {
        let iri1 = Iri::new(String::from("http://example.com/")).unwrap();
        let iri2 = Iri::new_unchecked(iri1.as_str());
        assert_eq!(iri1, iri2);
    }
}
