//! I provide generic wrappers around `Borrow<str>` types,
//! guaranteeing that their underlying string is a valid URI or URI reference.
use super::{InvalidUri, IsUri, IsUriRef, Result};
use crate::{IsIri, IsIriRef, is_absolute_iri_ref, is_valid_iri_ref, wrap};
use std::borrow::Borrow;
use std::fmt::Display;

wrap! { Uri borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `URI` rule in  RFC-3986
    /// (i.e. an absolute URI with an optional fragment).
    pub fn new(uri: T) -> Result<Self, InvalidUri> {
        let urib = uri.borrow();
        if urib.is_ascii() && is_absolute_iri_ref(urib) {
            Ok(Uri(uri))
        } else {
            Err(InvalidUri(uri.borrow().to_string()))
        }
    }

    /// Resolve a relative URI reference against this URI.
    ///
    /// NB: when resolving multiple URI references against the same base,
    /// it is preferable to first turn it into a [`BaseIri`](crate::resolve::BaseIri),
    /// with the [`Iri::as_base`](crate::Iri::as_base) or [`Iri::to_base`](crate::Iri::to_base) methods.
    pub fn resolve<U: IsIriRef + IsUriRef>(&self, rel: U) -> Uri<String> {
        let base = self.as_ref().into_iri().to_base();
        base.resolve(rel).to_uri_unchecked()
    }

    /// Turn this URI into an [`UriRef`]
    pub fn into_uri_ref(self) -> UriRef<T> {
        UriRef::new_unchecked(self.0)
    }

    /// Turn this URI into an [`Iri`](crate::Iri)
    pub fn into_iri(self) -> crate::Iri<T> {
        crate::Iri::new_unchecked(self.0)
    }
}

impl<T: Borrow<str>> IsIriRef for Uri<T> {}
impl<T: Borrow<str>> IsIri for Uri<T> {}
impl<T: Borrow<str>> IsUriRef for Uri<T> {}
impl<T: Borrow<str>> IsUri for Uri<T> {}

impl<T: Borrow<str>> Display for Uri<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow())
    }
}

//

wrap! { UriRef borrowing str :
    /// This wrapper guarantees that the underlying `str`
    /// satisfies the `irelative-ref` rule in  RFC-3986
    /// (i.e. an absolute or relative URI reference).
    pub fn new(uri: T) -> Result<Self, InvalidUri> {
        let urib = uri.borrow();
        if urib.is_ascii() && is_valid_iri_ref(urib) {
            Ok(UriRef(uri))
        } else {
            Err(InvalidUri(uri.borrow().to_string()))
        }
    }

    /// Resolve a relative URI reference against this one.
    ///
    /// NB: when resolving multiple URI references against the same base,
    /// it is preferable to first turn it into a [`BaseIriRef`](crate::resolve::BaseIriRef),
    /// with the [`Iri::as_base`](crate::Iri::as_base) or [`Iri::to_base`](crate::Iri::to_base) methods.
    pub fn resolve<U: IsIriRef + IsUriRef>(&self, rel: U) -> UriRef<String> {
        let base = self.as_ref().into_iri_ref().to_base();
        base.resolve(rel).to_uri_ref_unchecked()
    }

    /// Turn this URI reference into an [`IriRef`](crate::IriRef)
    pub fn into_iri_ref(self) -> crate::IriRef<T> {
        crate::IriRef::new_unchecked(self.0)
    }
}

impl<T: Borrow<str>> IsIriRef for UriRef<T> {}
impl<T: Borrow<str>> IsUriRef for UriRef<T> {}

impl<T: Borrow<str>> Display for UriRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow())
    }
}
