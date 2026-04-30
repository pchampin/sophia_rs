//! URI related traits.
use super::{Uri, UriRef};
use std::borrow::Borrow;

/// Marker trait guaranteeing that the underlying `str` is a valid URI
/// (i.e. absolute or relative, with an optional fragment identifier)
pub trait IsUriRef: Borrow<str> {}

/// Marker trait guaranteeing that the underlying `str` is a valid URI-reference
/// (i.e. absolute, with an optional fragment identifier)
pub trait IsUri: IsUriRef {}

/// Automatic trait for [`IsUriRef`], providing cheap conversion to [`UriRef`].
pub trait AsUriRef {
    /// Extract an [`UriRef`] wrapping the underlying `str`.
    fn as_uri_ref(&self) -> UriRef<&str>;
}

#[diagnostic::do_not_recommend]
impl<T: IsUriRef> AsUriRef for T {
    fn as_uri_ref(&self) -> UriRef<&str> {
        UriRef::new_unchecked(self.borrow())
    }
}

/// Automatic trait for [`IsUri`], providing cheap conversion to [`Uri`].
pub trait AsUri {
    /// Extract an [`Uri`] wrapping the underlying `str`.
    fn as_uri(&self) -> Uri<&str>;
}

#[diagnostic::do_not_recommend]
impl<T: IsUri> AsUri for T {
    fn as_uri(&self) -> Uri<&str> {
        Uri::new_unchecked(self.borrow())
    }
}
