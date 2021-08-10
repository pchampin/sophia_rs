//! I provide wrappers around `str` and `Box<str>`
//! guaranteeing that their underlying data is a valid IRI or IRI reference.
use super::{error::InvalidIri, IsIri, IsIriRef, *};
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::ops::Deref;

/// A `str` satisfying the `IRI` rule in RFC-3687.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Iri<'a>(&'a str);

impl<'a> Iri<'a> {
    /// Build new `Iri` from trusted `str`
    pub fn new_unchecked(iri: &'a str) -> Self {
        Iri(iri)
    }

    /// Build new `Iri` from `str`, checking that it is valid.
    pub fn new(iri: &'a str) -> Result<Self, InvalidIri> {
        if is_absolute_iri_ref(iri) {
            Ok(Iri(iri))
        } else {
            Err(InvalidIri(iri.to_string()))
        }
    }

    /// Convert to an `IriBox`
    pub fn boxed(self) -> IriBox {
        IriBox::new_unchecked(Box::from(self.0))
    }
}

impl<'a> IsIriRef for Iri<'a> {}

impl<'a> IsIri for Iri<'a> {}

impl<'a> Borrow<str> for Iri<'a> {
    fn borrow(&self) -> &str {
        self.0
    }
}

impl<'a> AsRef<str> for Iri<'a> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'a> Deref for Iri<'a> {
    type Target = str;
    fn deref(&self) -> &str {
        self.0
    }
}

impl<'a> PartialEq<str> for Iri<'a> {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'a> PartialEq<Iri<'a>> for str {
    fn eq(&self, other: &Iri<'a>) -> bool {
        self == other.0
    }
}

impl<'a> PartialOrd<str> for Iri<'a> {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

impl<'a> PartialOrd<Iri<'a>> for str {
    fn partial_cmp(&self, other: &Iri<'a>) -> Option<Ordering> {
        self.partial_cmp(other.0)
    }
}

//

/// A `str` satisfying the `IRI` rule in RFC-3687.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct IriRef<'a>(&'a str);

impl<'a> IriRef<'a> {
    /// Build new `IriRef` from trusted `str`
    pub fn new_unchecked(iri: &'a str) -> Self {
        IriRef(iri)
    }

    /// Build new `IriRef` from `str`, checking that it is valid.
    pub fn new(iri: &'a str) -> Result<Self, InvalidIri> {
        if is_valid_iri_ref(iri) {
            Ok(IriRef(iri))
        } else {
            Err(InvalidIri(iri.to_string()))
        }
    }

    /// Convert to an `IriRefBox`
    pub fn boxed(self) -> IriRefBox {
        IriRefBox::new_unchecked(Box::from(self.0))
    }
}

impl<'a> IsIriRef for IriRef<'a> {}

impl<'a> Borrow<str> for IriRef<'a> {
    fn borrow(&self) -> &str {
        self.0
    }
}

impl<'a> AsRef<str> for IriRef<'a> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl<'a> Deref for IriRef<'a> {
    type Target = str;
    fn deref(&self) -> &str {
        self.0
    }
}

impl<'a> PartialEq<str> for IriRef<'a> {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'a> PartialEq<IriRef<'a>> for str {
    fn eq(&self, other: &IriRef<'a>) -> bool {
        self == other.0
    }
}

impl<'a> PartialOrd<str> for IriRef<'a> {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        self.0.partial_cmp(other)
    }
}

impl<'a> PartialOrd<IriRef<'a>> for str {
    fn partial_cmp(&self, other: &IriRef<'a>) -> Option<Ordering> {
        self.partial_cmp(other.0)
    }
}

//

/// A `Box<str>` satisfying the `IRI-reference` rule in RFC-3687.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct IriBox(Box<str>);

impl IriBox {
    /// Build new `Iri` from trusted `str`
    pub fn new_unchecked(iri: Box<str>) -> Self {
        IriBox(iri)
    }

    /// Build new `Iri` from `Box<str>`, checking that it is valid.
    ///
    /// NB: to build a new IriBox from a `&str` `txt`, the recommended way is:
    ///   `Iri::new(txt)?.boxed()`
    /// as this will wait until the data is checked to allocate it.
    pub fn new(iri: Box<str>) -> Result<Self, InvalidIri> {
        if is_absolute_iri_ref(&iri) {
            Ok(IriBox(iri))
        } else {
            Err(InvalidIri(iri.to_string()))
        }
    }
}

impl IsIriRef for IriBox {}

impl IsIri for IriBox {}

impl Borrow<str> for IriBox {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for IriBox {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for IriBox {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl From<IriBox> for Box<str> {
    fn from(other: IriBox) -> Box<str> {
        other.0
    }
}

impl PartialEq<str> for IriBox {
    fn eq(&self, other: &str) -> bool {
        &*self.0 == other
    }
}

impl PartialEq<IriBox> for str {
    fn eq(&self, other: &IriBox) -> bool {
        self == &*other.0
    }
}

impl PartialOrd<str> for IriBox {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        (*self.0).partial_cmp(other)
    }
}

impl PartialOrd<IriBox> for str {
    fn partial_cmp(&self, other: &IriBox) -> Option<Ordering> {
        self.partial_cmp(&*other.0)
    }
}

//

/// A `Box<str>` satisfying the `IRI-reference` rule in RFC-3687.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct IriRefBox(Box<str>);

impl IriRefBox {
    /// Build new `Iri` from trusted `str`
    pub fn new_unchecked(iri: Box<str>) -> Self {
        IriRefBox(iri)
    }

    /// Build new `Iri` from `Box<str>`, checking that it is valid.
    ///
    /// NB: to build a new IriRefBox from a `&str` `txt`, the recommended way is:
    ///   `Iri::new(txt)?.boxed()`
    /// as this will wait until the data is checked to allocate it.
    pub fn new(iri: Box<str>) -> Result<Self, InvalidIri> {
        if is_valid_iri_ref(&iri) {
            Ok(IriRefBox(iri))
        } else {
            Err(InvalidIri(iri.to_string()))
        }
    }
}

impl IsIriRef for IriRefBox {}

impl Borrow<str> for IriRefBox {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for IriRefBox {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for IriRefBox {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl From<IriRefBox> for Box<str> {
    fn from(other: IriRefBox) -> Box<str> {
        other.0
    }
}

impl PartialEq<str> for IriRefBox {
    fn eq(&self, other: &str) -> bool {
        &*self.0 == other
    }
}

impl PartialEq<IriRefBox> for str {
    fn eq(&self, other: &IriRefBox) -> bool {
        self == &*other.0
    }
}

impl PartialOrd<str> for IriRefBox {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        (*self.0).partial_cmp(other)
    }
}

impl PartialOrd<IriRefBox> for str {
    fn partial_cmp(&self, other: &IriRefBox) -> Option<Ordering> {
        self.partial_cmp(&*other.0)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::*;

    #[test]
    fn iri() {
        for (txt, (abs, ..)) in POSITIVE_IRIS {
            assert!(Iri::new(txt).is_ok() == *abs)
        }
        for txt in NEGATIVE_IRIS {
            assert!(Iri::new(txt).is_err())
        }
    }

    #[test]
    fn iri_box() {
        for (txt, (abs, ..)) in POSITIVE_IRIS {
            assert!(IriBox::new(Box::from(txt as &str)).is_ok() == *abs)
        }
        for txt in NEGATIVE_IRIS {
            assert!(IriBox::new(Box::from(txt as &str)).is_err())
        }
    }

    #[test]
    fn iri_ref() {
        for (txt, _) in POSITIVE_IRIS {
            assert!(IriRef::new(txt).is_ok())
        }
        for txt in NEGATIVE_IRIS {
            assert!(IriRef::new(txt).is_err())
        }
        for (txt, _) in RELATIVE_IRIS {
            assert!(IriRef::new(txt).is_ok())
        }
    }

    #[test]
    fn iri_ref_box() {
        for (txt, _) in POSITIVE_IRIS {
            assert!(IriRefBox::new(Box::from(txt as &str)).is_ok())
        }
        for txt in NEGATIVE_IRIS {
            assert!(IriRefBox::new(Box::from(txt as &str)).is_err())
        }
        for (txt, _) in RELATIVE_IRIS {
            assert!(IriRefBox::new(Box::from(txt as &str)).is_ok())
        }
    }
}
