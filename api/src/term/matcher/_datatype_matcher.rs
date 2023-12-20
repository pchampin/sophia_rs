use std::borrow::Borrow;

use crate::ns::NsTerm;

use super::*;

/// A [`TermMatcher`] that matches all literals with a given datatype
#[derive(Clone, Copy, Debug)]
pub struct DatatypeMatcher<T: Borrow<str>>(IriRef<T>);

impl<T: Borrow<str>> DatatypeMatcher<T> {
    /// Construct a new [`DatatypeMatcher`] from an [`IriRef`]
    pub fn new(iri: IriRef<T>) -> Self {
        Self(iri)
    }

    /// Destructs this [`DatatypeMatcher`]
    pub fn unwrap(self) -> IriRef<T> {
        self.0
    }

    /// Borrow the inner [`IriRef`]
    pub fn as_ref(&self) -> IriRef<&str> {
        self.0.as_ref()
    }
}

impl<T: Borrow<str>> TermMatcher for DatatypeMatcher<T> {
    type Term = SimpleTerm<'static>; // not used

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        match term.datatype() {
            Some(iri) => iri == self.as_ref(),
            None => false,
        }
    }
}

impl<'a> std::ops::Mul<NsTerm<'a>> for Any {
    type Output = DatatypeMatcher<MownStr<'a>>;

    fn mul(self, rhs: NsTerm<'a>) -> Self::Output {
        DatatypeMatcher(rhs.to_iriref())
    }
}

impl<T: Borrow<str>> std::ops::Mul<IriRef<T>> for Any {
    type Output = DatatypeMatcher<T>;

    fn mul(self, rhs: IriRef<T>) -> Self::Output {
        DatatypeMatcher(rhs)
    }
}
