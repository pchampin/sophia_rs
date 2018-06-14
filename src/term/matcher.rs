//! A term-matcher is something that can be used
//! to discriminate members of a set of terms.
//!
// TODO point to Graph::iter_match, MutableGraph::remove_match
// once they are defined

use super::*;

// TODO document the fact that
// - T is the kind of term on which this matcher will be used
// - Holder is the kind of term this matcher will produce (with the `constant` method)
pub trait TermMatcher<T: Borrow<str>> {
    type Holder: Borrow<str>;
    fn constant(&self) -> Option<&Term<Self::Holder>>;
    fn try(&self, t: &Term<T>) -> bool;
}

impl<T, U> TermMatcher<T> for Term<U> where
    T: Borrow<str>,
    U: Borrow<str>,
{
    type Holder = U;
    fn constant(&self) -> Option<&Term<Self::Holder>> {
        Some(self)
    }
    fn try(&self, t: &Term<T>) -> bool {
        t==self
    }
}

impl<T, U> TermMatcher<T> for Option<Term<U>> where
    T: Borrow<str>,
    U: Borrow<str>,
{
    type Holder = U;
    fn constant(&self) -> Option<&Term<Self::Holder>> {
        self.as_ref()
    }
    fn try(&self, t: &Term<T>) -> bool {
        match self {
            Some(term) => t == term,
            None => true,
        }
    }
}

impl<T, U> TermMatcher<T> for [Term<U>] where
    T: Borrow<str>,
    U: Borrow<str>,
{
    type Holder = U;
    fn constant(&self) -> Option<&Term<Self::Holder>> {
        if self.len() == 1 { Some(&self[0]) }
        else { None }
    }
    fn try(&self, t: &Term<T>) -> bool {
        for term in self {
            if t == term { return true; }
        }
        false
    }
}

impl<T, F> TermMatcher<T> for F where
    T: Borrow<str>,
    F: Fn(&Term<T>) -> bool,
{
    type Holder = &'static str;
    fn constant(&self) -> Option<&Term<Self::Holder>> {
        None
    }
    fn try(&self, t: &Term<T>) -> bool {
        self(t)
    }
}
