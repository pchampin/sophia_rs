//! A term-matcher is something that can be used
//! to discriminate members of a set of terms.
//! 
//! See [`Graph::iter_matching`](../../graph/trait.Graph.html#method.iter_matching),
//! [`MutableGraph::remove_matching`](../../graph/trait.MutableGraph.html#method.remove_matching),
//! [`MutableGraph::retain`](../../graph/trait.MutableGraph.html#method.retain).

use super::*;

/// Anything that matches a [term] or a set of [term]s.
/// 
/// [term]: ../enum.Term.html
/// 
pub trait TermMatcher<T: Borrow<str>> {
    type Holder: Borrow<str>;
    /// If this matcher matches only one term, return this term, else `None`.
    fn constant(&self) -> Option<&Term<Self::Holder>>;

    /// Check whether this matcher matches `t`.
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

/// A matcher matching any term.
///
/// It is actually the `None` variant from `Option<StaticTerm>`,
/// but the name "None" may be confusing
/// when one wants to actually match *any* term.
pub const ANY: Option<StaticTerm> = None;
