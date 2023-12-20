use super::*;

#[derive(Clone, Copy, Debug)]
/// A universal matcher: it matches any [`Term`] or [`GraphName`] (even the default graph).
pub struct Any;

impl TermMatcher for Any {
    type Term = SimpleTerm<'static>; // not actually used

    fn matches<T2: Term + ?Sized>(&self, _: &T2) -> bool {
        true
    }
}
