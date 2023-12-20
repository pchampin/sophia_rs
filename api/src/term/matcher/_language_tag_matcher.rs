use std::borrow::Borrow;

use super::*;

/// A [`TermMatcher`] that matches all literals with a given language tag.
///
/// Note that only literals with the *exact* same language tag will be matched.
/// This type does **not** implement language tag matching as specified by RFC4647.
#[derive(Clone, Copy, Debug)]
pub struct LanguageTagMatcher<T: Borrow<str>>(LanguageTag<T>);

impl<T: Borrow<str>> LanguageTagMatcher<T> {
    /// Construct a new [`LanguageTagMatcher`] from a [`LanguageTag`]
    pub fn new(tag: LanguageTag<T>) -> Self {
        Self(tag)
    }

    /// Destructs this [`LanguageTagMatcher`]
    pub fn unwrap(self) -> LanguageTag<T> {
        self.0
    }

    /// Borrow the inner [`LanguageTag`]
    pub fn as_ref(&self) -> LanguageTag<&str> {
        self.0.as_ref()
    }
}

impl<T: Borrow<str>> TermMatcher for LanguageTagMatcher<T> {
    type Term = SimpleTerm<'static>; // not used

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        match term.language_tag() {
            Some(tag) => tag == self.0,
            None => false,
        }
    }
}

impl<T: Borrow<str>> std::ops::Mul<LanguageTag<T>> for Any {
    type Output = LanguageTagMatcher<T>;

    fn mul(self, rhs: LanguageTag<T>) -> Self::Output {
        LanguageTagMatcher(rhs)
    }
}
