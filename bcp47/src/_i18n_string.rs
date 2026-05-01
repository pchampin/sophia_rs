//! I define the [`I18nString`] type.

use std::borrow::Borrow;

use crate::LanguageTag;

/// An `I18nString` is a text associated with a language.
///
/// NB: "i18n" is short for "internationalization"
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct I18nString<T: Borrow<str>> {
    /// The text value of this I18nString
    pub value: T,
    /// The language of this I18nString
    pub language: LanguageTag<T>,
}

impl<T: Borrow<str>> I18nString<T> {
    /// Constructor
    pub fn new(value: T, language: LanguageTag<T>) -> Self {
        Self { value, language }
    }

    /// Convert reference to an `I18nString<&str>`
    pub fn as_ref(&self) -> I18nString<&str> {
        I18nString {
            value: self.value.borrow(),
            language: self.language.as_ref(),
        }
    }

    /// Map a [`I18nString`]`<T>` to a [`I18nString`]`<U>`
    /// by applying a function to the wrapped value.
    ///
    /// It does not check that the value returned by the function is valid for
    /// [`language`](I18nString::language)
    /// If it is not, it may result in undefined behaviour.
    pub fn map_unchecked<F, U>(self, mut f: F) -> I18nString<U>
    where
        F: FnMut(T) -> U,
        U: Borrow<str>,
    {
        I18nString {
            value: f(self.value),
            language: self.language.map_unchecked(f),
        }
    }
}

impl<T: Borrow<str>> std::fmt::Display for I18nString<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} [{}]", self.value.borrow(), &self.language)
    }
}
