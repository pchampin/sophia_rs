//! Types that can be used generally to hold the data of RDF terms, e.g. the
//! lexical value of a literal or an IRI.

mod _impl;

use std::hash::Hash;

/// Trait alias for types holding the textual data of terms.
///
/// The `TermData` considered in this crate are: `&str`, `String`, `Box<str>`,
/// `Rc<str>`, `Arc<str>`, Cow<\'_, str> and `MownStr<\'_>` and the respective
/// references like `&Box<str>` and so on.
pub trait TermData: AsRef<str> + Clone + Eq + Hash {}
impl<T> TermData for T where T: AsRef<str> + Clone + Eq + Hash {}

/// Convert efficiently between different `TermData`.
///
/// Due to the variety of `TermData` not all conversions can easily be
/// expressed in trait bounds. Therefore, this trait.
///
/// This trait is implemented for `&str`, `String`, `Box<str>`, `Rc<str>`,
/// `Arc<str>`, Cow<'_, str> and `MownStr<'_>` and the respective references
/// like `&Box<str>` and so on. This allows users to pass either ownership of
/// values or references.
///
/// If one wants to use a custom `TermData` the `FromData` implementation for
/// the named above should be provided.
pub trait FromData<T> {
    /// Build data from another type.
    fn from_data(t: T) -> Self;
}

impl<T> FromData<T> for T {
    fn from_data(t: T) -> Self {
        t
    }
}

/// Counterpart to `FromData` just like `Into` to `From`.
///
/// For further information see [`FromData`].
///
/// [`FromData`]: ./trait.FromData.html
pub trait IntoData<T> {
    /// Turns `self` into other data.
    fn into_data(self) -> T;
}

impl<T, U> IntoData<U> for T
where
    U: FromData<T>,
{
    fn into_data(self) -> U {
        U::from_data(self)
    }
}
