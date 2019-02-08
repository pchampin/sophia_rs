//! An RDF triple expresses a single fact.
//! Its formed of three terms called *subject*, *predicate* and *object*.
//! 
//! You can think of a triple as a sentence of the form
//! "subject verb complement"
//! (although the *predicate* is often better expressed as a relationship than a verb).
//! Examples :
//! 
//! * John is a person.
//! * John was born in Paris.
//! * John knows Jane.
//! * John's family name is "Doe".
//! 

use std::borrow::Borrow;
use std::hash::Hash;

use crate::term::*;

/// This trait represents an abstract RDF triple,
/// and provide convenient methods for working with triples.
pub trait Triple<'a> {
    type Holder: Borrow<str> + Clone + Eq + Hash + 'a;
    /// The subject of this triple.
    fn s(&self) -> &Term<Self::Holder>;
    /// The predicate of this triple.
    fn p(&self) -> &Term<Self::Holder>;
    /// The object of this triple.
    fn o(&self) -> &Term<Self::Holder>;
}

impl<'a, T> Triple<'a> for [Term<T>;3]
where
    T: Borrow<str> + Clone + Eq + Hash + 'a,
{
    type Holder= T;
    #[inline] fn s(&self) -> &Term<T> { &self[0] }
    #[inline] fn p(&self) -> &Term<T> { &self[1] }
    #[inline] fn o(&self) -> &Term<T> { &self[2] }
}

impl<'a, T> Triple<'a> for [&'a Term<T>;3]
where
    T: Borrow<str> + Clone + Eq + Hash + 'a,
{
    type Holder= T;
    #[inline] fn s(&self) -> &Term<T> { self[0] }
    #[inline] fn p(&self) -> &Term<T> { self[1] }
    #[inline] fn o(&self) -> &Term<T> { self[2] }
}

impl<'a, T: Triple<'a>> Triple<'a> for &'a T {
    type Holder = T::Holder;
    #[inline] fn s(&self) -> &Term<T::Holder> { (*self).s() }
    #[inline] fn p(&self) -> &Term<T::Holder> { (*self).p() }
    #[inline] fn o(&self) -> &Term<T::Holder> { (*self).o() }
}



#[cfg(test)]
mod test {
    // Nothing really worth testing here
}