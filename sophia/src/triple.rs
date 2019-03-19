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

use std::hash::Hash;

use crate::term::*;

pub mod stream;

/// This trait represents an abstract RDF triple,
/// and provide convenient methods for working with triples.
pub trait Triple<'a> {
    type TermData: AsRef<str> + Clone + Eq + Hash + 'a;
    /// The subject of this triple.
    fn s(&self) -> &Term<Self::TermData>;
    /// The predicate of this triple.
    fn p(&self) -> &Term<Self::TermData>;
    /// The object of this triple.
    fn o(&self) -> &Term<Self::TermData>;
}

impl<'a, T> Triple<'a> for [Term<T>;3]
where
    T: AsRef<str> + Clone + Eq + Hash + 'a,
{
    type TermData= T;
    #[inline] fn s(&self) -> &Term<T> { &self[0] }
    #[inline] fn p(&self) -> &Term<T> { &self[1] }
    #[inline] fn o(&self) -> &Term<T> { &self[2] }
}

impl<'a, T> Triple<'a> for [&'a Term<T>;3]
where
    T: AsRef<str> + Clone + Eq + Hash + 'a,
{
    type TermData= T;
    #[inline] fn s(&self) -> &Term<T> { self[0] }
    #[inline] fn p(&self) -> &Term<T> { self[1] }
    #[inline] fn o(&self) -> &Term<T> { self[2] }
}

impl<'a, T: Triple<'a>> Triple<'a> for &'a T {
    type TermData = T::TermData;
    #[inline] fn s(&self) -> &Term<T::TermData> { (*self).s() }
    #[inline] fn p(&self) -> &Term<T::TermData> { (*self).p() }
    #[inline] fn o(&self) -> &Term<T::TermData> { (*self).o() }
}



#[cfg(test)]
mod test {
    // Nothing really worth testing here
}