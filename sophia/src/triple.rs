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

use ::term::*;

/// This trait represents an abstract RDF triple,
/// and provide convenient methods for working with triples.
pub trait Triple<T: Borrow<str>> {
    /// The subject of this triple.
    fn s(&self) -> &Term<T>;
    /// The predicate of this triple.
    fn p(&self) -> &Term<T>;
    /// The object of this triple.
    fn o(&self) -> &Term<T>;
}

impl<'a, T: Borrow<str>> Triple<T> for (&'a Term<T>, &'a Term<T>, &'a Term<T>) {
    #[inline] fn s(&self) -> &Term<T> { self.0 }
    #[inline] fn p(&self) -> &Term<T> { self.1 }
    #[inline] fn o(&self) -> &Term<T> { self.2 }
}

impl<T: Borrow<str>> Triple<T> for (Term<T>, Term<T>, Term<T>) {
    #[inline] fn s(&self) -> &Term<T> { &self.0 }
    #[inline] fn p(&self) -> &Term<T> { &self.1 }
    #[inline] fn o(&self) -> &Term<T> { &self.2 }
}
