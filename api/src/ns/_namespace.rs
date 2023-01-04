use super::*;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// A custom namespace.
///
/// The [`get`](Namespace::get) method can be used to create a new IRI by concatenating a suffix to this namespace's IRI.
pub struct Namespace<T: Borrow<str>>(IriRef<T>);

impl<T: Borrow<str>> Namespace<T> {
    /// Build a custom namespace based on the given IRI.
    ///
    /// `iri` must be a valid IRI, otherwise this constructor returns an error.
    pub fn new(iri: T) -> Result<Self, InvalidIri> {
        IriRef::new(iri).map(Namespace)
    }

    /// Build a custom namespace based on the given IRI,
    /// without checking that it is valid.
    /// If it is not, it may result in undefined behaviour.
    pub fn new_unchecked(iri: T) -> Self {
        Namespace(IriRef::new_unchecked(iri))
    }

    /// Build an IRI by appending `suffix` to this namespace.
    ///
    /// Return an error if the concatenation produces an invalid IRI.
    pub fn get<'s>(&'s self, suffix: &'s str) -> Result<NsTerm<'s>, InvalidIri> {
        let ns_term = NsTerm {
            ns: self.0.as_ref(),
            suffix,
        };
        IriRef::new(ns_term.to_string())?;
        Ok(ns_term)
    }

    /// Build an IRI by appending `suffix` to this namespace,
    /// without checking that the resulting IRI is valid.
    /// If it is not, it may result in undefined behaviour.
    pub fn get_unchecked<'s>(&'s self, suffix: &'s str) -> NsTerm<'s> {
        NsTerm {
            ns: self.0.as_ref(),
            suffix,
        }
    }

    /// Consume this Namespace and return the inner [`IriRef`].
    pub fn inner(self) -> IriRef<T> {
        self.0
    }
}

impl Namespace<&'static str> {
    /// `const` constructor for [`Namespace`]
    ///
    ///  # Precondition
    ///  `iri` must be a valid IRI reference (possibly relative),
    ///  otherwise undefined behaviour may occur.
    pub const fn new_unchecked_const(iri: &'static str) -> Self {
        Namespace(IriRef::new_unchecked_const(iri))
    }
}

impl<T: Borrow<str>> From<IriRef<T>> for Namespace<T> {
    fn from(other: IriRef<T>) -> Self {
        Namespace(other)
    }
}

impl<T: Borrow<str>> std::ops::Deref for Namespace<T> {
    type Target = IriRef<T>;

    fn deref(&self) -> &IriRef<T> {
        &self.0
    }
}
