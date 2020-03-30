//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.

//!
//! Terms are the building blocks of an [RDF] graph.
//! There are four types of terms: IRIs, blank nodes (BNode for short),
//! literals and variables.
//!
//! NB: variable only exist in [generalized RDF].
//!
//! This module defines a generic type [`Term`](enum.Term.html)
//! which can be derived differently depending on your needs.
//!
//! * [`RefTerm<'a>`](type.RefTerm.html) (alias of `Term<&'a str>`)
//!   should be used for very short-lived terms,
//!   *i.e.* terms that live less than `'a`,
//!   which is the lifetime of their underlying text.
//!
//! * [`BoxTerm`](type.BoxTerm.html) (alias of `Term<Box<str>>`)
//!    should be used when the term may outlive the text used to create it.
//!
//! * [`RcTerm`](type.RcTerm.html) (alias of `Term<Rc<str>>`)
//!    should also be used for long-lived terms,
//!    especially if they need to be cloned multiple times.
//!    The use of `Rc` prevents the duplication of the underlying text,
//!    while ensuring that it is cleaned when appropriate.
//!
//! * [`ArcTerm`](type.ArcTerm.html) (alias of `Term<Arc<str>>`)
//!    should be used when, additionally,
//!    terms need to be sent to other threads.
//!
//! * [`StaticTerm`](type.StaticTerm.html) (alias of `Term<&'static str>)
//!   is a special case of `RefTerm`
//!   where the underlying text is a static string.
//!   Those terms can live as long as the program runs,
//!   and be cloned and sent without any restriction.
//!
//! * [`MownTerm`](type.MownTerm.html) (alias of `Term<MownStr<'a>>)
//!   should be used in situations where some terms can borrow their data,
//!   while others need to own it.
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/
//! [generalized RDF]: https://docs.rs/sophia/latest/sophia/#generalized-vs-strict-rdf-model

#![deny(missing_docs)]

use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

pub mod factory;
pub mod index_map;
pub mod matcher;
pub mod ns;

pub mod variable;
use self::variable::Variable;
pub mod blank_node;
use self::blank_node::BlankNode;
pub mod iri;
use self::iri::{Iri, Normalization};
pub mod literal;
use self::literal::{AsLiteral, Literal};
pub mod mown_str;
use self::mown_str::MownStr;

mod _display;
mod _error;
mod _graph_name_matcher; // is 'pub use'd by module 'matcher'
pub use self::_error::*;

/// Generic type for RDF terms.
///
/// See [module documentation](index.html) for more detail.
///
#[derive(Clone, Copy, Debug, Eq, Hash)]
pub enum Term<TD>
where
    TD: TermData,
{
    /// An IRI referencing a resource.
    Iri(Iri<TD>),
    /// A blank node.
    ///
    /// Also known as existentially quantified variable.
    BNode(BlankNode<TD>),
    /// An RDF literal.
    Literal(Literal<TD>),
    /// A universally quantified variable like in SPARQL or Notation3.
    Variable(Variable<TD>),
}

/// Trait alias for types holding the textual data of terms.
pub trait TermData: AsRef<str> + Clone + Eq + Hash {}
impl<T> TermData for T where T: AsRef<str> + Clone + Eq + Hash {}

/// Convenient alias for a specialization of `Term<T>`.
///
/// See [module documentation](index.html)
/// for more detail on when to use it.
pub type BoxTerm = Term<Box<str>>;
/// Convenient alias for a specialization of `Term<T>`.
///
/// See [module documentation](index.html)
/// for more detail on when to use it.
pub type RcTerm = Term<Rc<str>>;
/// Convenient alias for a specialization of `Term<T>`.
///
/// See [module documentation](index.html)
/// for more detail on when to use it.
pub type ArcTerm = Term<Arc<str>>;
/// Convenient alias for a specialization of `Term<T>`.
///
/// See [module documentation](index.html)
/// for more detail on when to use it.
pub type RefTerm<'a> = Term<&'a str>;
/// Convenient alias for a specialization of `Term<T>`.
///
/// See [module documentation](index.html)
/// for more detail on when to use it.
pub type StaticTerm = RefTerm<'static>;
/// Convenient alias for a specialization of `Term<T>`.
///
/// See [module documentation](index.html)
/// for more detail on when to use it.
pub type MownTerm<'a> = Term<MownStr<'a>>;

impl<T> Term<T>
where
    T: TermData,
{
    /// Return a new IRI term from the given text.
    ///
    /// May fail if `txt` is not a valid IRI.
    pub fn new_iri<U>(iri: U) -> Result<Term<T>>
    where
        U: AsRef<str>,
        T: From<U>,
    {
        Iri::<T>::new(iri).map(Into::into)
    }

    /// Return a new IRI term from the two given parts (prefix and suffix).
    ///
    /// May fail if the concatenation of `ns` and `suffix`
    /// does not produce a valid IRI.
    pub fn new_iri_suffixed<U, V>(ns: U, suffix: V) -> Result<Term<T>>
    where
        U: AsRef<str>,
        V: AsRef<str>,
        T: From<U> + From<V>,
    {
        Iri::<T>::new_suffixed(ns, suffix).map(Into::into)
    }

    /// Return a new blank node term with the given bnode ID.
    ///
    /// Currently, this may never fail;
    /// however it returns a result for homogeneity with other constructor methods,
    /// and because future versions may be more picky regarding bnode IDs.
    pub fn new_bnode<U>(id: U) -> Result<Term<T>>
    where
        U: AsRef<str>,
        T: From<U>,
    {
        BlankNode::new(id).map(Into::into)
    }

    /// Return a new literal term with the given value and language tag.
    ///
    /// May fail if the language tag is not a valid BCP47 language tag.
    pub fn new_literal_lang<U, V>(txt: U, lang: V) -> Result<Self>
    where
        V: AsRef<str>,
        T: From<U> + From<V>,
    {
        Literal::<T>::new_lang(txt, lang).map(Into::into)
    }

    /// Return a new literal term with the given value and datatype.
    ///
    /// May fail if `dt` is not an IRI.
    pub fn new_literal_dt<U, V>(txt: U, dt: V) -> Result<Self>
    where
        T: From<U>,
        V: TryInto<Iri<T>>,
        TermError: From<<V as TryInto<Iri<T>>>::Error>,
    {
        Ok(Literal::new_dt(txt, dt.try_into()?).into())
    }

    /// Return a new variable term with the given name.
    ///
    /// May fail if `name` is not a valid variable name.
    pub fn new_variable<U>(name: U) -> Result<Term<T>>
    where
        U: AsRef<str>,
        T: From<U>,
    {
        Variable::new(name).map(Into::into)
    }

    /// Clone another term with the given factory.
    ///
    /// Clone as this might allocate a new `TermData`. However there is also
    /// `TermData` that is cheap to clone, i.e. `Copy`.
    pub fn clone_with<'a, U, F>(&'a self, factory: F) -> Term<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        use self::Term::*;

        match self {
            Iri(iri) => iri.clone_with(factory).into(),
            BNode(bn) => bn.clone_with(factory).into(),
            Literal(lit) => lit.clone_with(factory).into(),
            Variable(var) => var.clone_with(factory).into(),
        }
    }

    /// Transforms the underlying IRIs according to the given policy.
    ///
    /// If the policy already applies the Term is returned unchanged.
    pub fn clone_normalized_with<F, U>(&self, policy: Normalization, factory: F) -> Term<U>
    where
        F: FnMut(&str) -> U,
        U: TermData,
    {
        match self {
            Term::Iri(iri) => iri.clone_normalized_with(policy, factory).into(),
            Term::Literal(lit) => lit.clone_normalized_with(policy, factory).into(),
            _ => self.clone_with(factory),
        }
    }

    /// Create a new IRI-term from a given IRI without checking its validity.
    ///
    /// As it is not checked if absolute or relative this property must be
    /// entered as well.
    ///
    /// # Pre-conditions
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    ///
    /// However, in `debug` builds assertions that perform checks are enabled.
    pub fn new_iri_unchecked<U>(iri: U, absolute: bool) -> Term<T>
    where
        T: From<U>,
    {
        Iri::<T>::new_unchecked(iri, absolute).into()
    }

    /// Create a new IRI-term from a given namespace and suffix.
    ///
    /// As it is not checked if absolute or relative this property must be
    /// entered as well.
    ///
    /// # Pre-conditions
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    ///
    /// However, in `debug` builds assertions that perform checks are enabled.
    pub fn new_iri_suffixed_unchecked<U, V>(ns: U, suffix: V, absolute: bool) -> Term<T>
    where
        T: From<U> + From<V>,
    {
        Iri::<T>::new_suffixed_unchecked(ns, suffix, absolute).into()
    }

    /// Return a new blank node term.
    ///
    /// # Pre-condition
    ///
    /// This function requires that `id` is a valid bnode ID.
    pub fn new_bnode_unchecked<U>(id: U) -> Term<T>
    where
        U: AsRef<str>,
        T: From<U>,
    {
        BlankNode::<T>::new_unchecked(id).into()
    }

    /// Return a literal term.
    ///
    /// # Pre-condition
    ///
    /// This function requires that `lang` is a valid language tag.
    /// In debug mode this constraint is asserted.
    pub fn new_literal_lang_unchecked<U, V>(txt: U, lang: V) -> Self
    where
        V: AsRef<str>,
        T: From<U> + From<V>,
    {
        Literal::<T>::new_lang_unchecked(txt, lang).into()
    }

    /// Return a typed literal term.
    ///
    /// # Panics
    ///
    /// Panics if `dt` cannot be converted into an IRI.
    pub fn new_literal_dt_unchecked<U, V>(txt: U, dt: V) -> Self
    where
        T: From<U>,
        V: TryInto<Iri<T>>,
        <V as TryInto<Iri<T>>>::Error: Debug,
    {
        Literal::new_dt(txt, dt.try_into().unwrap()).into()
    }

    /// Return a new variable term.
    ///
    /// # Pre-condition
    ///
    /// This function requires that `name` is a valid variable name.
    pub fn new_variable_unchecked<U>(name: U) -> Term<T>
    where
        U: AsRef<str>,
        T: From<U>,
    {
        Variable::<T>::new_unchecked(name).into()
    }

    /// Return this term's value as text.
    ///
    /// NB: for literals, the value only conveys the literal value,
    /// *not* the datatype or the language tag.error
    pub fn value(&self) -> MownStr {
        use self::Term::*;

        match self {
            Iri(iri) => iri.value(),
            BNode(bn) => bn.value(),
            Literal(lit) => lit.value(),
            Variable(var) => var.value(),
        }
    }

    /// Return whether this term is absolute.
    ///
    /// * An IRI is absolute iff it is an absolute IRI.
    /// * A typed literal is absolute iff its datatype is absolute.
    /// * Any other term is always absolute.
    pub fn is_absolute(&self) -> bool {
        match self {
            Term::Iri(iri) => iri.is_absolute(),
            Term::Literal(lit) => lit.is_absolute(),
            _ => true,
        }
    }
}

impl<T, U> PartialEq<Term<U>> for Term<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Term<U>) -> bool {
        use self::Term::*;

        match (self, other) {
            (Iri(iri1), Iri(iri2)) => iri1 == iri2,
            (BNode(id1), BNode(id2)) => id1 == id2,
            (Literal(l1), Literal(l2)) => l1 == l2,
            (Variable(var1), Variable(var2)) => var1 == var2,
            _ => false,
        }
    }
}

impl<T, U> PartialEq<Iri<U>> for Term<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Iri<U>) -> bool {
        match self {
            Term::Iri(iri) => iri == other,
            _ => false,
        }
    }
}

impl<T, U> PartialEq<Literal<U>> for Term<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Literal<U>) -> bool {
        match self {
            Term::Literal(lit) => lit == other,
            _ => false,
        }
    }
}

impl<T, U> PartialEq<BlankNode<U>> for Term<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &BlankNode<U>) -> bool {
        match self {
            Term::BNode(bn) => bn == other,
            _ => false,
        }
    }
}

impl<T, U> PartialEq<Variable<U>> for Term<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Variable<U>) -> bool {
        match self {
            Term::Variable(var) => var == other,
            _ => false,
        }
    }
}

impl<'a, T, U> From<&'a Term<U>> for Term<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    fn from(other: &'a Term<U>) -> Term<T> {
        other.clone_with(T::from)
    }
}

impl<TD> From<Iri<TD>> for Term<TD>
where
    TD: TermData,
{
    fn from(iri: Iri<TD>) -> Self {
        Term::Iri(iri)
    }
}

impl<TD> From<Literal<TD>> for Term<TD>
where
    TD: TermData,
{
    fn from(lit: Literal<TD>) -> Self {
        Term::Literal(lit)
    }
}

impl<TD> From<Variable<TD>> for Term<TD>
where
    TD: TermData,
{
    fn from(var: Variable<TD>) -> Self {
        Term::Variable(var)
    }
}

impl<TD> From<BlankNode<TD>> for Term<TD>
where
    TD: TermData,
{
    fn from(bn: BlankNode<TD>) -> Self {
        Term::BNode(bn)
    }
}

impl<TD> From<String> for Term<TD>
where
    TD: TermData + From<String> + From<&'static str>,
{
    fn from(txt: String) -> Self {
        txt.as_term()
    }
}

impl<'a> From<&'a str> for RefTerm<'a> {
    fn from(txt: &'a str) -> Self {
        txt.as_term()
    }
}

/// Check the equality of two graph names (`Option<&Term>`)
/// using possibly different `TermData`.
pub fn same_graph_name<T, U>(g1: Option<&Term<T>>, g2: Option<&Term<U>>) -> bool
where
    T: TermData,
    U: TermData,
{
    match (g1, g2) {
        (Some(n1), Some(n2)) => n1 == n2,
        (None, None) => true,
        _ => false,
    }
}

#[cfg(test)]
pub(crate) mod test;
