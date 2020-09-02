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

use mownstr::MownStr;
use sophia_api::term::{
    term_cmp, term_eq, term_format, term_hash, term_to_string, CopyTerm, RawValue, SimpleIri,
    TTerm, TermKind, TryCopyTerm,
};
use std::borrow::Borrow;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::Arc;

pub mod factory;
pub mod index_map;

pub mod variable;
use self::variable::Variable;
pub mod blank_node;
use self::blank_node::BlankNode;
pub mod iri;
use self::iri::{Iri, Normalization};
pub mod literal;
use literal::convert::{AsLiteral, DataType, NativeLiteral};
use literal::Literal;

mod _display;
mod _error;
pub use self::_error::*;

/// Generic type for RDF terms.
///
/// See [module documentation](index.html) for more detail.
///
#[derive(Clone, Copy, Debug, Eq, Ord)]
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

    /// Borrow the inner contents of the term.
    pub fn as_ref(&self) -> Term<&T> {
        use self::Term::*;

        match &self {
            Iri(iri) => Iri(iri.as_ref()),
            Literal(lit) => Literal(lit.as_ref()),
            BNode(bn) => BNode(bn.as_ref()),
            Variable(var) => Variable(var.as_ref()),
        }
    }

    /// Borrow the inner contents of the term as `&str`.
    pub fn as_ref_str(&self) -> Term<&str> {
        use self::Term::*;

        match &self {
            Iri(iri) => Iri(iri.as_ref_str()),
            Literal(lit) => Literal(lit.as_ref_str()),
            BNode(bn) => BNode(bn.as_ref_str()),
            Variable(var) => Variable(var.as_ref_str()),
        }
    }

    /// Create a new term by applying `f` to the `TermData` of `self`.
    pub fn map<F, TD2>(self, f: F) -> Term<TD2>
    where
        F: FnMut(T) -> TD2,
        TD2: TermData,
    {
        use self::Term::*;

        match self {
            Iri(iri) => Iri(iri.map(f)),
            Literal(lit) => Literal(lit.map(f)),
            BNode(bn) => BNode(bn.map(f)),
            Variable(var) => Variable(var.map(f)),
        }
    }

    /// Maps the term using the `Into` trait.
    pub fn map_into<TD2>(self) -> Term<TD2>
    where
        T: Into<TD2>,
        TD2: TermData,
    {
        self.map(Into::into)
    }

    /// Clone self while transforming the inner `TermData` with the given
    /// factory.
    ///
    /// This is done in one step in contrast to calling `clone().map(factory)`.
    pub fn clone_map<'a, U, F>(&'a self, factory: F) -> Term<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        use self::Term::*;

        match self {
            Iri(iri) => iri.clone_map(factory).into(),
            BNode(bn) => bn.clone_map(factory).into(),
            Literal(lit) => lit.clone_map(factory).into(),
            Variable(var) => var.clone_map(factory).into(),
        }
    }

    /// Apply `clone_map()` using the `Into` trait.
    pub fn clone_into<'src, U>(&'src self) -> Term<U>
    where
        U: TermData + From<&'src str>,
    {
        self.clone_map(Into::into)
    }

    /// Return a term equivalent to this one,
    /// with all IRIs (if any)
    /// internally represented with all its data in `ns`, and an empty `suffix`.
    ///
    /// # Performances
    /// The returned term will borrow data from this one as much as possible,
    /// but strings may be allocated in case a concatenation is required.
    pub fn normalized(&self, policy: Normalization) -> MownTerm {
        match self {
            Term::Iri(iri) => iri.normalized(policy).into(),
            Term::Literal(lit) => lit.normalized(policy).into(),
            _ => self.as_ref_str().map_into(),
        }
    }

    /// Create a new IRI-term from a given IRI without checking its validity.
    ///
    /// # Pre-conditions
    ///
    /// This function conducts no checks if the resulting IRI is valid. This is
    /// a contract that is generally assumed. Breaking it could result in
    /// unexpected behavior.
    ///
    /// However, in `debug` builds assertions that perform checks are enabled.
    pub fn new_iri_unchecked<U>(iri: U) -> Term<T>
    where
        T: From<U>,
    {
        Iri::<T>::new_unchecked(iri).into()
    }

    /// Create a new IRI-term from a given namespace and suffix.
    ///
    /// # Pre-conditions
    ///
    /// It is expected that
    ///
    /// * the resulting IRI is valid per RFC3987,
    /// * `suffix` is not the empty string
    ///   (otherwise, [`new_iri_unchecked`](#method.new_iri_unchecked) should be used instead).
    ///
    /// This is a contract that is generally assumed.
    /// Breaking it could result in unexpected behavior.
    /// However in `debug` mode, assertions that perform checks are enabled.
    pub fn new_iri_suffixed_unchecked<U, V>(ns: U, suffix: V) -> Term<T>
    where
        T: From<U> + From<V>,
    {
        Iri::<T>::new_suffixed_unchecked(ns, suffix).into()
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
}

impl<T: TermData> TTerm for Term<T> {
    fn kind(&self) -> TermKind {
        use Term::*;
        match self {
            Iri(_) => TermKind::Iri,
            Literal(_) => TermKind::Literal,
            BNode(_) => TermKind::BlankNode,
            Variable(_) => TermKind::Variable,
        }
    }
    fn value_raw(&self) -> RawValue {
        use Term::*;
        match self {
            Iri(i) => i.value_raw(),
            Literal(l) => l.value_raw(),
            BNode(b) => b.value_raw(),
            Variable(v) => v.value_raw(),
        }
    }
    fn datatype(&self) -> Option<SimpleIri> {
        if let Term::Literal(lit) = self {
            lit.datatype()
        } else {
            None
        }
    }
    fn language(&self) -> Option<&str> {
        if let Term::Literal(lit) = self {
            lit.language()
        } else {
            None
        }
    }
    fn as_dyn(&self) -> &dyn TTerm {
        self
    }
}

impl<TD, TE> PartialEq<TE> for Term<TD>
where
    TD: TermData,
    TE: TTerm + ?Sized,
{
    fn eq(&self, other: &TE) -> bool {
        term_eq(self, other)
    }
}
impl<TD, TE> PartialOrd<TE> for Term<TD>
where
    TD: TermData,
    TE: TTerm + ?Sized,
{
    fn partial_cmp(&self, other: &TE) -> Option<std::cmp::Ordering> {
        Some(term_cmp(self, other))
    }
}

impl<TD> Hash for Term<TD>
where
    TD: TermData,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        term_hash(self, state)
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
    TD: TermData + From<Box<str>> + From<&'static str>,
{
    fn from(txt: String) -> Self {
        txt.as_literal().into()
    }
}

impl<'a> From<SimpleIri<'a>> for RefTerm<'a> {
    fn from(other: SimpleIri<'a>) -> Self {
        Iri::from(other).into()
    }
}

impl<T, TD> From<NativeLiteral<T>> for Term<TD>
where
    T: DataType + ?Sized,
    TD: TermData + From<Box<str>> + From<&'static str>,
{
    fn from(other: NativeLiteral<T>) -> Self {
        Literal::from(other).into()
    }
}

impl<'a, T> From<NativeLiteral<T, &'a str>> for RefTerm<'a>
where
    T: DataType + ?Sized,
{
    fn from(other: NativeLiteral<T, &'a str>) -> Self {
        Literal::from(other).into()
    }
}

impl<TD> CopyTerm for Term<TD>
where
    TD: TermData + for<'x> From<&'x str>,
{
    fn copy<T>(term: &T) -> Self
    where
        T: TTerm + ?Sized,
    {
        match term.kind() {
            TermKind::Iri => Term::Iri(Iri::try_copy(term).unwrap()),
            TermKind::Literal => Term::Literal(Literal::try_copy(term).unwrap()),
            TermKind::BlankNode => Term::BNode(BlankNode::try_copy(term).unwrap()),
            TermKind::Variable => Term::Variable(Variable::try_copy(term).unwrap()),
        }
    }
}

impl<'a, T> From<&'a T> for RefTerm<'a>
where
    T: TTerm + ?Sized,
{
    fn from(t: &'a T) -> Self {
        let v = t.value_raw();
        match t.kind() {
            TermKind::Iri => Term::Iri(match v.1 {
                None => Iri::new_unchecked(v.0),
                Some(suffix) => Iri::new_suffixed_unchecked(v.0, suffix),
            }),
            TermKind::Literal => Term::Literal(match t.language() {
                None => {
                    let dt: Iri<&'a str> = t.datatype().unwrap().into();
                    Literal::new_dt(v.0, dt)
                }
                Some(tag) => Literal::new_lang_unchecked(v.0, tag),
            }),
            TermKind::BlankNode => Term::BNode(BlankNode::new_unchecked(v.0)),
            TermKind::Variable => Term::Variable(Variable::new_unchecked(v.0)),
        }
    }
}

impl<'a, TD: TermData + 'a> Borrow<dyn TTerm + 'a> for Term<TD> {
    fn borrow(&self) -> &(dyn TTerm + 'a) {
        self as _
    }
}

#[cfg(test)]
pub(crate) mod test;

/// This line re-exports `same_graph_name` from `sophia_api::term`,
/// to ease transition from older versions of Sophia.
/// It will eventually be deprecated.
///
/// See [`sophia_api`](https://docs.rs/sophia_api/latest/sophia_api/)
pub use sophia_api::term::same_graph_name;

/// This module re-exports things from `sophia_api::ns`,
/// to ease transition from older versions of Sophia.
/// It will eventually be deprecated.
///
/// See [`sophia_api`](https://docs.rs/sophia_api/latest/sophia_api/)
pub mod ns {
    pub use sophia_api::ns::*;
}

/// This line re-exports the module `sophia_api::term::matcher`,
/// to ease transition from older versions of Sophia.
/// It will eventually be deprecated.
///
/// See [`sophia_api`](https://docs.rs/sophia_api/latest/sophia_api/)
pub use sophia_api::term::matcher;
