//! Terms are the building blocks of an RDF graph.
//! There are four types of terms: IRIs, blank nodes (BNode for short),
//! literals and variables.
//!
//! NB: variable only exist in [generalized RDF](../index.html#generalized-vs-strict-rdf-model).
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

use std::fmt::Debug;
use std::hash::Hash;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use language_tag::LangTag;

pub mod factory;
pub mod index_map;
pub mod iri_rfc3987;
use self::iri_rfc3987::IriRefStructure;
pub mod matcher;

pub mod variable;
use self::variable::Variable;
pub mod blank_node;
pub use self::blank_node::BlankNode;

mod _convert;
pub use self::_convert::*;
mod _display;
mod _iri_data;
pub use self::_iri_data::*;
mod _graph_name_matcher; // is 'pub use'd by module 'matcher'
mod _literal_kind;
pub use self::_literal_kind::*;
mod _error;
pub use self::_error::*;

/// Generic type for RDF terms.
///
/// See [module documentation](index.html) for more detail.
///
#[derive(Clone, Copy, Debug, Eq, Hash)]
pub enum Term<T>
where
    T: TermData,
{
    Iri(IriData<T>),
    BNode(BlankNode<T>),
    Literal(T, LiteralKind<T>),
    Variable(Variable<T>),
}
pub use self::Term::*;

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

impl<T> Term<T>
where
    T: TermData,
{
    /// Return a new IRI term from the given text.
    ///
    /// May fail if `txt` is not a valid IRI.
    ///
    pub fn new_iri<U>(iri: U) -> Result<Term<T>>
    where
        T: From<U>,
    {
        Ok(Iri(IriData::new(T::from(iri), None)?))
    }

    /// Return a new IRI term from the two given parts (prefix and suffix).
    ///
    /// May fail if the concatenation of `ns` and `suffix`
    /// does not produce a valid IRI.
    ///
    pub fn new_iri2<U, V>(ns: U, suffix: V) -> Result<Term<T>>
    where
        T: From<U> + From<V>,
    {
        Ok(Iri(IriData::new(T::from(ns), Some(T::from(suffix)))?))
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
    pub fn new_literal_lang<U, V>(txt: U, lang: V) -> Result<Term<T>>
    where
        T: From<U> + From<V>,
    {
        let tag = T::from(lang);
        match LangTag::from_str(tag.as_ref()) {
            Err(err) => Err(TermError::InvalidLanguageTag {
                tag: tag.as_ref().to_string(),
                err,
            }),
            Ok(_) => Ok(Literal(T::from(txt), Lang(tag))),
        }
    }

    /// Return a new literal term with the given value and datatype.
    ///
    /// May fail if `dt` is not a valid datatype.
    pub fn new_literal_dt<U>(txt: U, dt: Term<T>) -> Result<Term<T>>
    where
        T: From<U>,
    {
        match dt {
            Iri(iri) => Ok(Literal(T::from(txt), Datatype(iri))),
            _ => Err(TermError::InvalidDatatype(format!("{}", dt))),
        }
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

    /// Copy another term with the given factory.
    pub fn from_with<'a, U, F>(other: &'a Term<U>, mut factory: F) -> Term<T>
    where
        U: TermData,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Iri(iri) => Iri(IriData::from_with(&iri, factory)),
            BNode(bn) => bn.copy_with(factory).into(),
            Literal(value, kind) => Literal(
                factory(value.as_ref()),
                LiteralKind::from_with(kind, factory),
            ),
            Variable(var) => var.copy_with(factory).into(),
        }
    }

    /// Copy another term with the given factory,
    /// applying the given normalization policy.
    pub fn normalized_with<U, F>(other: &'_ Term<U>, mut factory: F, norm: Normalization) -> Term<T>
    where
        U: TermData,
        F: FnMut(&str) -> T,
    {
        match other {
            Iri(iri) => Iri(IriData::normalized_with(&iri, factory, norm)),
            Literal(value, kind) => Literal(
                factory(value.as_ref()),
                LiteralKind::normalized_with(kind, factory, norm),
            ),
            _ => Self::from_with(other, factory),
        }
    }

    /// Return a new IRI term.
    ///
    /// # Safety
    /// This function requires that `iri` is a valid IRI reference,
    /// and that `abs` correctly indicates whether it is absolute or relative.
    pub unsafe fn new_iri_unchecked<U>(iri: U, abs: Option<bool>) -> Term<T>
    where
        T: From<U>,
    {
        Iri(IriData::new_unchecked(T::from(iri), None, abs))
    }

    /// Return a new IRI term,
    ///
    /// # Safety
    /// This function requires that that
    /// `ns` and `suffix` concatenate to a valid IRI reference,
    /// and that `abs` correctly indicates whether it is absolute or relative.
    pub unsafe fn new_iri2_unchecked<U, V>(ns: U, suffix: V, abs: Option<bool>) -> Term<T>
    where
        T: From<U> + From<V>,
    {
        Iri(IriData::new_unchecked(
            T::from(ns),
            Some(T::from(suffix)),
            abs,
        ))
    }

    /// Return a new blank node term.
    ///
    /// # Safety
    /// This function requires that `id` is a valid bnode ID.
    pub unsafe fn new_bnode_unchecked<U>(id: U) -> Term<T>
    where
        T: From<U>,
    {
        BlankNode::<T>::new_unchecked(id).into()
    }

    /// Return a literal term.
    ///
    /// # Safety
    /// This function that `lang` is a valid language tag.
    pub unsafe fn new_literal_lang_unchecked<U, V>(txt: U, lang: V) -> Term<T>
    where
        T: From<U> + From<V>,
    {
        Literal(T::from(txt), Lang(T::from(lang)))
    }

    /// Return a typed literal term.
    ///
    /// # Panics
    /// Panics if `dt` is not an IRI.
    pub fn new_literal_dt_unchecked<U>(txt: U, dt: Term<T>) -> Term<T>
    where
        T: From<U> + Debug,
    {
        if let Iri(dt) = dt {
            Literal(T::from(txt), Datatype(dt))
        } else {
            panic!(format!(
                "new_literal_dt_unchecked expects Term::Iri as dt, got {:?}",
                dt
            ))
        }
    }

    /// Return a new variable term.
    ///
    /// # Safety
    /// This function requires that `name` is a valid variable name.
    pub unsafe fn new_variable_unchecked<U>(name: U) -> Term<T>
    where
        T: From<U>,
    {
        Variable::<T>::new_unchecked(name).into()
    }

    /// Return a copy of this term's underlying text.
    ///
    /// NB: for literals, the value only conveys the literal value,
    /// *not* the datatype or the language tag.error
    ///
    /// See also [`n3`](#method.n3).
    pub fn value(&self) -> String {
        match self {
            Iri(iri) => iri.to_string(),
            BNode(bn) => bn.value(),
            Literal(value, _) => String::from(value.as_ref()),
            Variable(var) => var.value(),
        }
    }

    /// If `t` is or contains a relative IRI, replace it with an absolute one,
    /// using this term as the base.
    /// Otherwise, returns `t` unchanged.
    ///
    /// This affects IRI terms, but also Literal terms with a datatype.
    ///
    /// # Example
    /// ```
    /// use sophia::term::*;
    ///
    /// let i1 = BoxTerm::new_iri("http://example.org/foo/bar").unwrap();
    /// let i2 = BoxTerm::new_iri("../baz").unwrap();
    /// let i3 = i1.join(&i2);
    /// assert_eq!(&i3.value(), "http://example.org/baz");
    /// ```
    ///
    /// # Panics
    /// Panics if this Term is not an IRI or is not absolute (see [`is_absolute`](#method.is_absolute)).
    ///
    /// # Performance
    /// If you need to join multiple terms to the same base,
    /// you should use [`batch_join`](#method.batch_join) instead,
    /// as it factorizes the pre-processing required for joining IRIs.
    ///
    pub fn join<U>(&self, t: &Term<U>) -> Term<U>
    where
        U: TermData + From<String>,
    {
        let mut ret = None;
        self.batch_join(|join| {
            //let t = warp.take().unwrap();
            ret = Some(join(t));
        });
        ret.unwrap()
    }

    /// Takes a closure with a `join` parameter,
    /// where `join` is a function comparable to the [`join`](#method.join) method.
    /// Useful for joining multiple terms with this IRI.
    ///
    /// # Example
    /// ```
    /// use sophia::term::*;
    ///
    /// let i1 = BoxTerm::new_iri("http://example.org/foo/bar").unwrap();
    /// let mut terms = vec![
    ///     BoxTerm::new_iri("../baz").unwrap(),
    ///     BoxTerm::new_iri("#baz").unwrap(),
    ///     BoxTerm::new_iri("http://another.example.org").unwrap(),
    /// ];
    /// i1.batch_join(|join| {
    ///     for t in &mut terms {
    ///         *t = join(t);
    ///     }
    /// });
    /// ```
    ///
    /// # Panics
    /// Panics if this Term is not an IRI or is not absolute (see [`is_absolute`](#method.is_absolute)).
    ///
    pub fn batch_join<F, U>(&self, task: F)
    where
        F: FnOnce(&dyn Fn(&Term<U>) -> Term<U>) -> (),
        U: TermData + From<String>,
    {
        match self {
            Iri(iri) if iri.is_absolute() => {
                let iri_txt = iri.to_string();
                let base = IriRefStructure::new(&iri_txt).unwrap();
                task(&|t| match t {
                    Iri(ref iri) => Iri(base.join_iri(iri)),
                    Literal(ref txt, Datatype(ref iri)) => {
                        Literal(txt.clone(), Datatype(base.join_iri(iri)))
                    }
                    _ => t.clone(),
                });
            }
            _ => panic!("Can only join with absolute Iri"),
        }
    }

    /// Return whether this term is absolue.
    ///
    /// * An IRI is absolute iff it is an absolute IRI.
    /// * A typed literal is absolute iff its datatype is absolute.
    /// * Any other term is always absolute.
    pub fn is_absolute(&self) -> bool {
        match self {
            Iri(iri) | Literal(_, Datatype(iri)) => iri.is_absolute(),
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
        match (self, other) {
            (Iri(iri1), Iri(iri2)) => iri1 == iri2,
            (BNode(id1), BNode(id2)) => id1 == id2,
            (Literal(value1, kind1), Literal(value2, kind2)) => {
                value1.as_ref() == value2.as_ref() && kind1 == kind2
            }
            (Variable(var1), Variable(var2)) => var1 == var2,
            _ => false,
        }
    }
}

impl<T, U> PartialEq<IriData<U>> for Term<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &IriData<U>) -> bool {
        match self {
            Iri(iri1) => iri1 == other,
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
        Self::from_with(other, T::from)
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
