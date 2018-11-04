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

use std::borrow::Borrow;
use std::fmt::Debug;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use language_tag::LangTag;
use regex::Regex;

use ::error::*;

pub mod factory;
pub mod iri_rfc3987; use self::iri_rfc3987::ParsedIri;
pub mod matcher;

mod bnode_id;  pub use self::bnode_id::*;
mod convert;  pub use self::convert::*;
mod iri_data;  pub use self::iri_data::*;
mod literal_kind; pub use self::literal_kind::*;

/// Generic type for RDF terms.
///
/// See [module documentation](index.html) for more detail.
/// 
#[derive(Clone,Copy,Debug,Eq,Hash)]
pub enum Term<T: Borrow<str>> {
    Iri(IriData<T>),
    BNode(BNodeId<T>),
    Literal(T, LiteralKind<T>),
    Variable(T),
}
pub use self::Term::*;


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


impl<T> Term<T> where
    T: Borrow<str>,
{
    /// Return a copy of this term's underlying text.
    /// 
    /// NB: for literals, the value only conveys the literal value,
    /// *not* the datatype or the language tag.error
    /// 
    /// See also [`n3`](#method.n3).
    pub fn value(&self) -> String {
        match self {
            Iri(iri) => iri.to_string(),
            BNode(id) => String::from(id.borrow()),
            Literal(value, _) => String::from(value.borrow()),
            Variable(name) => String::from(name.borrow()),
        }
    }

    /// Return the [N3] serialization of this term.
    /// 
    /// [N3]: https://www.w3.org/DesignIssues/Notation3
    /// 
    pub fn n3(&self) -> String {
        ::serializers::nt::stringify_term(self)
    }
}

impl<T> Term<T> where
    T: Borrow<str>,
{
    /// Return a new IRI term from the given text.
    /// 
    /// May fail if `txt` is not a valid IRI.
    /// 
    pub fn new_iri<U> (iri: U) -> Result<Term<T>> where
        T: From<U>
    {
        Ok(Iri(IriData::new(T::from(iri), None)?))
    }

    /// Return a new IRI term from the two given parts (prefix and suffix).
    /// 
    /// May fail if the concatenation of `ns` and `suffix`
    /// does not produce a valid IRI.
    /// 
    pub fn new_iri2<U, V> (ns: U, suffix: V) -> Result<Term<T>> where
        T: From<U> + From<V>
    {
        Ok(Iri(IriData::new(T::from(ns), Some(T::from(suffix)))?))
    }

    /// Return a new blank node term with the given bnode ID.
    /// 
    /// Currently, this may never fail;
    /// however it returns a result for homogeneity with other constructor methods,
    /// and because future versions may be more picky regarding bnode IDs.
    pub fn new_bnode<U> (id: U) -> Result<Term<T>> where
        T: From<U>
    {
        Ok(BNode(BNodeId::new(T::from(id))))
    }

    /// Return a new literal term with the given value and language tag.
    /// 
    /// May fail if the language tag is not valid.
    pub fn new_literal_lang<U, V> (txt: U, lang: V) -> Result<Term<T>> where
        T: From<U> + From<V>
    {
        let tag = T::from(lang);
        match LangTag::from_str(tag.borrow()) {
            Err(msg) => Err(ErrorKind::InvalidLanguageTag(
                tag.borrow().to_string(), msg,
            ).into()),
            Ok(_) => Ok(Literal(T::from(txt), Lang(tag))),
        }
    }

    /// Return a new literal term with the given value and datatype.
    /// 
    /// May fail if `dt` is not a valid datatype.
    pub fn new_literal_dt<U> (txt: U, dt: Term<T>) -> Result<Term<T>> where
        T: From<U> + Debug
    {
        match dt {
            Iri(iri) => Ok(Literal(T::from(txt), Datatype(iri))),
            _ => Err(ErrorKind::InvalidDatatype(dt.n3()).into()),
        }
    }

    /// Return a new variable term with the given name.
    /// 
    /// May fail if `name` is not a valid variable name.
    pub fn new_variable<U> (name: U) -> Result<Term<T>> where
        T: From<U>
    {
        let name = T::from(name);
        if N3_VARIABLE_NAME.is_match(name.borrow()) {
            Ok(Variable(name))
        } else {
            Err(ErrorKind::InvalidVariableName(
                name.borrow().to_string()
            ).into())
        }
    }

    /// Copy another term with the given factory.
    pub fn from_with<'a, U, F> (other: &'a Term<U>, mut factory: F) -> Term<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Iri(iri)
                => Iri(IriData::from_with(&iri, factory)),
            BNode(id)
                => BNode(BNodeId::from_with(&id, factory)),
            Literal(value, kind)
                => Literal(factory(value.borrow()),
                           LiteralKind::from_with(kind, factory)),
            Variable(name)
                => Variable(factory(name.borrow())),
        }
    }

    /// Copy another term with the given factory,
    /// applying the given normalization policy.
    pub fn normalized_with<'a, U, F> (other: &'a Term<U>, mut factory: F, norm: Normalization) -> Term<T> where
        U: Borrow<str>,
        F: FnMut(&str) -> T,
    {
        match other {
            Iri(iri)
                => Iri(IriData::normalized_with(&iri, factory, norm)),
            Literal(value, kind)
                => Literal(factory(value.borrow()),
                           LiteralKind::normalized_with(kind, factory, norm)),
            _
                => Self::from_with(other, factory)
        }
    }

    /// Return a new IRI term,
    /// assuming that `iri` is a valid IRI,
    /// and that `abs` correctly indicates whether it is absolute or relative.
    pub unsafe fn new_iri_unchecked<U> (iri: U, abs: Option<bool>) -> Term<T> where
        T: From<U>
    {
        Iri(IriData::new_unchecked(T::from(iri), None, abs))
    }

    /// Return a new IRI term,
    /// assuming that `ns` and `suffix` concatenate to a valid IRI,
    /// and that `abs` correctly indicates whether it is absolute or relative.
    pub unsafe fn new_iri2_unchecked<U, V> (ns: U, suffix: V, abs: Option<bool>) -> Term<T> where
        T: From<U> + From<V>
    {
        Iri(IriData::new_unchecked(T::from(ns), Some(T::from(suffix)), abs))
    }

    /// Return a new blank node term,
    /// assuming that `id` is a valid bnode ID.
    pub unsafe fn new_bnode_unchecked<U> (id: U) -> Term<T> where
        T: From<U>
    {
        BNode(BNodeId::new(T::from(id)))
    }

    /// Return a literal term,
    /// assuming that `lang` is a valid language tag.
    pub unsafe fn new_literal_lang_unchecked<U, V> (txt: U, lang: V) -> Term<T> where
        T: From<U> + From<V>
    {
        Literal(T::from(txt), Lang(T::from(lang)))
    }

    /// Return a typed literal term.
    /// 
    /// # Panics
    /// Panics if `dt` is not an IRI.
    pub unsafe fn new_literal_dt_unchecked<U> (txt: U, dt: Term<T>) -> Term<T> where
        T: From<U> + Debug
    {
        if let Iri(dt) = dt {
            Literal(T::from(txt), Datatype(dt))
        } else {
            panic!(format!("new_literal_dt_unchecked expects Term::Iri as dt, got {:?}", dt))
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
    pub fn join<U> (&self, t: &Term<U>) -> Term<U> where
        U: Borrow<str> + Clone + From<String>,
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
    pub fn batch_join<'a, F, U> (&self, task: F) where
        F: FnOnce(&Fn(&Term<U>) -> Term<U>) -> (),
        U: Borrow<str> + Clone + From<String>,
    {
        match self {
            Iri(iri) if iri.is_absolute() => {
                let iri_txt = iri.to_string();
                let base = ParsedIri::new(&iri_txt).unwrap();
                task(&|t| {
                    match t {
                        Iri(ref iri)
                            => Iri(base.join_iri(iri)),
                        Literal(ref txt, Datatype(ref iri))
                            => Literal(txt.clone(), Datatype(base.join_iri(iri))),
                        _
                            => t.clone(),
                    }
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

impl<T, U> PartialEq<Term<U>> for Term<T> where
    T: Borrow<str>,
    U: Borrow<str>,
{
    fn eq(&self, other: &Term<U>) -> bool {
        match (self, other) {
            (Iri(iri1), Iri(iri2))
                => iri1 == iri2,
            (BNode(id1), BNode(id2))
                => id1 == id2,
            (Literal(value1, kind1), Literal(value2, kind2))
                => value1.borrow() == value2.borrow() && kind1 == kind2,
            (Variable(name1), Variable(name2))
                => name1.borrow() == name2.borrow(),
            _ => false,
        }
    }
}

impl<'a, T, U> From<&'a Term<U>> for Term<T> where
        T: Borrow<str> + From<&'a str>,
        U: Borrow<str>,
{
    fn from(other: &'a Term<U>) -> Term<T> {
        Self::from_with(other, T::from)
    }
}





lazy_static! {
    static ref N3_VARIABLE_NAME: Regex = Regex::new(r"(?x)
      ^
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9]
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]*
      $
    ").unwrap();
}

#[cfg(test)]
pub(crate) mod test;
