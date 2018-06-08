use std::borrow::Borrow;
use std::fmt::Debug;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use language_tag::LangTag;
use regex::Regex;
use url::{ParseError,Url};

pub mod factory;
mod iri_term;  pub use self::iri_term::*;
mod literal_kind; pub use self::literal_kind::*;

#[derive(Clone,Debug,Eq,Hash)]
pub enum Term<T: Borrow<str>> {
    Iri(IriTerm<T>),
    BNode(T, bool),
    Literal(T, LiteralKind<T>),
    Variable(T),
}
use self::Term::*;

pub type BoxTerm = Term<Box<str>>;
pub type RcTerm = Term<Rc<str>>;
pub type ArcTerm = Term<Arc<str>>;
pub type RefTerm<'a> = Term<&'a str>;
pub type StaticTerm = RefTerm<'static>;

impl<T> Term<T> where
    T: Borrow<str>,
{
    pub fn value(&self) -> String {
        match self {
            Iri(iri) => iri.value(),
            BNode(id, _) => String::from(id.borrow()),
            Literal(value, _) => String::from(value.borrow()),
            Variable(name) => String::from(name.borrow()),
        }
    }
}

impl<T> Term<T> where
    T: Borrow<str>,
{
    pub fn new_iri<U> (iri: U) -> Result<Term<T>, Err> where
        T: From<U>
    {
        Ok(Iri(IriTerm::from1(iri)?))
    }

    pub fn new_iri2<U, V> (ns: U, suffix: V) -> Result<Term<T>, Err> where
        T: From<U> + From<V>
    {
        Ok(Iri(IriTerm::from2(ns, suffix)?))
    }

    pub fn new_bnode<U> (id: U) -> Result<Term<T>, Err> where
        T: From<U>
    {
        let id = T::from(id);
        let n3 = N3_BNODE_ID.is_match(id.borrow());
        Ok(BNode(id, n3))
    }
    
    pub fn new_literal_lang<U, V> (txt: U, lang: V) -> Result<Term<T>, Err> where
        T: From<U> + From<V>
    {
        let tag = T::from(lang);
        match LangTag::from_str(tag.borrow()) {
            Err(err) => Err(Err::InvalidLanguageTag(err)),
            Ok(_) => Ok(Literal(T::from(txt), Lang(tag))),
        }
    }

    pub fn new_literal_dt<U> (txt: U, dt: Term<T>) -> Result<Term<T>, Err> where
        T: From<U> + Debug
    {
        match dt {
            Iri(iri) => Ok(Literal(T::from(txt), Datatype(iri))),
            _ => Err(Err::InvalidDatatype(format!("{:?}", dt))),
        }
    }

    pub fn new_variable<U> (name: U) -> Result<Term<T>, Err> where
        T: From<U>
    {
        let name = T::from(name);
        if N3_VARIABLE_NAME.is_match(name.borrow()) {
            Ok(Variable(name))
        } else {
            Err(Err::InvalidVariableName(String::from(name.borrow())))
        }
        
    }

    pub fn copy_with<'a, U, F> (other: &'a Term<U>, factory: &mut F) -> Term<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Iri(iri)
                => Iri(IriTerm::copy_with(&iri, factory)),
            BNode(id, n3)
                => BNode(factory(id.borrow()), *n3),
            Literal(value, kind)
                => Literal(factory(value.borrow()),
                           LiteralKind::copy_with(kind, factory)),
            Variable(name)
                => Variable(factory(name.borrow())),
        }
    }

    pub fn copy<'a, U> (other: &'a Term<U>) -> Term<T> where
        T: From<&'a str>,
        U: Borrow<str>,
    {
        Self::copy_with(other, &mut T::from)
    }

    pub unsafe fn trusted_iri<U> (iri: U) -> Term<T> where
        T: From<U>
    {
        Iri(IriTerm::trusted1(iri))
    }

    pub unsafe fn trusted_iri2<U, V> (ns: U, suffix: V) -> Term<T> where
        T: From<U> + From<V>
    {
        Iri(IriTerm::trusted2(ns, suffix))
    }

    pub unsafe fn trusted_bnode<U> (id: U, n3: bool) -> Term<T> where
        T: From<U>
    {
        BNode(T::from(id), n3)
    }

    pub unsafe fn trusted_literal_lang<U, V> (txt: U, lang: V) -> Term<T> where
        T: From<U> + From<V>
    {
        Literal(T::from(txt), Lang(T::from(lang)))
    }

    pub unsafe fn trusted_literal_dt<U> (txt: U, dt: Term<T>) -> Term<T> where
        T: From<U> + Debug
    {
        if let Iri(dt) = dt {
            Literal(T::from(txt), Datatype(dt))
        } else {
            panic!(format!("trusted_literal_dt expects Term::Iri as dt, got {:?}", dt))
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
            (BNode(id1, _), BNode(id2, _))
                => id1.borrow() == id2.borrow(),
            (Literal(value1, kind1), Literal(value2, kind2))
                => value1.borrow() == value2.borrow() && kind1 == kind2,
            (Variable(name1), Variable(name2))
                => name1.borrow() == name2.borrow(),
            _ => false,
        }
    }
}



lazy_static! {
    pub static ref N3_BNODE_ID: Regex = Regex::new(r"(?x)
      ^
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9]
      (
          [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
          |
          \u{2e} [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_\u{2d}0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]
      )*
      $
    ").unwrap();

    pub static ref N3_VARIABLE_NAME: Regex = Regex::new(r"(?x)
      ^
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9]
      [A-Za-z\u{c0}-\u{d6}\u{d8}-\u{f6}\u{f8}-\u{2ff}\u{370}-\u{37D}\u{37F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}]*
      $
    ").unwrap();
}



#[derive(Debug)]
pub enum Err {
    InvalidDatatype(String),
    InvalidIri(ParseError),
    InvalidLanguageTag(String),
    InvalidVariableName(String),
    InvalidPrefix(String), // useful for parsers dealing with PNames
    Other(String),
}

#[cfg(test)]
mod test;
