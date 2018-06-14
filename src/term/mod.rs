use std::borrow::Borrow;
use std::fmt::Debug;
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use language_tag::LangTag;
use regex::Regex;

pub mod factory;
pub mod iri;
mod iri_term;  pub use self::iri_term::*;
mod bnode_id;  pub use self::bnode_id::*;
mod literal_kind; pub use self::literal_kind::*;

#[derive(Clone,Debug,Eq,Hash)]
pub enum Term<T: Borrow<str>> {
    Iri(IriTerm<T>),
    BNode(BNodeId<T>),
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
            Iri(iri) => iri.to_string(),
            BNode(id) => String::from(id.borrow()),
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
        Ok(Iri(IriTerm::new(T::from(iri), None)?))
    }

    pub fn new_iri2<U, V> (ns: U, suffix: V) -> Result<Term<T>, Err> where
        T: From<U> + From<V>
    {
        Ok(Iri(IriTerm::new(T::from(ns), Some(T::from(suffix)))?))
    }

    pub fn new_bnode<U> (id: U) -> Result<Term<T>, Err> where
        T: From<U>
    {
        Ok(BNode(BNodeId::new(T::from(id))))
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

    pub fn from_with<'a, U, F> (other: &'a Term<U>, mut factory: F) -> Term<T> where
        U: Borrow<str>,
        F: FnMut(&'a str) -> T,
    {
        match other {
            Iri(iri)
                => Iri(IriTerm::from_with(&iri, factory)),
            BNode(id)
                => BNode(BNodeId::from_with(&id, factory)),
            Literal(value, kind)
                => Literal(factory(value.borrow()),
                           LiteralKind::from_with(kind, factory)),
            Variable(name)
                => Variable(factory(name.borrow())),
        }
    }

    pub unsafe fn new_iri_unchecked<U> (iri: U, abs: Option<bool>) -> Term<T> where
        T: From<U>
    {
        Iri(IriTerm::new_unchecked(T::from(iri), None, abs))
    }

    pub unsafe fn new_iri2_unchecked<U, V> (ns: U, suffix: V, abs: Option<bool>) -> Term<T> where
        T: From<U> + From<V>
    {
        Iri(IriTerm::new_unchecked(T::from(ns), Some(T::from(suffix)), abs))
    }

    pub unsafe fn new_bnode_unchecked<U> (id: U) -> Term<T> where
        T: From<U>
    {
        BNode(BNodeId::new(T::from(id)))
    }

    pub unsafe fn new_literal_lang_unchecked<U, V> (txt: U, lang: V) -> Term<T> where
        T: From<U> + From<V>
    {
        Literal(T::from(txt), Lang(T::from(lang)))
    }

    pub unsafe fn new_literal_dt_unchecked<U> (txt: U, dt: Term<T>) -> Term<T> where
        T: From<U> + Debug
    {
        if let Iri(dt) = dt {
            Literal(T::from(txt), Datatype(dt))
        } else {
            panic!(format!("new_literal_dt_unchecked expects Term::Iri as dt, got {:?}", dt))
        }
    }

    /// If `t` is a relative IRI, replace it with an absolute one,
    /// using this term as the base.
    /// Otherwise, returns `t` unchanged.
    /// 
    /// # Example
    /// ```
    /// use myrdf2::term::*;
    ///
    /// let i1 = BoxTerm::new_iri("http://example.org/foo/bar").unwrap();
    /// let i2 = BoxTerm::new_iri("../baz").unwrap();
    /// let i3 = i1.join(&i2);
    /// assert_eq!(&i3.value(), "http://example.org/baz");
    /// ```
    ///
    /// # Panics
    /// Panics if this Term is not an IRI or is not absolute (see is_absolute (TODO link)).
    /// 
    /// # Performance
    /// If you need to join multiple terms to the same base,
    /// you should use `batch_join` (TODO link) instead,
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
    /// where `join` is a function comparable to the `join` method (TODO link).
    /// Useful for joining multiple terms with this IRI.
    /// 
    /// # Example
    /// ```
    /// use myrdf2::term::*;
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
    /// Panics if this Term is not an IRI or is not absolute (see is_absolute (TODO link)).
    ///
    pub fn batch_join<'a, F, U> (&self, task: F) where
        F: FnOnce(&Fn(&Term<U>) -> Term<U>) -> (),
        U: Borrow<str> + Clone + From<String>,
    {
        match self {
            Iri(iri) if iri.is_absolute() => {
                let iri_txt = iri.to_string();
                let base = iri::ParsedIri::new(&iri_txt).unwrap();
                task(&|t| {
                    match t {
                        Iri(ref iri)
                            => Iri(base.join_iriterm(iri)),
                        Literal(ref txt, Datatype(ref iri))
                            => Literal(txt.clone(), Datatype(base.join_iriterm(iri))),
                        _ 
                            => t.clone(),
                    }
                });
            }
            _ => panic!("Can only join with absolute Iri"),
        }
        
    }


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
    InvalidIri(String),
    InvalidLanguageTag(String),
    InvalidVariableName(String),
    InvalidPrefix(String), // useful for parsers dealing with PNames
    Other(String),
}

#[cfg(test)]
mod test;
