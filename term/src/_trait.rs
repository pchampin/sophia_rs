//! Experimenting with a `Term` trait#[derive(Eq, PartialEq)]


use crate::mown_str::MownStr;

/// A wraper around MownStr that guarantees that it contains a valid IRI ref
#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct MownIri<'a> (MownStr<'a>);
// TODO impl Deref, AsRef<str>, AsRef<MownStr>
// TODO provide constructor for MownIri checking that it is a valid IRI

impl<'a> MownIri<'a> {
    pub fn map<'b, F: FnOnce(MownStr<'a>) -> MownStr<'b>>(self, f: F) -> MownIri<'b> {
        MownIri(f(self.0))
    }
}

//

/// A trait for types that can be used as terms
pub trait Term: std::fmt::Debug {
    fn kind(&self) -> TermKind;
    fn value(&self) -> MownStr;
    fn iri(&self) -> Option<MownIri>;
    fn datatype(&self) -> Option<MownIri>;
    fn bnode_id(&self) -> Option<MownStr>;
    fn absolute(&self) -> bool {
        todo!()
        // provide default impl here,
        // checking that self.iri() and self.datatype() are None or contain an absolute IRI
    }
}

/// A trait for types that can be created from any term
pub trait FromTerm {
    fn from_term<T: Term>(other: &T) -> Self;
}

/// A trait for types that can be created from some terms
pub trait TryFromTerm: Sized {
    fn try_from_term<T: Term>(other: &T) -> Result<Self, TermConversionError>;
}

/// A function that can compare any two terms
///
/// If you want to avoid monomorphization, use term_eq_dyn instead
pub fn term_eq<T1: Term + ?Sized, T2: Term + ?Sized>(t1: &T1, t2: &T2) -> bool {
    t1.kind() == t2.kind() && t1.value() == t2.value()
    // TODO this impl is not accurate
}
/// A function that can compare any two terms
///
/// If you want to avoid trait objects, use term_eq instead
pub fn term_eq_dyn(t1: &dyn Term, t2: &dyn Term) -> bool {
    term_eq(t1, t2)
}

fn foo(t1: &MownIri, t2: i32) -> bool {
    term_eq(t1 as &dyn Term, &t2 as &dyn Term)
}

/// An enum of all kinds of terms
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum TermKind {
    Iri,
    Literal,
    BNode,
    Variable,
}

#[derive(Debug, thiserror::Error)]
pub enum TermConversionError {
    #[error("Unsupported term kind {0:?}")]
    UnsupportedTermKind(TermKind),
    #[error("Invalid lexical value {0:?}")]
    InvalidLexicalValue(String),
}
use TermConversionError::*;

//

/// MownIri implements Term and friends
impl<'a> Term for MownIri<'a> {
    fn kind(&self) -> TermKind { TermKind::Iri }
    fn value(&self) -> MownStr { self.clone().0 }
    fn iri(&self) -> Option<MownIri> { Some(self.clone()) }
    fn datatype(&self) -> Option<MownIri> { None }
    fn bnode_id(&self) -> Option<MownStr> { None }
}

impl<'a> TryFromTerm for MownIri<'a> {
    fn try_from_term<T: Term>(other: &T) -> Result<Self, TermConversionError> {
        if other.kind() != TermKind::Iri {
            Err(UnsupportedTermKind(other.kind()))
        } else {
            Ok(other.iri().unwrap().map(|ms| ms.as_own()))
        }
    }
}

//

/// Native types (such as i32) can also implement Term and friends
impl Term for i32 {
    fn kind(&self) -> TermKind { TermKind::Literal }
    fn value(&self) -> MownStr { format!("{}", self).into() }
    fn iri(&self) -> Option<MownIri> { None }
    fn datatype(&self) -> Option<MownIri> { Some(XSD_INTEGER) }
    fn bnode_id(&self) -> Option<MownStr> { None }
}

impl TryFromTerm for i32 {
    fn try_from_term<T: Term>(other: &T) -> Result<Self, TermConversionError> {
        if other.kind() != TermKind::Literal {
            Err(UnsupportedTermKind(other.kind()))
        } else {
            other.value().parse::<i32>().map_err(|_| TermConversionError::InvalidLexicalValue(other.value().into()))
        }
    }
}
const XSD_INTEGER: MownIri = MownIri(MownStr::Ref("http://www.w3.org/2001/XMLSchema#integer"));

//

/// Suffixed Iri (as produced by a Namespace object, for example)
#[derive(Clone, Debug)]
pub struct SuffixedIri<'a> {
    ns: &'a str,
    suffix: Box<str>,
}
// TODO constructor that checks the validity of the ns and suffix
// as well as constructor taking a MownIri for ns, avoiding the check

impl<'a> Term for SuffixedIri<'a> {
    fn kind(&self) -> TermKind { TermKind::Iri }
    fn value(&self) -> MownStr { format!("{}{}", self.ns, self.suffix).into() }
    fn iri(&self) -> Option<MownIri> { Some(MownIri(self.value())) }
    fn datatype(&self) -> Option<MownIri> { None }
    fn bnode_id(&self) -> Option<MownStr> { None }
}

//

/// Simple type implemeting 
pub enum MyTerm {
    Iri(Box<str>),
    BNode(Box<str>),
    Literal(Box<str>, Box<str>),
    Variable(Box<str>),
}
// TODO impl Term
