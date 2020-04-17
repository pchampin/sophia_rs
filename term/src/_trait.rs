//! Experimenting with a `Term` trait#[derive(Eq, PartialEq)]


use crate::mown_str::MownStr;

/// A trait for types that can be used as terms
pub trait Term: std::fmt::Debug {
    fn kind(&self) -> TermKind;
    fn value(&self) -> MownStr;
    fn datatype(&self) -> Option<MownStr>;
    fn language(&self) -> Option<MownStr>;
    fn absolute(&self) -> bool {
        todo!()
        // provide default impl here,
        // checking kind() and, depending, value() or datatype()
    }
}

/// An enum of all kinds of terms
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum TermKind {
    Iri,
    Literal,
    BNode,
    Variable,
}

/// A function that can compare any two terms
///
/// If you want to avoid monomorphization, use term_eq_dyn instead
pub fn term_eq<T1: Term + ?Sized, T2: Term + ?Sized>(t1: &T1, t2: &T2) -> bool {
    t1.kind() == t2.kind() && t1.value() == t2.value() && (t1.kind() != TermKind::Literal || t1.language() == t2.language() && t1.datatype() == t2.datatype())
}
/// A function that can compare any two terms
///
/// If you want to avoid trait objects, use term_eq instead
pub fn term_eq_dyn(t1: &dyn Term, t2: &dyn Term) -> bool {
    term_eq(t1, t2)
}

/// A trait for types that can be created from any term
pub trait FromTerm {
    fn from_term<T: Term>(other: &T) -> Self;
}

/// A trait for types that can be created from some terms
pub trait TryFromTerm: Sized {
    fn try_from_term<T: Term>(other: &T) -> Result<Self, TermConversionError>;
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

/// Native types (such as i32) can also implement Term and friends
impl Term for i32 {
    fn kind(&self) -> TermKind { TermKind::Literal }
    fn value(&self) -> MownStr { format!("{}", self).into() }
    fn datatype(&self) -> Option<MownStr> { Some(XSD_INTEGER) }
    fn language(&self) -> Option<MownStr> { None }
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
const XSD_INTEGER: MownStr = MownStr::Ref("http://www.w3.org/2001/XMLSchema#integer");

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
    fn datatype(&self) -> Option<MownStr> { None }
    fn language(&self) -> Option<MownStr> { None }
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
