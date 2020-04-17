//! Experimenting with a `Term` trait#[derive(Eq, PartialEq)]
use crate::mown_str::MownStr;

pub mod simple;
use simple::SimpleTerm;

/// A trait for types that can be used as terms in RDF graphs and datasets
///
/// The returned MownStr should borrow existing data as much as possible;
/// when no such data exists, MownStr still can own data produced on demand.
///
/// As a consequence, all methods from `Term` returning `MownStr`
/// should in general be assumed to be expensive.
/// An exception to this rule is the `SimpleTerm` type,
/// and will always return *borrowing* `MownStr`.
///
/// For this reason, this trait provides a `as_simple` method,
/// returning a `SimpleTerm`
/// constructed once and for all from return values of other methods
/// (a kind of "cache" of the data from this term).
///
/// This also allows for any two terms to be comparable,
/// provided that one of them is converted to a `SimpleTerm`.
pub trait Term: std::fmt::Debug + for <'x> PartialEq<SimpleTerm<'x>> {
    /// The kind of term (IRI, blank node, literal or variable)
    fn kind(&self) -> TermKind;
    /// The value of that term
    ///
    /// * the IRI itself for IRIs
    /// * the name/identifier for blank nodes or variable
    /// * the lexical value for literals
    fn value(&self) -> MownStr;
    /// If this term is a literal, return its datatype IRI
    fn datatype(&self) -> Option<MownStr>;
    /// If this term is a language-tagged literal, return its language tag
    fn language(&self) -> Option<MownStr>;
    /// cheap conversion to SimpleTerm
    fn as_simple(&self) -> SimpleTerm {
        match self.kind() {
            TermKind::Iri => SimpleTerm::new_iri_unchecked(self.value()),
            TermKind::Literal => match self.language() {
                Some(tag) => SimpleTerm::new_literal_lang(self.value(), tag),
                None => SimpleTerm::new_literal_dt(self.value(), self.datatype().unwrap()),
            }
            TermKind::BNode => SimpleTerm::new_bnode_unchecked(self.value()),
            TermKind::Variable => SimpleTerm::new_variable_unchecked(self.value()),
        }
    }
    /// All terms are absolute, except relative IRIs, and literals with a relative datatype IRI.
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

impl<'a> PartialEq<SimpleTerm<'a>> for i32 {
    fn eq(&self, other: &SimpleTerm<'a>) -> bool {
        other.kind() == TermKind::Literal && other.datatype() == Some(XSD_INTEGER) && other.value().parse::<i32>().map(|v| v==*self).unwrap_or(false)
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

impl<'a, 'b> PartialEq<SimpleTerm<'a>> for SuffixedIri<'b> {
    fn eq(&self, other: &SimpleTerm<'a>) -> bool {
        other.kind() == TermKind::Iri && {
            let iri = other.value();
            iri.starts_with(self.ns) && &iri[self.ns.len()..] == self.suffix.as_ref()
        }
    }
}
