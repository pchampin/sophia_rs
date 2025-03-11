#![allow(clippy::module_name_repetitions)]

use std::{
    fmt,
    sync::{Arc, OnceLock},
};

use sophia_api::{
    MownStr,
    term::{SimpleTerm, Term},
};
use sophia_term::ArcTerm;

use crate::value::SparqlValue;

#[derive(Clone, Debug)]
pub struct ResultTerm {
    inner: ArcTerm,
    value: OnceLock<Option<SparqlValue>>,
}

impl Term for ResultTerm {
    type BorrowTerm<'x>
        = &'x ArcTerm
    where
        Self: 'x;

    fn kind(&self) -> sophia_api::term::TermKind {
        self.inner.kind()
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        &self.inner
    }

    fn is_iri(&self) -> bool {
        self.inner.is_iri()
    }

    fn is_blank_node(&self) -> bool {
        self.inner.is_blank_node()
    }

    fn is_literal(&self) -> bool {
        self.inner.is_literal()
    }

    fn is_variable(&self) -> bool {
        self.inner.is_variable()
    }

    fn is_atom(&self) -> bool {
        self.inner.is_atom()
    }

    fn is_triple(&self) -> bool {
        self.inner.is_triple()
    }

    fn iri(&self) -> Option<sophia_api::term::IriRef<MownStr>> {
        self.inner.iri()
    }

    fn bnode_id(&self) -> Option<sophia_api::term::BnodeId<MownStr>> {
        self.inner.bnode_id()
    }

    fn lexical_form(&self) -> Option<MownStr> {
        self.inner.lexical_form()
    }

    fn datatype(&self) -> Option<sophia_api::term::IriRef<MownStr>> {
        self.inner.datatype()
    }

    fn language_tag(&self) -> Option<sophia_api::term::LanguageTag<MownStr>> {
        self.inner.language_tag()
    }

    fn variable(&self) -> Option<sophia_api::term::VarName<MownStr>> {
        self.inner.variable()
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        self.inner.triple()
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        self.inner.to_triple().map(|tr| tr.map(From::from))
    }
}

impl ResultTerm {
    pub(crate) fn from_parts(inner: ArcTerm, value: Option<SparqlValue>) -> Self {
        let value = value.into();
        Self { inner, value }
    }

    /// The inner `ArcTerm`
    pub fn inner(&self) -> &ArcTerm {
        &self.inner
    }

    /// The parsed value of this term, if any.
    pub fn value(&self) -> Option<&SparqlValue> {
        self.value
            .get_or_init(|| SparqlValue::try_from_term(&self.inner))
            .as_ref()
    }

    /// Unwrap the inner `ArcTerm`
    pub fn unwrap(self) -> ArcTerm {
        self.inner
    }
}

impl From<ArcTerm> for ResultTerm {
    fn from(inner: ArcTerm) -> Self {
        Self {
            inner,
            value: OnceLock::new(),
        }
    }
}

impl From<[ResultTerm; 3]> for ResultTerm {
    fn from([s, p, o]: [ResultTerm; 3]) -> Self {
        Self {
            inner: ArcTerm::Triple(Arc::new([s.inner, p.inner, o.inner])),
            value: OnceLock::new(),
        }
    }
}

impl<T: Term> PartialEq<T> for ResultTerm {
    fn eq(&self, other: &T) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl Eq for ResultTerm {}

impl std::hash::Hash for ResultTerm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state);
    }
}

impl<T: Term> PartialOrd<T> for ResultTerm {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        Some(Term::cmp(self, other.borrow_term()))
    }
}

impl Ord for ResultTerm {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Term::cmp(self, other.borrow_term())
    }
}

impl fmt::Display for ResultTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_simple_term(f, &self.as_simple())
    }
}

fn write_simple_term(f: &mut fmt::Formatter<'_>, t: &SimpleTerm<'_>) -> fmt::Result {
    use SimpleTerm::*;
    match t {
        Iri(iri) => write!(f, "<{iri}>"),
        BlankNode(bnid) => write!(f, "_:{}", bnid.as_str()),
        LiteralDatatype(lex, dt) => write!(f, "{lex:?}^^<{dt}>"),
        LiteralLanguage(lex, tag) => write!(f, "{lex:?}@{}", tag.as_str()),
        Triple(spo) => {
            write!(f, "<< ")?;
            write_simple_term(f, &spo[0])?;
            write!(f, " ")?;
            write_simple_term(f, &spo[1])?;
            write!(f, " ")?;
            write_simple_term(f, &spo[2])?;
            write!(f, " >>")
        }
        Variable(varname) => write!(f, "?{}", varname.as_str()),
    }
}
