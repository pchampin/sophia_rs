use rdf_types::{Id, Literal, LiteralType, Quad, Term};
use sophia_api::{
    MownStr,
    quad::Spog,
    term::{IriRef, Term as SophiaTerm, TermKind},
};

use crate::vocabulary::{ArcBnode, ArcIri};

/// Sophia wrapper for `rdf_type` types
#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct RdfTerm(RdfO);

impl SophiaTerm for RdfTerm {
    type BorrowTerm<'x>
        = &'x Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        match self.0 {
            Term::Id(Id::Blank(_)) => TermKind::BlankNode,
            Term::Id(Id::Iri(_)) => TermKind::Iri,
            Term::Literal(_) => TermKind::Literal,
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        match &self.0 {
            Term::Id(Id::Iri(iri)) => iri.iri(),
            _ => None,
        }
    }

    fn bnode_id(&self) -> Option<sophia_api::term::BnodeId<MownStr<'_>>> {
        match &self.0 {
            Term::Id(Id::Blank(bnid)) => bnid.bnode_id(),
            _ => None,
        }
    }

    fn lexical_form(&self) -> Option<MownStr<'_>> {
        match &self.0 {
            Term::Literal(lit) => Some(MownStr::from_ref(lit.as_value())),
            _ => None,
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        match &self.0 {
            Term::Literal(lit) => match &lit.type_ {
                LiteralType::Any(iri) => iri.iri(),
                LiteralType::LangString(_) => sophia_api::ns::rdf::langString.iri(),
            },
            _ => None,
        }
    }

    fn language_tag(&self) -> Option<sophia_api::term::LanguageTag<MownStr<'_>>> {
        match &self.0 {
            Term::Literal(lit) => match &lit.type_ {
                LiteralType::LangString(tag) => Some(sophia_api::term::LanguageTag::new_unchecked(
                    MownStr::from_ref(tag.as_str()),
                )),
                LiteralType::Any(_) => None,
            },
            _ => None,
        }
    }

    fn base_direction(&self) -> Option<sophia_api::term::BaseDirection> {
        None
    }
}

impl From<RdfO> for RdfTerm {
    fn from(value: RdfO) -> Self {
        Self(value)
    }
}

impl From<RdfS> for RdfTerm {
    fn from(value: RdfS) -> Self {
        Self(Term::Id(value))
    }
}

impl From<ArcIri> for RdfTerm {
    fn from(value: ArcIri) -> Self {
        Self(Term::Id(Id::Iri(value)))
    }
}

pub fn convert_quad(q: RdfQuad) -> Spog<RdfTerm> {
    (
        [RdfTerm::from(q.0), RdfTerm::from(q.1), RdfTerm::from(q.2)],
        q.3.map(RdfTerm::from),
    )
}

type RdfS = Id<ArcIri, ArcBnode>;
type RdfP = Id<ArcIri, ArcBnode>;
type RdfLit = Literal<ArcIri>;
type RdfO = Term<RdfS, RdfLit>;
type RdfQuad = Quad<RdfS, RdfP, RdfO, RdfS>;
