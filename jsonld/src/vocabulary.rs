//! A Sophia-friendly implementation of [`rdf_types::Vocabulary`]
use std::sync::Arc;

use rdf_types::vocabulary::{
    BlankIdVocabulary, BlankIdVocabularyMut, IriVocabulary, IriVocabularyMut, LiteralVocabulary,
    LiteralVocabularyMut,
};
use sophia_api::{
    MownStr,
    term::{BnodeId, Term, TermKind},
};

use sophia_iri::Iri as SophiaIri;

/// Self-explanatory IRI index for JSON-LD processing.
pub type ArcIri = SophiaIri<Arc<str>>;

/// A [`Vocabulary`](rdf_types::Vocabulary) using `Arc<str>`-based types as index.
///
/// These types allow efficient conversion to/from sophia_iri and rdf-types types.
#[derive(Clone, Debug, Default)]
pub struct ArcVoc {}

impl IriVocabulary for ArcVoc {
    type Iri = ArcIri;

    fn iri<'i>(&'i self, id: &'i Self::Iri) -> Option<&'i iref::Iri> {
        // SAFETY: ArcIri is always a valid IRI
        Some(unsafe { iref::Iri::new_unchecked(id.as_str()) })
    }

    fn get(&self, iri: &iref::Iri) -> Option<Self::Iri> {
        Some(SophiaIri::new_unchecked(Arc::from(iri.as_str())))
    }
}

impl BlankIdVocabulary for ArcVoc {
    type BlankId = ArcBnode;

    fn blank_id<'b>(&'b self, id: &'b Self::BlankId) -> Option<&'b rdf_types::BlankId> {
        Some(rdf_types::BlankId::new(id.as_ref()).unwrap())
    }

    fn get_blank_id(&self, id: &rdf_types::BlankId) -> Option<Self::BlankId> {
        Some(ArcBnode {
            ident: Arc::from(id.as_str()),
        })
    }
}

impl LiteralVocabulary for ArcVoc {
    type Literal = rdf_types::Literal<ArcIri>;

    fn literal<'l>(&'l self, id: &'l Self::Literal) -> Option<rdf_types::LiteralRef<'l, ArcIri>> {
        Some(id.as_ref())
    }

    fn get_literal(&self, id: rdf_types::LiteralRef<'_, ArcIri>) -> Option<Self::Literal> {
        Some(id.into_owned())
    }

    fn owned_literal(
        &self,
        id: Self::Literal,
    ) -> Result<rdf_types::Literal<Self::Iri>, Self::Literal> {
        Ok(id)
    }
}

impl IriVocabularyMut for ArcVoc {
    fn insert(&mut self, iri: &iref::Iri) -> Self::Iri {
        SophiaIri::new_unchecked(Arc::from(iri.as_str()))
    }
}

impl BlankIdVocabularyMut for ArcVoc {
    fn insert_blank_id(&mut self, id: &rdf_types::BlankId) -> Self::BlankId {
        self.get_blank_id(id).unwrap()
    }
}

impl LiteralVocabularyMut for ArcVoc {
    fn insert_literal(&mut self, value: rdf_types::LiteralRef<'_, ArcIri>) -> Self::Literal {
        self.get_literal(value).unwrap()
    }
}

/// Self-explanatory bnode index for JSON-LD processing.
///
/// We are not using Sophia's `BnodeId` because it does not allocate the '_:'.
/// Since instances of this type are always created via [`ArcVoc`] from a valid bnode identifier,
/// we don't need to implement any validity check.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ArcBnode {
    ident: Arc<str>,
}

impl std::ops::Deref for ArcBnode {
    type Target = Arc<str>;

    fn deref(&self) -> &Self::Target {
        &self.ident
    }
}

impl Term for ArcBnode {
    type BorrowTerm<'x>
        = BnodeId<&'x str>
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::BlankNode
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        BnodeId::new_unchecked(&self[2..])
    }

    fn bnode_id(&self) -> Option<BnodeId<sophia_api::MownStr<'_>>> {
        Some(BnodeId::new_unchecked(MownStr::from_ref(&self[2..])))
    }
}
