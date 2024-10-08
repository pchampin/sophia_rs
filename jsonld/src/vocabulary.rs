//! A Sophia-friendly implementation of [`rdf_types::Vocabulary`]
use std::sync::Arc;

use rdf_types::{
    BlankIdVocabulary, BlankIdVocabularyMut, IriVocabulary, IriVocabularyMut,
    LanguageTagVocabulary, LanguageTagVocabularyMut, LiteralVocabulary, LiteralVocabularyMut,
};
use sophia_api::{
    term::{BnodeId, Term, TermKind},
    MownStr,
};

/// Self-explanatory IRI index for JSON-LD processing.
pub type ArcIri = sophia_iri::Iri<Arc<str>>;
/// Self-explanatory language tag index for JSON-LD processing.
pub type ArcTag = sophia_api::term::LanguageTag<Arc<str>>;

/// A [`Vocabulary`](rdf_types::Vocabulary) using `Arc<str>`-based types as index.
///
/// These types match the notion of index in [`rdf_types`] as they are relatively cheap to clone,
/// but offer the advantage of being self-explanatory, and resolvable to actual terms without actually needing the vocabulary.
#[derive(Clone, Debug, Default)]
pub struct ArcVoc {}

impl IriVocabulary for ArcVoc {
    type Iri = ArcIri;

    fn iri<'i>(&'i self, id: &'i Self::Iri) -> Option<iref::Iri<'i>> {
        Some(iref::Iri::new(id.as_str()).unwrap())
    }

    fn get(&self, iri: iref::Iri) -> Option<Self::Iri> {
        Some(ArcIri::new_unchecked(Arc::from(iri.as_str())))
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
    type Literal = rdf_types::Literal<Self::Type, Self::Value>;

    type Type = rdf_types::literal::Type<ArcIri, ArcTag>;

    type Value = String;
    // I would rather use Arc<str>, but ToRdf::cloned_quads require that Value=String

    fn literal<'l>(
        &'l self,
        id: &'l Self::Literal,
    ) -> Option<&'l rdf_types::Literal<Self::Type, Self::Value>> {
        Some(id)
    }

    fn get_literal(
        &self,
        id: &rdf_types::Literal<Self::Type, Self::Value>,
    ) -> Option<Self::Literal> {
        Some(id.to_owned())
    }
}

impl LanguageTagVocabulary for ArcVoc {
    type LanguageTag = ArcTag;

    fn language_tag<'l>(&'l self, id: &'l Self::LanguageTag) -> Option<langtag::LanguageTag<'l>> {
        Some(langtag::LanguageTag::parse(id.as_str()).unwrap())
    }

    fn get_language_tag(&self, id: langtag::LanguageTag) -> Option<Self::LanguageTag> {
        Some(ArcTag::new_unchecked(Arc::from(id.as_str())))
    }
}

impl IriVocabularyMut for ArcVoc {
    fn insert(&mut self, iri: iref::Iri) -> Self::Iri {
        self.get(iri).unwrap()
    }
}

impl BlankIdVocabularyMut for ArcVoc {
    fn insert_blank_id(&mut self, id: &rdf_types::BlankId) -> Self::BlankId {
        self.get_blank_id(id).unwrap()
    }
}

impl LiteralVocabularyMut for ArcVoc {
    fn insert_literal(
        &mut self,
        value: &rdf_types::Literal<Self::Type, Self::Value>,
    ) -> Self::Literal {
        self.get_literal(value).unwrap()
    }
}

impl LanguageTagVocabularyMut for ArcVoc {
    fn insert_language_tag(&mut self, value: langtag::LanguageTag) -> Self::LanguageTag {
        self.get_language_tag(value).unwrap()
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
    type BorrowTerm<'x> = BnodeId<&'x str> where Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::BlankNode
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        BnodeId::new_unchecked(&self[2..])
    }

    fn bnode_id(&self) -> Option<BnodeId<sophia_api::MownStr>> {
        Some(BnodeId::new_unchecked(MownStr::from_ref(&self[2..])))
    }
}
