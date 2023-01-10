use std::rc::Rc;

use sophia_api::term::{BnodeId, Term, TermKind};

#[derive(Clone, Debug)]
pub enum C14nTerm<T: Term> {
    Blank(BnodeId<Rc<str>>),
    Other(T),
}
use C14nTerm::*;

impl<T: Term> Term for C14nTerm<T> {
    type BorrowTerm<'x> = &'x Self where Self: 'x;

    fn kind(&self) -> sophia_api::term::TermKind {
        match self {
            Blank(_) => TermKind::BlankNode,
            Other(t) => t.kind(),
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn iri(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr>> {
        match self {
            Blank(_) => None,
            Other(t) => t.iri(),
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<sophia_api::MownStr>> {
        match self {
            Blank(b) => Some(BnodeId::new_unchecked(b.as_str().into())),
            Other(t) => t.bnode_id(),
        }
    }

    fn lexical_form(&self) -> Option<sophia_api::MownStr> {
        match self {
            Blank(_) => None,
            Other(t) => t.lexical_form(),
        }
    }

    fn datatype(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr>> {
        match self {
            Blank(_) => None,
            Other(t) => t.datatype(),
        }
    }

    fn language_tag(&self) -> Option<sophia_api::term::LanguageTag<sophia_api::MownStr>> {
        match self {
            Blank(_) => None,
            Other(t) => t.language_tag(),
        }
    }

    fn variable(&self) -> Option<sophia_api::term::VarName<sophia_api::MownStr>> {
        self.is_variable()
            .then(|| unimplemented!("Default implementation should have been overridden"))
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        unimplemented!()
        // when we need to support RDF-star, the idea will probably be to split
        // Other() into Atom(T) and Triple(Box[Self; 3])
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        unimplemented!()
    }
}

// TODO test
