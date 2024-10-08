use sophia_api::term::{BnodeId, Term, TermKind};
use std::{cmp::Ordering, rc::Rc};

use crate::_cnq::nq;

#[derive(Clone, Debug)]
pub enum C14nTerm<T: Term> {
    Blank(BnodeId<Rc<str>>),
    Other(T),
}
use C14nTerm::{Blank, Other};

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
        // TODO when we need to support RDF-star,
        // a good way to implement it will probably be to split Other()
        // into Atom(T) and Triple(Box[Self; 3])
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        unimplemented!()
        // TODO when we need to support RDF-star,
        // see triple() above
    }
}

pub fn cmp_c14n_terms<'a, 'b, T: Term>(
    t1: Option<&'a C14nTerm<T>>,
    t2: Option<&'a C14nTerm<T>>,
    buf1: &'b mut String,
    buf2: &'b mut String,
) -> Ordering {
    if let Some(t1) = t1 {
        nq(t1, buf1);
    }
    if let Some(t2) = t2 {
        nq(t2, buf2);
    }
    buf1.cmp(&buf2)
}
