use sophia_api::term::{BnodeId, IriRef, Term, TermKind};
use std::{cmp::Ordering, rc::Rc};

use crate::_cnq::nq;

#[derive(Clone, Debug)]
pub enum C14nTerm<T: Term> {
    /// Blank node renamed by the C14n process
    /// (or inserted, in the case of the generalized algorithm)
    Blank(BnodeId<Rc<str>>),
    /// Reserved to the generalized algorithm, to insert new IRIs in the aaa: scheme
    Aaa(IriRef<Rc<str>>),
    /// Unmodified term from the dataset to canonicalize
    Other(T),
}
use C14nTerm::{Blank, Aaa, Other};

impl<T: Term> Term for C14nTerm<T> {
    type BorrowTerm<'x>
        = &'x Self
    where
        Self: 'x;

    fn kind(&self) -> sophia_api::term::TermKind {
        match self {
            Blank(_) => TermKind::BlankNode,
            Aaa(_) => TermKind::Iri,
            Other(t) => t.kind(),
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        self
    }

    fn iri(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr<'_>>> {
        match self {
            Blank(_) => None,
            Aaa(iri_ref) => Some(iri_ref.clone().map_unchecked(|txt| txt.to_string().into())),
            Other(t) => t.iri(),
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<sophia_api::MownStr<'_>>> {
        match self {
            Blank(b) => Some(BnodeId::new_unchecked(b.as_str().into())),
            Aaa(_) => None,
            Other(t) => t.bnode_id(),
        }
    }

    fn lexical_form(&self) -> Option<sophia_api::MownStr<'_>> {
        match self {
            Blank(_) => None,
            Aaa(_) => None,
            Other(t) => t.lexical_form(),
        }
    }

    fn datatype(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr<'_>>> {
        match self {
            Blank(_) => None,
            Aaa(_) => None,
            Other(t) => t.datatype(),
        }
    }

    fn language_tag(&self) -> Option<sophia_api::term::LanguageTag<sophia_api::MownStr<'_>>> {
        match self {
            Blank(_) => None,
            Aaa(_) => None,
            Other(t) => t.language_tag(),
        }
    }

    fn base_direction(&self) -> Option<sophia_api::term::BaseDirection> {
        match self {
            Blank(_) => None,
            Aaa(_) => None,
            Other(t) => t.base_direction(),
        }
    }

    fn variable(&self) -> Option<sophia_api::term::VarName<sophia_api::MownStr<'_>>> {
        match self {
            Blank(_) => None,
            Aaa(_) => None,
            Other(t) => t.variable(),
        }
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

impl<T: Term> PartialEq for C14nTerm<T> {
    fn eq(&self, other: &Self) -> bool {
        Term::eq(self, other)
    }
}

impl<T: Term> Eq for C14nTerm<T> {}

impl<T: Term> std::hash::Hash for C14nTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state)
    }
}

pub fn cmp_c14n_terms<'a, 'b, T: Term>(
    t1: Option<&'a C14nTerm<T>>,
    t2: Option<&'a C14nTerm<T>>,
    buf1: &'b mut String,
    buf2: &'b mut String,
) -> Ordering {
    if let Some(t1) = t1 {
        nq(t1, buf1).unwrap();
    }
    if let Some(t2) = t2 {
        nq(t2, buf2).unwrap();
    }
    buf1.cmp(&buf2)
}
