//! I define [`IsoTerm`],
//! a [`Term`] wrapper implementing [`Ord`] and [`Eq`]
//! in a way that is convenient to compute graph/dataset isomorphism.
//!
//! More specifically, all blank nodes are considered equal.
use sophia_api::quad::Spog;
use sophia_api::term::{BnodeId, FromTerm, LanguageTag, Term, TermKind, TryFromTerm, VarName};
use sophia_api::MownStr;
use sophia_iri::IriRef;
use std::cmp::{Ordering, PartialOrd};

#[derive(Clone, Copy, Debug)]
pub struct IsoTerm<T>(pub(crate) T);

impl<T: Term> Term for IsoTerm<T> {
    type BorrowTerm<'x> = IsoTerm<T::BorrowTerm<'x>> where T: 'x;

    fn kind(&self) -> TermKind {
        self.0.kind()
    }
    fn is_iri(&self) -> bool {
        self.0.is_iri()
    }
    fn is_blank_node(&self) -> bool {
        self.0.is_blank_node()
    }
    fn is_literal(&self) -> bool {
        self.0.is_literal()
    }
    fn is_variable(&self) -> bool {
        self.0.is_variable()
    }
    fn is_atom(&self) -> bool {
        self.0.is_atom()
    }
    fn is_triple(&self) -> bool {
        self.0.is_triple()
    }
    fn iri(&self) -> Option<IriRef<MownStr>> {
        self.0.iri()
    }
    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        self.0.bnode_id()
    }
    fn lexical_form(&self) -> Option<MownStr> {
        self.0.lexical_form()
    }
    fn datatype(&self) -> Option<IriRef<MownStr>> {
        self.0.datatype()
    }
    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        self.0.language_tag()
    }
    fn variable(&self) -> Option<VarName<MownStr>> {
        self.0.variable()
    }
    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        self.0.triple().map(|a| a.map(IsoTerm))
    }
    fn to_triple(self) -> Option<[Self; 3]> {
        self.0.to_triple().map(|a| a.map(IsoTerm))
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        IsoTerm(self.0.borrow_term())
    }
    fn eq<U: Term>(&self, other: U) -> bool {
        self.0.eq(other)
    }
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
    fn into_term<U: FromTerm>(self) -> U {
        self.0.into_term()
    }
    fn try_into_term<U: TryFromTerm>(self) -> Result<U, U::Error> {
        self.0.try_into_term()
    }
    // NOT overriding the iterator methods
    // (constituents, to_constituents, atoms, to_atoms)
    // because this would introduce an additional Box<dyn ...> indirection,
    // potentially hurting performances,
    // beyond the benefit of a hypothetical custom impl of these methods in T.
}

impl<T1, T2> PartialEq<IsoTerm<T1>> for IsoTerm<T2>
where
    T1: Term,
    T2: Term,
{
    fn eq(&self, other: &IsoTerm<T1>) -> bool {
        use TermKind::BlankNode;
        if self.kind() == BlankNode && other.kind() == BlankNode {
            true
        } else {
            Term::eq(&self.0, other.0.borrow_term())
        }
    }
}

impl<T: Term> Eq for IsoTerm<T> {}

impl<T1, T2> PartialOrd<IsoTerm<T1>> for IsoTerm<T2>
where
    T1: Term,
    T2: Term,
{
    fn partial_cmp(&self, other: &IsoTerm<T1>) -> Option<Ordering> {
        use TermKind::BlankNode;
        if self.kind() == BlankNode && other.kind() == BlankNode {
            Some(Ordering::Equal)
        } else {
            Some(Term::cmp(&self.0, other.0.borrow_term()))
        }
    }
}

impl<T: Term> Ord for IsoTerm<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        use TermKind::BlankNode;
        if self.kind() == BlankNode && other.kind() == BlankNode {
            Ordering::Equal
        } else {
            Term::cmp(&self.0, other.0.borrow_term())
        }
    }
}

fn eq_gn<T1, T2>(g1: &Option<IsoTerm<T1>>, g2: &Option<IsoTerm<T2>>) -> bool
where
    T1: Term,
    T2: Term,
{
    match (g1, g2) {
        (None, None) => true,
        (Some(t1), Some(t2)) => t1 == t2,
        _ => false,
    }
}

pub fn eq_triples<T1, T2>((t1, t2): (&[IsoTerm<T1>; 3], &[IsoTerm<T2>; 3])) -> bool
where
    T1: Term,
    T2: Term,
{
    t1[0] == t2[0] && t1[1] == t2[1] && t1[2] == t2[2]
}

pub fn cmp_quads<T1, T2>((q1, q2): (&Spog<IsoTerm<T1>>, &Spog<IsoTerm<T2>>)) -> bool
where
    T1: Term,
    T2: Term,
{
    eq_triples((&q1.0, &q2.0)) && eq_gn(&q1.1, &q2.1)
}
