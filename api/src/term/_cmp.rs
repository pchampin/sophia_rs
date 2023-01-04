use super::*;

/// A wrapper for any term type that ensures comparability
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct CmpTerm<T>(pub T);

impl<T: Term> Term for CmpTerm<T> {
    type BorrowTerm<'x> = CmpTerm<T::BorrowTerm<'x>> where T: 'x;

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
    fn lexical_value(&self) -> Option<MownStr> {
        self.0.lexical_value()
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
        self.0.triple().map(|a| a.map(CmpTerm))
    }
    fn to_triple(self) -> Option<[Self; 3]> {
        self.0.to_triple().map(|a| a.map(CmpTerm))
    }
    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        CmpTerm(self.0.borrow_term())
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
    // NOT overridding the iterator methods
    // (constituents, to_constituents, atoms, to_atoms)
    // because this would introduce an additional Box<dyn ...> indirection,
    // potentially hurting performances,
    // beyond the benefit of a hypotherical custom impl of these methods in T.
}

impl<T1: Term, T2: Term> PartialEq<T2> for CmpTerm<T1> {
    fn eq(&self, other: &T2) -> bool {
        Term::eq(&self.0, other.borrow_term())
    }
}

impl<T: Term> Eq for CmpTerm<T> {}

impl<T1: Term, T2: Term> PartialOrd<T2> for CmpTerm<T1> {
    fn partial_cmp(&self, other: &T2) -> Option<Ordering> {
        Some(Term::cmp(&self.0, other.borrow_term()))
    }
}

impl<T: Term> Ord for CmpTerm<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        Term::cmp(&self.0, other.borrow_term())
    }
}

impl<T: Term> std::hash::Hash for CmpTerm<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(&self.0, state)
    }
}

impl<T: Term> AsRef<T> for CmpTerm<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T: Term> std::ops::Deref for CmpTerm<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: Term + FromTerm> FromTerm for CmpTerm<T> {
    fn from_term<U: Term>(term: U) -> Self {
        Self(term.into_term())
    }
}

impl<T: Term + TryFromTerm> TryFromTerm for CmpTerm<T> {
    type Error = T::Error;

    fn try_from_term<U: Term>(term: U) -> Result<Self, Self::Error> {
        term.try_into_term().map(Self)
    }
}
