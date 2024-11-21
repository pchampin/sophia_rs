use super::*;

#[derive(Clone, Copy, Debug)]
pub enum AnyPattern<'a> {
    Term(&'a TermPattern),
    Named(&'a NamedNodePattern),
}

impl<'a> From<&'a TermPattern> for AnyPattern<'a> {
    fn from(value: &'a TermPattern) -> Self {
        AnyPattern::Term(value)
    }
}

impl<'a> From<&'a NamedNodePattern> for AnyPattern<'a> {
    fn from(value: &'a NamedNodePattern) -> Self {
        AnyPattern::Named(value)
    }
}

impl<'a> Term for AnyPattern<'a> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).kind(),
            AnyPattern::Named(nnp) => Ox2So::new_ref(*nnp).kind(),
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }

    fn iri(&self) -> Option<IriRef<MownStr>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).iri(),
            AnyPattern::Named(nnp) => Ox2So::new_ref(*nnp).iri(),
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).bnode_id(),
            AnyPattern::Named(nnp) => None,
        }
    }

    fn lexical_form(&self) -> Option<MownStr> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).lexical_form(),
            AnyPattern::Named(nnp) => None,
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).datatype(),
            AnyPattern::Named(nnp) => None,
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).language_tag(),
            AnyPattern::Named(nnp) => None,
        }
    }

    fn variable(&self) -> Option<VarName<MownStr>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).variable(),
            AnyPattern::Named(nnp) => Ox2So::new_ref(*nnp).variable(),
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).triple(),
            AnyPattern::Named(nnp) => None,
        }
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(tp).triple(),
            AnyPattern::Named(nnp) => None,
        }
    }
}
