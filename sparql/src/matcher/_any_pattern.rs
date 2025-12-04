use sophia_api::term::BaseDirection;

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

impl Term for AnyPattern<'_> {
    type BorrowTerm<'x>
        = Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).kind(),
            AnyPattern::Named(nnp) => Ox2So::new_ref(*nnp).kind(),
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).iri(),
            AnyPattern::Named(nnp) => Ox2So::new_ref(*nnp).iri(),
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).bnode_id(),
            AnyPattern::Named(_) => None,
        }
    }

    fn lexical_form(&self) -> Option<MownStr<'_>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).lexical_form(),
            AnyPattern::Named(_) => None,
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).datatype(),
            AnyPattern::Named(_) => None,
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr<'_>>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).language_tag(),
            AnyPattern::Named(_) => None,
        }
    }

    fn base_direction(&self) -> Option<BaseDirection> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).base_direction(),
            AnyPattern::Named(_) => None,
        }
    }

    fn variable(&self) -> Option<VarName<MownStr<'_>>> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).variable(),
            AnyPattern::Named(nnp) => Ox2So::new_ref(*nnp).variable(),
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(*tp).triple(),
            AnyPattern::Named(_) => None,
        }
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        match self {
            AnyPattern::Term(tp) => Ox2So::new_ref(tp).triple(),
            AnyPattern::Named(_) => None,
        }
    }
}
