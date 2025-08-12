use sophia_api::term::BaseDirection;

use super::*;

#[repr(transparent)]
#[derive(Clone, Debug)]
/// `OxRdf` to Sophia adapter
pub struct Ox2So<T>(pub T);

impl<T> Ox2So<T> {
    pub fn new_ref(t: &T) -> &Self {
        // we cast &T to *const T to *const Ox2So<T>
        let p = std::ptr::from_ref(t).cast::<Self>();
        unsafe {
            // this is safe because
            // p points to a valid T, and because the layout of Ox2So<T> is transparent
            &*p
        }
    }
}

impl Term for Ox2So<TermPattern> {
    type BorrowTerm<'x>
        = AnyPattern<'x>
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        match self.0 {
            TermPattern::NamedNode(_) => TermKind::Iri,
            TermPattern::BlankNode(_) => TermKind::BlankNode,
            TermPattern::Literal(_) => TermKind::Literal,
            TermPattern::Triple(_) => TermKind::Triple,
            TermPattern::Variable(_) => TermKind::Variable,
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        AnyPattern::Term(&self.0)
    }

    fn is_iri(&self) -> bool {
        matches!(self.0, TermPattern::NamedNode(_))
    }

    fn is_blank_node(&self) -> bool {
        matches!(self.0, TermPattern::BlankNode(_))
    }

    fn is_literal(&self) -> bool {
        matches!(self.0, TermPattern::Literal(_))
    }

    fn is_variable(&self) -> bool {
        matches!(self.0, TermPattern::Variable(_))
    }

    fn is_triple(&self) -> bool {
        matches!(self.0, TermPattern::Triple(_))
    }

    fn iri(&self) -> Option<IriRef<MownStr>> {
        match &self.0 {
            TermPattern::NamedNode(nn) => Some(IriRef::new_unchecked(nn.as_str().into())),
            _ => None,
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        match &self.0 {
            TermPattern::BlankNode(bn) => Some(BnodeId::new_unchecked(bn.as_str().into())),
            _ => None,
        }
    }

    fn lexical_form(&self) -> Option<MownStr> {
        match &self.0 {
            TermPattern::Literal(lit) => Some(lit.value().into()),
            _ => None,
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr>> {
        match &self.0 {
            TermPattern::Literal(lit) => {
                Some(IriRef::new_unchecked(lit.datatype().as_str().into()))
            }
            _ => None,
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        match &self.0 {
            TermPattern::Literal(lit) => lit
                .language()
                .map(|tag| LanguageTag::new_unchecked(tag.into())),
            _ => None,
        }
    }

    fn base_direction(&self) -> Option<BaseDirection> {
        match &self.0 {
            TermPattern::Literal(lit) => lit.direction().map(|dir| match dir {
                oxrdf::BaseDirection::Ltr => BaseDirection::Ltr,
                oxrdf::BaseDirection::Rtl => BaseDirection::Rtl,
            }),
            _ => None,
        }
    }

    fn variable(&self) -> Option<VarName<MownStr>> {
        match &self.0 {
            TermPattern::Variable(v) => Some(VarName::new_unchecked(v.as_str().into())),
            _ => None,
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        match &self.0 {
            TermPattern::Triple(t) => Some([
                AnyPattern::Term(&t.subject),
                AnyPattern::Named(&t.predicate),
                AnyPattern::Term(&t.object),
            ]),
            _ => None,
        }
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        match self.0 {
            TermPattern::Triple(t) => {
                Some([Ox2So(t.subject), Ox2So(t.predicate.into()), Ox2So(t.object)])
            }
            _ => None,
        }
    }
}

impl Term for Ox2So<NamedNodePattern> {
    type BorrowTerm<'x>
        = AnyPattern<'x>
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        match self.0 {
            NamedNodePattern::NamedNode(_) => TermKind::Iri,
            NamedNodePattern::Variable(_) => TermKind::Variable,
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        AnyPattern::Named(&self.0)
    }

    fn is_iri(&self) -> bool {
        matches!(self.0, NamedNodePattern::NamedNode(_))
    }

    fn is_blank_node(&self) -> bool {
        false
    }

    fn is_literal(&self) -> bool {
        false
    }

    fn is_variable(&self) -> bool {
        matches!(self.0, NamedNodePattern::Variable(_))
    }

    fn is_atom(&self) -> bool {
        true
    }

    fn is_triple(&self) -> bool {
        false
    }

    fn iri(&self) -> Option<IriRef<MownStr>> {
        match &self.0 {
            NamedNodePattern::NamedNode(nn) => Some(IriRef::new_unchecked(nn.as_str().into())),
            NamedNodePattern::Variable(_) => None,
        }
    }

    fn variable(&self) -> Option<VarName<MownStr>> {
        match &self.0 {
            NamedNodePattern::Variable(v) => Some(VarName::new_unchecked(v.as_str().into())),
            NamedNodePattern::NamedNode(_) => None,
        }
    }
}
