//! Implement Sophia traits for types defined in oxrdf.
//!
//! Similar to `sophia_rio::model`, this module wraps oxrdf types
//! to implement Sophia's Term and Triple traits.
//!
//! The [`Trusted`] wrapper is used to materialize the fact that we trust
//! the underlying data of oxrdf types (e.g., from a parser).

use oxrdf::{
    BlankNodeRef, LiteralRef, NamedNodeRef, NamedOrBlankNodeRef, TermRef, Triple as OxTriple,
};
use sophia_api::MownStr;
use sophia_api::term::{BaseDirection, BnodeId, IriRef, LanguageTag, Term, TermKind};
use sophia_api::triple::Triple;

impl<'a> Term for Trusted<NamedNodeRef<'a>> {
    type BorrowTerm<'x>
        = Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        Some(iri(self.0))
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

fn iri(n: NamedNodeRef) -> IriRef<MownStr> {
    debug_assert!(IriRef::new(n.as_str()).is_ok());
    IriRef::new_unchecked(n.as_str().into())
}

impl<'a> Term for Trusted<BlankNodeRef<'a>> {
    type BorrowTerm<'x>
        = Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::BlankNode
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        Some(bnode_id(self.0))
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

fn bnode_id(b: BlankNodeRef) -> BnodeId<MownStr> {
    debug_assert!(BnodeId::new(b.as_str()).is_ok());
    BnodeId::new_unchecked(b.as_str().into())
}

impl<'a> Term for Trusted<LiteralRef<'a>> {
    type BorrowTerm<'x>
        = Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }

    fn lexical_form(&self) -> Option<MownStr<'_>> {
        Some(self.0.value().into())
    }

    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        Some(datatype(self.0))
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr<'_>>> {
        language_tag(self.0)
    }

    fn base_direction(&self) -> Option<BaseDirection> {
        base_direction(self.0)
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

fn datatype(l: LiteralRef) -> IriRef<MownStr> {
    let dt = l.datatype();
    debug_assert!(IriRef::new(dt.as_str()).is_ok());
    IriRef::new_unchecked(dt.as_str().into())
}

fn language_tag(l: LiteralRef) -> Option<LanguageTag<MownStr>> {
    l.language().map(|lang| {
        debug_assert!(LanguageTag::new(lang).is_ok());
        LanguageTag::new_unchecked(lang.into())
    })
}

fn base_direction(l: LiteralRef) -> Option<BaseDirection> {
    l.direction().map(|dir| match dir {
        oxrdf::BaseDirection::Ltr => BaseDirection::Ltr,
        oxrdf::BaseDirection::Rtl => BaseDirection::Rtl,
    })
}

impl<'a> Term for Trusted<NamedOrBlankNodeRef<'a>> {
    type BorrowTerm<'x>
        = Trusted<TermRef<'a>>
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        use oxrdf::NamedOrBlankNodeRef::*;
        match self.0 {
            NamedNode(_) => TermKind::Iri,
            BlankNode(_) => TermKind::BlankNode,
        }
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        if let oxrdf::NamedOrBlankNodeRef::NamedNode(n) = self.0 {
            Some(iri(n))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        if let oxrdf::NamedOrBlankNodeRef::BlankNode(b) = self.0 {
            Some(bnode_id(b))
        } else {
            None
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        Trusted(self.0.into())
    }
}

impl<'a> Term for Trusted<TermRef<'a>> {
    type BorrowTerm<'x>
        = Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        use oxrdf::TermRef::*;
        match self.0 {
            NamedNode(_) => TermKind::Iri,
            BlankNode(_) => TermKind::BlankNode,
            Literal(_) => TermKind::Literal,
            Triple(_) => TermKind::Triple,
        }
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        if let oxrdf::TermRef::NamedNode(n) = self.0 {
            Some(iri(n))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        if let oxrdf::TermRef::BlankNode(b) = self.0 {
            Some(bnode_id(b))
        } else {
            None
        }
    }

    fn lexical_form(&self) -> Option<MownStr<'_>> {
        if let oxrdf::TermRef::Literal(l) = self.0 {
            Some(l.value().into())
        } else {
            None
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        if let oxrdf::TermRef::Literal(l) = self.0 {
            Some(datatype(l))
        } else {
            None
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr<'_>>> {
        if let oxrdf::TermRef::Literal(l) = self.0 {
            language_tag(l)
        } else {
            None
        }
    }

    fn base_direction(&self) -> Option<BaseDirection> {
        if let oxrdf::TermRef::Literal(l) = self.0 {
            base_direction(l)
        } else {
            None
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        self.to_triple()
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        if let oxrdf::TermRef::Triple(t) = self.0 {
            let s = Trusted(t.subject.as_ref().into());
            // let p = Trusted(TermRef::NamedNode(t.predicate.as_ref()));
            let p = Trusted(t.predicate.as_ref().into());
            let o = Trusted(t.object.as_ref());
            Some([s, p, o])
        } else {
            None
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

impl<'a> Triple for Trusted<&'a OxTriple> {
    type Term = Trusted<TermRef<'a>>;
    type BorrowTerm<'x>
        = Trusted<TermRef<'a>>
    where
        'a: 'x;

    fn s(&self) -> Self::BorrowTerm<'_> {
        Trusted(self.0.subject.as_ref().into())
    }

    fn p(&self) -> Self::BorrowTerm<'_> {
        Trusted(TermRef::NamedNode(self.0.predicate.as_ref()))
    }

    fn o(&self) -> Self::BorrowTerm<'_> {
        Trusted(self.0.object.as_ref())
    }

    fn to_s(self) -> Self::Term {
        Trusted(self.0.subject.as_ref().into())
    }

    fn to_p(self) -> Self::Term {
        Trusted(TermRef::NamedNode(self.0.predicate.as_ref()))
    }

    fn to_o(self) -> Self::Term {
        Trusted(self.0.object.as_ref())
    }

    fn to_spo(self) -> [Self::Term; 3] {
        [self.to_s(), self.to_p(), self.to_o()]
    }
}

/// A wrapper for oxrdf types that are trusted to contain valid data
#[derive(Clone, Copy, Debug)]
pub struct Trusted<T>(pub T);

impl<T> std::ops::Deref for Trusted<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use oxrdf::{BlankNode, Literal, NamedNode, NamedOrBlankNode as Subject, Term as OxTerm};
    use sophia_api::term::assert_consistent_term_impl;

    #[test]
    fn named_node() {
        let nn = NamedNode::new_unchecked("http://example.org/");
        assert_consistent_term_impl(&Trusted(nn.as_ref()));
    }

    #[test]
    fn blank_node() {
        let bn = BlankNode::new_unchecked("foo");
        assert_consistent_term_impl(&Trusted(bn.as_ref()));
    }

    #[test]
    fn literal_simple() {
        let lit = Literal::new_simple_literal("hello");
        assert_consistent_term_impl(&Trusted(lit.as_ref()));
    }

    #[test]
    fn literal_language() {
        let lit = Literal::new_language_tagged_literal_unchecked("hello", "en");
        assert_consistent_term_impl(&Trusted(lit.as_ref()));
    }

    #[test]
    fn literal_typed() {
        let dt = NamedNode::new_unchecked("http://www.w3.org/2001/XMLSchema#integer");
        let lit = Literal::new_typed_literal("42", dt);
        assert_consistent_term_impl(&Trusted(lit.as_ref()));
    }

    #[test]
    fn literal_with_direction() {
        let lit = Literal::new_directional_language_tagged_literal(
            "hello",
            "en",
            oxrdf::BaseDirection::Ltr,
        )
        .unwrap();
        assert_consistent_term_impl(&Trusted(lit.as_ref()));
        assert_eq!(
            Trusted(lit.as_ref()).base_direction(),
            Some(BaseDirection::Ltr)
        );
    }

    #[test]
    fn term_named_node() {
        let nn = NamedNode::new_unchecked("http://example.org/");
        let term = OxTerm::NamedNode(nn);
        assert_consistent_term_impl(&Trusted(term.as_ref()));
    }

    #[test]
    fn term_blank_node() {
        let bn = BlankNode::new_unchecked("foo");
        let term = OxTerm::BlankNode(bn);
        assert_consistent_term_impl(&Trusted(term.as_ref()));
    }

    #[test]
    fn term_literal() {
        let lit = Literal::new_simple_literal("hello");
        let term = OxTerm::Literal(lit);
        assert_consistent_term_impl(&Trusted(term.as_ref()));
    }

    #[test]
    fn term_triple() {
        let s = Subject::NamedNode(NamedNode::new_unchecked("http://example.org/s"));
        let p = NamedNode::new_unchecked("http://example.org/p");
        let o = OxTerm::Literal(Literal::new_simple_literal("o"));
        let triple = OxTriple::new(s, p, o);
        let term = OxTerm::Triple(Box::new(triple));
        assert_consistent_term_impl(&Trusted(term.as_ref()));
    }

    #[test]
    fn subject_named_node() {
        let nn = NamedNode::new_unchecked("http://example.org/");
        let subj = Subject::NamedNode(nn);
        assert_consistent_term_impl(&Trusted(subj.as_ref()));
    }

    #[test]
    fn subject_blank_node() {
        let bn = BlankNode::new_unchecked("foo");
        let subj = Subject::BlankNode(bn);
        assert_consistent_term_impl(&Trusted(subj.as_ref()));
    }

    #[test]
    fn triple() {
        let s = Subject::NamedNode(NamedNode::new_unchecked("http://example.org/s"));
        let p = NamedNode::new_unchecked("http://example.org/p");
        let o = OxTerm::Literal(Literal::new_simple_literal("o"));
        let triple = OxTriple::new(s, p, o);

        use sophia_api::triple::Triple;
        let wrapped = Trusted(&triple);
        assert!(wrapped.s().is_iri());
        assert!(wrapped.p().is_iri());
        assert!(wrapped.o().is_literal());
    }
}
