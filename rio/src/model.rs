//! Implement Sophia traits for types defined in Rio.
//!
//! NB: since [`rio_api::model`] types have public fields,
//! they can not in general be trusted to contain valid data
//! (e.g. a valid IRI in [`NamedNode`]).
//!
//! However, their typical use-case is to be produced by a parser,
//! which ensures the validity of the underlying data.
//!
//! The [`Trusted`] wrapper is used to materialize the fact that we trust the underlying data of Rio types.
use rio_api::model::{Quad as RioQuad, Term as RioTerm, Triple as RioTriple, *};
use sophia_api::ns::{rdf, xsd};
use sophia_api::quad::{QBorrowTerm, Quad, Spog};
use sophia_api::term::{BnodeId, LanguageTag, Term, TermKind, VarName};
use sophia_api::triple::{TBorrowTerm, Triple};
use sophia_api::MownStr;
use sophia_iri::{Iri, IriRef};

impl<'a> Term for Trusted<BlankNode<'a>> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::BlankNode
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        Some(bnode_id(self.0))
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

fn bnode_id(b: BlankNode) -> BnodeId<MownStr> {
    debug_assert!(BnodeId::new(b.id).is_ok());
    BnodeId::new_unchecked(b.id.into())
}

impl<'a> Term for Trusted<NamedNode<'a>> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Iri
    }

    fn iri(&self) -> Option<sophia_iri::IriRef<MownStr<'_>>> {
        Some(iri(self.0))
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

fn iri(n: NamedNode) -> IriRef<MownStr> {
    debug_assert!(Iri::new(n.iri).is_ok());
    IriRef::new_unchecked(n.iri.into())
}

impl<'a> Term for Trusted<Variable<'a>> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Variable
    }

    fn variable(&self) -> Option<VarName<MownStr>> {
        Some(variable(self.0))
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

fn variable(v: Variable) -> VarName<MownStr> {
    debug_assert!(VarName::new(v.name).is_ok());
    VarName::new_unchecked(v.name.into())
}

impl<'a> Term for Trusted<Literal<'a>> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        TermKind::Literal
    }

    fn lexical_value(&self) -> Option<MownStr> {
        Some(lexical_value(self.0))
    }

    fn datatype(&self) -> Option<IriRef<MownStr>> {
        Some(datatype(self.0))
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        language_tag(self.0)
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

fn lexical_value(l: Literal) -> MownStr {
    use Literal::*;
    let value = match l {
        Simple { value } => value,
        LanguageTaggedString { value, .. } => value,
        Typed { value, .. } => value,
    };
    value.into()
}

fn datatype(l: Literal) -> IriRef<MownStr> {
    use Literal::*;
    let dt = match l {
        Simple { .. } => xsd::string.iriref(),
        LanguageTaggedString { .. } => rdf::langString.iriref(),
        Typed { datatype, .. } => {
            debug_assert!(Iri::new(datatype.iri).is_ok());
            IriRef::new_unchecked(datatype.iri.into())
        }
    };
    dt
}

fn language_tag(l: Literal) -> Option<LanguageTag<MownStr>> {
    if let Literal::LanguageTaggedString { language, .. } = l {
        debug_assert!(LanguageTag::new(language).is_ok());
        Some(LanguageTag::new_unchecked(language.into()))
    } else {
        None
    }
}

/*  // NB: rio::Subject can not fully implement sophia::Term,
    // because the subterms of embedded triples might be literals

    // However, this is not required for interfacing with Sophia;
    // we cheat by forcing RioTriple to expose its subject as a RioTerm.

impl<'a> Term for Trusted<Subject<'a>> {
    type BorrowTerm<'x>
    where
        Self: 'x,
    = Trusted<RioTerm<'a>>;

    fn kind(&self) -> TermKind {
        use Subject::*;
        match self.0 {
            NamedNode(_) => TermKind::Iri,
            BlankNode(_) => TermKind::BlankNode,
            Triple(_) => TermKind::Triple,
        }
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        if let Subject::NamedNode(n) = self.0 {
            Some(iri(n))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        if let Subject::BlankNode(b) = self.0 {
            Some(bnode_id(b))
        } else {
            None
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        if let Subject::Triple(t) = self.0 {
            let s = Trusted(t.subject.into());
            let p = Trusted(t.predicate.into());
            let o = Trusted(t.object);
            Some([s, p, o])
        } else {
            None
        }
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        if let Subject::Triple(_) = self.0 {
            // there is an impedence mismatch between Sophia and Rio:
            // we can not convert a triple into [Subject; 3]
            unimplemented!()
        } else {
            None
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        Trusted(self.0.into())
    }
}
*/

impl<'a> Term for Trusted<GraphName<'a>> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        use GraphName::*;
        match self.0 {
            NamedNode(_) => TermKind::Iri,
            BlankNode(_) => TermKind::BlankNode,
        }
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        if let GraphName::NamedNode(n) = self.0 {
            Some(iri(n))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        if let GraphName::BlankNode(b) = self.0 {
            Some(bnode_id(b))
        } else {
            None
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        Trusted(self.0)
    }
}

impl<'a> Term for Trusted<RioTerm<'a>> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        use RioTerm::*;
        match self.0 {
            NamedNode(_) => TermKind::Iri,
            BlankNode(_) => TermKind::BlankNode,
            Literal(_) => TermKind::Literal,
            Triple(_) => TermKind::Triple,
        }
    }

    fn iri(&self) -> Option<IriRef<MownStr>> {
        if let RioTerm::NamedNode(n) = self.0 {
            Some(iri(n))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        if let RioTerm::BlankNode(b) = self.0 {
            Some(bnode_id(b))
        } else {
            None
        }
    }

    fn lexical_value(&self) -> Option<MownStr> {
        if let RioTerm::Literal(l) = self.0 {
            Some(lexical_value(l))
        } else {
            None
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr>> {
        if let RioTerm::Literal(l) = self.0 {
            Some(datatype(l))
        } else {
            None
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        if let RioTerm::Literal(l) = self.0 {
            language_tag(l)
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
        if let RioTerm::Triple(t) = self.0 {
            let s = Trusted(t.subject.into());
            let p = Trusted(t.predicate.into());
            let o = Trusted(t.object);
            Some([s, p, o])
        } else {
            None
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

impl<'a> Term for Trusted<GeneralizedTerm<'a>> {
    type BorrowTerm<'x> = Self where Self: 'x;

    fn kind(&self) -> TermKind {
        use GeneralizedTerm::*;
        match self.0 {
            NamedNode(_) => TermKind::Iri,
            BlankNode(_) => TermKind::BlankNode,
            Literal(_) => TermKind::Literal,
            Triple(_) => TermKind::Triple,
            Variable(_) => TermKind::Variable,
        }
    }

    fn iri(&self) -> Option<IriRef<MownStr>> {
        if let GeneralizedTerm::NamedNode(n) = self.0 {
            Some(iri(n))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr>> {
        if let GeneralizedTerm::BlankNode(b) = self.0 {
            Some(bnode_id(b))
        } else {
            None
        }
    }

    fn lexical_value(&self) -> Option<MownStr> {
        if let GeneralizedTerm::Literal(l) = self.0 {
            Some(lexical_value(l))
        } else {
            None
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr>> {
        if let GeneralizedTerm::Literal(l) = self.0 {
            Some(datatype(l))
        } else {
            None
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr>> {
        if let GeneralizedTerm::Literal(l) = self.0 {
            language_tag(l)
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
        if let GeneralizedTerm::Triple(t) = self.0 {
            let s = Trusted(t[0]);
            let p = Trusted(t[1]);
            let o = Trusted(t[2]);
            Some([s, p, o])
        } else {
            None
        }
    }

    fn variable(&self) -> Option<VarName<MownStr>> {
        if let GeneralizedTerm::Variable(v) = self.0 {
            Some(variable(v))
        } else {
            None
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

impl<'a> Triple for Trusted<RioTriple<'a>> {
    type Term = Trusted<RioTerm<'a>>;

    fn s(&self) -> TBorrowTerm<Self> {
        Trusted(self.subject.into())
    }

    fn p(&self) -> TBorrowTerm<Self> {
        Trusted(self.predicate.into())
    }

    fn o(&self) -> TBorrowTerm<Self> {
        Trusted(self.object)
    }

    fn to_s(self) -> Self::Term {
        Trusted(self.subject.into())
    }

    fn to_p(self) -> Self::Term {
        Trusted(self.predicate.into())
    }

    fn to_o(self) -> Self::Term {
        Trusted(self.object)
    }

    fn to_spo(self) -> [Self::Term; 3] {
        [self.to_s(), self.to_p(), self.to_o()]
    }
}

impl<'a> Quad for Trusted<RioQuad<'a>> {
    type Term = Trusted<RioTerm<'a>>;

    fn s(&self) -> QBorrowTerm<Self> {
        Trusted(self.subject.into())
    }

    fn p(&self) -> QBorrowTerm<Self> {
        Trusted(self.predicate.into())
    }

    fn o(&self) -> QBorrowTerm<Self> {
        Trusted(self.object)
    }

    fn g(&self) -> Option<QBorrowTerm<Self>> {
        self.graph_name.map(|g| Trusted(g.into()))
    }

    fn to_s(self) -> Self::Term {
        Trusted(self.subject.into())
    }

    fn to_p(self) -> Self::Term {
        Trusted(self.predicate.into())
    }

    fn to_o(self) -> Self::Term {
        Trusted(self.object)
    }

    fn to_g(self) -> Option<Self::Term> {
        self.graph_name.map(|g| Trusted(g.into()))
    }

    fn to_spog(self) -> Spog<Self::Term> {
        ([self.to_s(), self.to_p(), self.to_o()], self.to_g())
    }
}

impl<'a> Quad for Trusted<GeneralizedQuad<'a>> {
    type Term = Trusted<GeneralizedTerm<'a>>;

    fn s(&self) -> QBorrowTerm<Self> {
        Trusted(self.subject)
    }

    fn p(&self) -> QBorrowTerm<Self> {
        Trusted(self.predicate)
    }

    fn o(&self) -> QBorrowTerm<Self> {
        Trusted(self.object)
    }

    fn g(&self) -> Option<QBorrowTerm<Self>> {
        self.graph_name.map(Trusted)
    }

    fn to_s(self) -> Self::Term {
        Trusted(self.subject)
    }

    fn to_p(self) -> Self::Term {
        Trusted(self.predicate)
    }

    fn to_o(self) -> Self::Term {
        Trusted(self.object)
    }

    fn to_g(self) -> Option<Self::Term> {
        self.graph_name.map(Trusted)
    }

    fn to_spog(self) -> Spog<Self::Term> {
        ([self.s(), self.p(), self.o()], self.g())
    }
}

/// A wrapper for Rio types that are trusted to contain valid data
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
    use sophia_api::term::test_term_impl;

    #[test]
    fn blank_node() {
        test_term_impl(&Trusted(BlankNode { id: "foo" }))
    }

    #[test]
    fn named_node() {
        test_term_impl(&Trusted(NamedNode { iri: "tag:foo" }));
    }

    #[test]
    fn variable() {
        test_term_impl(&Trusted(Variable { name: "foo" }));
    }

    #[test]
    fn literal_simple() {
        test_term_impl(&Trusted(Literal::Simple { value: "foo" }));
    }

    #[test]
    fn literal_language() {
        test_term_impl(&Trusted(Literal::LanguageTaggedString {
            value: "foo",
            language: "en",
        }));
    }

    #[test]
    fn literal_typed() {
        let datatype = xsd::integer.iriref().to_string();
        let datatype = NamedNode { iri: &datatype };
        test_term_impl(&Trusted(Literal::Typed {
            value: "42",
            datatype,
        }));
    }

    /*
    #[test]
    fn subject_blank_node() {
        let t: Subject = BlankNode { id: "foo" }.into();
        test_term_impl(&Trusted(t))
    }

    #[test]
    fn subject_named_node() {
        let t: Subject = NamedNode { iri: "tag:foo" }.into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn subject_triple() {
        let subject = BlankNode { id: "foo" }.into();
        let predicate = NamedNode { iri: "tag:bar" };
        let object = Literal::Simple { value: "baz" }.into();
        let tr = RioTriple {
            subject,
            predicate,
            object,
        };
        let t = Trusted(Subject::Triple(&tr));
        assert_eq!(t.kind(), TermKind::Triple);
        assert_eq!(t.iri(), None);
        assert!(t.triple().is_some());
    }
     */

    #[test]
    fn graph_name_blank_node() {
        let t: GraphName = BlankNode { id: "foo" }.into();
        test_term_impl(&Trusted(t))
    }

    #[test]
    fn graph_name_named_node() {
        let t: GraphName = NamedNode { iri: "tag:foo" }.into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn term_blank_node() {
        let t: RioTerm = BlankNode { id: "foo" }.into();
        test_term_impl(&Trusted(t))
    }

    #[test]
    fn term_named_node() {
        let t: RioTerm = NamedNode { iri: "tag:foo" }.into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn term_literal_simple() {
        let t: RioTerm = Literal::Simple { value: "foo" }.into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn term_literal_language() {
        let t: RioTerm = Literal::LanguageTaggedString {
            value: "foo",
            language: "en",
        }
        .into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn term_literal_typed() {
        let datatype = xsd::integer.iriref().to_string();
        let datatype = NamedNode { iri: &datatype };
        let t: RioTerm = Literal::Typed {
            value: "42",
            datatype,
        }
        .into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn term_triple() {
        let subject = BlankNode { id: "foo" }.into();
        let predicate = NamedNode { iri: "tag:bar" };
        let object = Literal::Simple { value: "baz" }.into();
        let tr = RioTriple {
            subject,
            predicate,
            object,
        };
        let t = RioTerm::Triple(&tr);
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn gterm_blank_node() {
        let t: RioTerm = BlankNode { id: "foo" }.into();
        test_term_impl(&Trusted(t))
    }

    #[test]
    fn gterm_named_node() {
        let t: RioTerm = NamedNode { iri: "tag:foo" }.into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn gterm_literal_simple() {
        let t: RioTerm = Literal::Simple { value: "foo" }.into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn gterm_literal_language() {
        let t: RioTerm = Literal::LanguageTaggedString {
            value: "foo",
            language: "en",
        }
        .into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn gterm_literal_typed() {
        let datatype = xsd::integer.iriref().to_string();
        let datatype = NamedNode { iri: &datatype };
        let t: RioTerm = Literal::Typed {
            value: "42",
            datatype,
        }
        .into();
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn gterm_triple() {
        let subject = BlankNode { id: "foo" }.into();
        let predicate = NamedNode { iri: "tag:bar" };
        let object = Literal::Simple { value: "baz" }.into();
        let tr = RioTriple {
            subject,
            predicate,
            object,
        };
        let t = RioTerm::Triple(&tr);
        test_term_impl(&Trusted(t));
    }

    #[test]
    fn gterm_variable() {
        let t: GeneralizedTerm = Variable { name: "foo" }.into();
        test_term_impl(&Trusted(t))
    }
}
