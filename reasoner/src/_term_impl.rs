use sophia_api::{
    MownStr,
    ns::rdf,
    term::{BaseDirection, BnodeId, IriRef, LanguageTag, Term, TermKind, VarName},
};

use crate::{InternalTerm, ReasonableGraph, ReasonableTerm};

impl<'a, D, R> Copy for ReasonableTerm<'a, D, R> {}

impl<'a, D, R> Clone for ReasonableTerm<'a, D, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, D, R> std::fmt::Debug for ReasonableTerm<'a, D, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReasonableTerm")
            .field("graph", &self.graph)
            .field("index", &self.index)
            .finish()
    }
}

impl<'a, D, R> ReasonableTerm<'a, D, R> {
    pub(crate) fn new(graph: &'a ReasonableGraph<D, R>, index: usize) -> Self {
        ReasonableTerm { graph, index }
    }

    fn internal(&self) -> &InternalTerm {
        &self.graph.i2t[self.index]
    }
}

impl<D, R> Term for ReasonableTerm<'_, D, R> {
    type BorrowTerm<'x>
        = ReasonableTerm<'x, D, R>
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        match self.internal() {
            InternalTerm::Iri(_) => TermKind::Iri,
            InternalTerm::BlankNode(_) => TermKind::BlankNode,
            InternalTerm::TypedLiteral(..) | InternalTerm::LangString(..) => TermKind::Literal,
            InternalTerm::TripleTerm(_) => TermKind::Triple,
            InternalTerm::Variable(_) => TermKind::Variable,
        }
    }

    fn iri(&self) -> Option<IriRef<MownStr<'_>>> {
        if let InternalTerm::Iri(i) = &self.internal() {
            Some(i.as_ref().map_unchecked(MownStr::from_ref))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<MownStr<'_>>> {
        if let InternalTerm::BlankNode(b) = self.internal() {
            Some(b.as_ref().map_unchecked(MownStr::from_ref))
        } else {
            None
        }
    }

    fn lexical_form(&self) -> Option<MownStr<'_>> {
        if let InternalTerm::TypedLiteral(lex, _) | InternalTerm::LangString(lex, ..) =
            self.internal()
        {
            Some(MownStr::from_ref(lex))
        } else {
            None
        }
    }

    fn datatype(&self) -> Option<IriRef<MownStr<'_>>> {
        match self.internal() {
            InternalTerm::TypedLiteral(_, dti) => {
                let InternalTerm::Iri(i) = &*self.graph.i2t[*dti] else {
                    unreachable!();
                };
                Some(i.as_ref().map_unchecked(MownStr::from_ref))
            }
            InternalTerm::LangString(_, _, None) => {
                let ret = rdf::langString.iri();
                debug_assert!(ret.is_some());
                ret
            }
            InternalTerm::LangString(_, _, Some(_)) => {
                let ret = rdf::dirLangString.iri();
                debug_assert!(ret.is_some());
                ret
            }
            _ => None,
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<MownStr<'_>>> {
        if let InternalTerm::LangString(_, tag, _) = self.internal() {
            Some(tag.as_ref().map_unchecked(MownStr::from_ref))
        } else {
            None
        }
    }

    fn base_direction(&self) -> Option<BaseDirection> {
        if let InternalTerm::LangString(.., base_dir) = self.internal() {
            *base_dir
        } else {
            None
        }
    }

    fn variable(&self) -> Option<VarName<MownStr<'_>>> {
        if let InternalTerm::Variable(v) = self.internal() {
            Some(v.as_ref().map_unchecked(MownStr::from_ref))
        } else {
            None
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        self.to_triple()
    }

    fn to_triple(self) -> Option<[Self; 3]> {
        if let InternalTerm::TripleTerm(spo) = self.internal() {
            let graph = self.graph;
            Some(spo.map(|index| Self { index, graph }))
        } else {
            None
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }
}

impl<'a, D, R, T: Term> PartialEq<T> for ReasonableTerm<'a, D, R> {
    fn eq(&self, other: &T) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl<'a, D, R> Eq for ReasonableTerm<'a, D, R> {}

impl<'a, D, R, T: Term> PartialOrd<T> for ReasonableTerm<'a, D, R> {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        Some(Term::cmp(self, other.borrow_term()))
    }
}

impl<'a, D, R> Ord for ReasonableTerm<'a, D, R> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Term::cmp(self, *other)
    }
}

impl<'a, D, R> std::hash::Hash for ReasonableTerm<'a, D, R> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state)
    }
}
