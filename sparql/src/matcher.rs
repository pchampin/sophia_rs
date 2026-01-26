#![allow(clippy::module_name_repetitions)]

use std::sync::MutexGuard;

use sophia_api::{
    MownStr,
    term::{
        BnodeId, IriRef, LanguageTag, SimpleTerm, Term, TermKind, VarName, matcher::TermMatcher,
    },
};
use sophia_term::ArcStrStash;
use spargebra::term::{NamedNodePattern, TermPattern, TriplePattern};

use crate::stash::ArcStrStashExt;
use crate::{binding::Binding, term::ResultTerm};

mod _any_pattern;
pub(crate) use _any_pattern::*;
mod _ox2so;
pub(crate) use _ox2so::*;

#[derive(Clone, Debug)]
pub enum SparqlMatcher {
    /// Free variable or bnode
    Free, // free variable or bnode
    /// Non-ground triple
    /// NB: for ground triples, the `Bound` variant should be used instead
    Triple(Box<[SparqlMatcher; 3]>),
    /// Ground term
    Bound(ResultTerm),
}

impl SparqlMatcher {
    pub fn build(pattern: AnyPattern, stash: &mut MutexGuard<'_, ArcStrStash>) -> Self {
        use SparqlMatcher::*;
        match pattern.as_simple() {
            SimpleTerm::BlankNode(_) | SimpleTerm::Variable(_) => Free,
            SimpleTerm::Triple(_) => {
                // the following is safe because we know we have a triple
                let tr = unsafe { pattern.to_triple().unwrap_unchecked() };
                match tr.map(move |t| SparqlMatcher::build(t, stash)) {
                    [Bound(s), Bound(p), Bound(o)] => Bound([s, p, o].into()),
                    spo => Triple(Box::new(spo)),
                }
            }
            _ => Bound(stash.copy_result_term(pattern)),
        }
    }

    pub fn build_with<'a, T: Into<AnyPattern<'a>>>(
        pattern: T,
        bindings: &Binding,
        stash: &mut MutexGuard<'_, ArcStrStash>,
    ) -> Self {
        use SparqlMatcher::*;
        let pattern = pattern.into();
        match pattern.as_simple() {
            SimpleTerm::BlankNode(bnid) => {
                if let Some(b) = bindings.b.get(bnid.as_str()) {
                    Bound(b.clone())
                } else {
                    Free
                }
            }
            SimpleTerm::Triple(_) => {
                // the following is safe because we know we have a triple
                let tr = unsafe { pattern.to_triple().unwrap_unchecked() };
                match tr.map(move |t| SparqlMatcher::build_with(t, bindings, stash)) {
                    [Bound(s), Bound(p), Bound(o)] => Bound([s, p, o].into()),
                    spo => Triple(Box::new(spo)),
                }
            }
            SimpleTerm::Variable(vn) => {
                if let Some(b) = bindings.v.get(vn.as_str()) {
                    Bound(b.clone())
                } else {
                    Free
                }
            }
            _ => Bound(stash.copy_result_term(pattern)),
        }
    }

    pub fn is_bound(&self) -> bool {
        matches!(self, SparqlMatcher::Bound(_))
    }

    pub fn build3(pattern: &TriplePattern, stash: &mut MutexGuard<'_, ArcStrStash>) -> [Self; 3] {
        [
            AnyPattern::from(&pattern.subject),
            AnyPattern::from(&pattern.predicate),
            AnyPattern::from(&pattern.object),
        ]
        .map(|p| Self::build(p, stash))
    }

    pub fn bound_or_else<F>(&self, f: F) -> Self
    where
        F: FnOnce() -> Self,
    {
        if self.is_bound() { self.clone() } else { f() }
    }
}

impl TermMatcher for SparqlMatcher {
    type Term = ResultTerm;

    fn matches<T2: Term + ?Sized>(&self, term: &T2) -> bool {
        match self {
            SparqlMatcher::Bound(t) => Term::eq(t.borrow_term(), term.borrow_term()),
            SparqlMatcher::Triple(tr) => (
                tr[0].matcher_ref(),
                tr[1].matcher_ref(),
                tr[2].matcher_ref(),
            )
                .matches(term),
            _ => true,
        }
    }

    fn constant(&self) -> Option<&Self::Term> {
        match self {
            SparqlMatcher::Bound(t) => Some(t),
            _ => None,
        }
    }
}
