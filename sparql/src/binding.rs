#![allow(clippy::module_name_repetitions)]

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::hash_map::Entry::Occupied;
use std::collections::hash_map::Entry::Vacant;
use std::sync::Arc;
use std::sync::MutexGuard;

use sophia_api::prelude::*;
use sophia_api::sparql::SparqlBindings;
use sophia_api::term::SimpleTerm;
use sophia_api::term::VarName;
use sophia_term::ArcStrStash;
use sophia_term::ArcTerm;
use spargebra::term::NamedNodePattern;
use spargebra::term::TermPattern;
use spargebra::term::TriplePattern;

use crate::SparqlWrapper;
use crate::SparqlWrapperError;
use crate::matcher::AnyPattern;
use crate::stash::ArcStrStashExt;
use crate::term::ResultTerm;

pub struct Bindings<'a, D: Dataset + ?Sized> {
    pub(crate) variables: Vec<VarName<Arc<str>>>,
    pub(crate) iter: BindingsIter<'a, D>,
}

pub type BindingsIter<'a, D> =
    Box<dyn Iterator<Item = Result<Binding, SparqlWrapperError<<D as Dataset>::Error>>> + 'a>;

impl<'a, D: Dataset + ?Sized> Bindings<'a, D> {
    pub fn empty() -> Self {
        Self::empty_with(vec![])
    }

    pub fn empty_with(variables: Vec<VarName<Arc<str>>>) -> Self {
        let iter = Box::new(std::iter::empty());
        Self { variables, iter }
    }

    pub fn err<T: Into<SparqlWrapperError<D::Error>>>(err: T) -> Self {
        Self::err_with(err, vec![])
    }

    pub fn err_with<T: Into<SparqlWrapperError<D::Error>>>(
        err: T,
        variables: Vec<VarName<Arc<str>>>,
    ) -> Self {
        let iter = Box::new(std::iter::once(Err(err.into())));
        Self { variables, iter }
    }
}

impl<D: Dataset + ?Sized> Bindings<'_, D> {
    pub fn variables(&self) -> Vec<&str> {
        self.variables.iter().map(VarName::as_str).collect()
    }
}

impl<'a, D: Dataset + ?Sized> SparqlBindings<SparqlWrapper<'a, D>> for Bindings<'a, D> {
    fn variables(&self) -> Vec<&str> {
        Bindings::variables(self)
    }
}

impl<'a, D: Dataset + ?Sized> IntoIterator for Bindings<'a, D> {
    type Item = Result<Vec<Option<ResultTerm>>, SparqlWrapperError<D::Error>>;

    type IntoIter = Box<dyn Iterator<Item = Self::Item> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.iter.map(move |rb| {
            rb.map(|b| {
                self.variables
                    .iter()
                    .map(|v| b.v.get(v.as_str()).cloned())
                    .collect::<Vec<_>>()
            })
        }))
    }
}

//

#[derive(Clone, Debug, Default)]
pub struct Binding {
    /// Binding of variables
    pub v: BindingMap,
    /// Binding of blank nodes
    pub b: BindingMap,
}

impl Binding {
    /// Return whether self and other are compatible with non-disjoint domain,
    /// or None if their domain are disjoint.
    pub fn compatible(&self, other: &Binding) -> Option<bool> {
        other
            .v
            .iter()
            .filter_map(|(vn, vv)| self.v.get(vn).map(|ov| ov == vv))
            .reduce(|b1, b2| b1 && b2)
    }

    pub fn merge_if_compatible(mut self, context: Option<&Binding>) -> Option<Binding> {
        if let Some(context) = context {
            for (vn, vv) in &context.v {
                match self.v.entry(vn.clone()) {
                    Occupied(e) => {
                        if e.get() != vv {
                            return None;
                        }
                    }
                    Vacant(e) => {
                        e.insert(vv.clone());
                    }
                };
            }
        }
        Some(self)
    }

    pub fn project(mut self, variables: &[VarName<Arc<str>>]) -> Binding {
        self.v
            .retain(|k, _| variables.iter().any(|i| i.as_str() == k.as_ref()));
        self.b.clear();
        self
    }
}

//

pub type BindingMap = HashMap<Arc<str>, ResultTerm>;

pub fn populate_variables(
    patterns: &[TriplePattern],
    stash: &mut ArcStrStash,
    context: Option<&Binding>,
) -> Vec<VarName<Arc<str>>> {
    let mut variable_set = context
        .iter()
        .flat_map(|b| {
            b.v.keys()
                .map(|varname| VarName::new_unchecked(varname.clone()))
        })
        .collect::<HashSet<_>>();

    for tp in patterns {
        collect_variables(&tp.subject, &mut variable_set, stash);
        collect_variables(&tp.predicate, &mut variable_set, stash);
        collect_variables(&tp.object, &mut variable_set, stash);
    }
    variable_set.into_iter().collect()
}

pub(crate) fn populate_binding<'a, T: Term, P: Into<AnyPattern<'a>>>(
    pattern: P,
    result: T,
    b: &mut Binding,
    stash: &mut MutexGuard<ArcStrStash>,
) -> bool {
    let pattern = pattern.into();
    let st = pattern.as_simple();
    match st {
        SimpleTerm::BlankNode(bnid) => {
            let key = stash.copy_str(bnid);
            match b.b.entry(key) {
                Occupied(occupied) => Term::eq(occupied.get(), result),
                Vacant(vacant) => {
                    vacant.insert(stash.copy_result_term(result.borrow_term()));
                    true
                }
            }
        }
        SimpleTerm::Variable(var) => {
            let key = stash.copy_str(var);
            match b.v.entry(key) {
                Occupied(occupied) => Term::eq(occupied.get(), result),
                Vacant(vacant) => {
                    vacant.insert(stash.copy_result_term(result.borrow_term()));
                    true
                }
            }
        }
        SimpleTerm::Triple(_) => {
            let AnyPattern::Term(TermPattern::Triple(triple_pattern)) = pattern else {
                unreachable!();
            };
            debug_assert!(result.is_triple());
            let result = result.triple().unwrap();
            populate_binding(&triple_pattern.subject, result.s(), b, stash)
                && populate_binding(&triple_pattern.predicate, result.p(), b, stash)
                && populate_binding(&triple_pattern.object, result.o(), b, stash)
        }
        _ => {
            debug_assert!(Term::eq(&pattern, result.borrow_term()));
            true
        }
    }
}

pub(crate) fn populate_binding_arcterm(
    pattern: AnyPattern,
    t: ArcTerm,
    b: &mut Binding,
    stash: &mut MutexGuard<ArcStrStash>,
) -> bool {
    let st = pattern.as_simple();
    match st {
        SimpleTerm::BlankNode(bnid) => {
            let key = stash.copy_str(bnid);
            match b.b.entry(key) {
                Occupied(occupied) => Term::eq(occupied.get(), t),
                Vacant(vacant) => {
                    vacant.insert(t.into());
                    true
                }
            }
        }
        SimpleTerm::Variable(var) => {
            let key = stash.copy_str(var);
            match b.v.entry(key) {
                Occupied(occupied) => Term::eq(occupied.get(), t),
                Vacant(vacant) => {
                    vacant.insert(t.into());
                    true
                }
            }
        }
        SimpleTerm::Triple(_) => {
            let AnyPattern::Term(TermPattern::Triple(triple_pattern)) = pattern else {
                unreachable!();
            };
            debug_assert!(t.is_triple());
            let [s, p, o] = t.to_triple().unwrap();
            populate_binding_arcterm((&triple_pattern.subject).into(), s, b, stash)
                && populate_binding_arcterm((&triple_pattern.predicate).into(), p, b, stash)
                && populate_binding_arcterm((&triple_pattern.object).into(), o, b, stash)
        }
        _ => {
            debug_assert!(Term::eq(&pattern, &t));
            true
        }
    }
}

pub(crate) fn collect_variables<'a, T>(
    pattern: &'a T,
    set: &mut HashSet<VarName<Arc<str>>>,
    stash: &mut ArcStrStash,
) where
    AnyPattern<'a>: From<&'a T>,
{
    set.extend(AnyPattern::from(pattern).atoms().filter_map(|i| match i {
        AnyPattern::Term(TermPattern::Variable(v))
        | AnyPattern::Named(NamedNodePattern::Variable(v)) => Some(stash.copy_variable(v)),
        _ => None,
    }));
}
