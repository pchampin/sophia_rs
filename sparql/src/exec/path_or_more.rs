//! Iterator used by property paths expression (ZeroOrMore and OneOrMore)

use std::{
    collections::{HashMap, hash_map::Entry},
    sync::Arc,
};

use sophia_api::{dataset::DResult, prelude::Dataset, term::matcher::TermMatcher};
use sophia_term::ArcTerm;
use spargebra::algebra::PropertyPathExpression;

use crate::{
    exec::{ExecState, GraphMatcher},
    matcher::SparqlMatcher,
};

pub struct PathOrMore<'a, D: Dataset + ?Sized> {
    state: Arc<ExecState<'a, D>>,
    subject: ArcTerm,
    current: Option<DResult<D, ArcTerm>>,
    iter_stack: Vec<Box<dyn Iterator<Item = DResult<D, [ArcTerm; 2]>> + 'a>>,
    seen: HashMap<ArcTerm, ()>,
    path: PropertyPathExpression,
    omatcher: SparqlMatcher,
    gmatcher: GraphMatcher,
}

impl<'a, D: Dataset + ?Sized> PathOrMore<'a, D> {
    pub fn new(
        state: Arc<ExecState<'a, D>>,
        subject: ArcTerm,
        first: ArcTerm,
        path: PropertyPathExpression,
        omatcher: SparqlMatcher,
        gmatcher: GraphMatcher,
    ) -> Self {
        let current = Some(Ok(first));
        let iter_stack = vec![];
        let seen = HashMap::new();
        Self {
            state,
            subject,
            current,
            iter_stack,
            seen,
            path,
            omatcher,
            gmatcher,
        }
    }

    fn populate_next_current(&mut self) {
        debug_assert!(self.current.is_none());
        let Some(iter) = self.iter_stack.last_mut() else {
            return;
        };
        self.current = iter.next().map(|res| res.map(|[_, o]| o));
        if self.current.is_none() {
            self.iter_stack.pop();
            self.populate_next_current();
        }
    }
}

impl<'a, D: Dataset + ?Sized> Iterator for PathOrMore<'a, D> {
    type Item = DResult<D, [ArcTerm; 2]>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current.take()?;
        let Ok(current) = current else {
            return Some(current.map(|_| unreachable!()));
        };
        let current = match self.seen.entry(current) {
            Entry::Occupied(_) => {
                self.populate_next_current();
                return self.next();
            }
            Entry::Vacant(e) => {
                let ret = e.key().clone();
                e.insert(());
                ret
            }
        };
        self.iter_stack.push(self.state.path_rec(
            SparqlMatcher::Bound(current.clone().into()),
            &self.path,
            SparqlMatcher::Free,
            &self.gmatcher,
        ));
        self.populate_next_current();
        if self.omatcher.matches(&current) {
            Some(Ok([self.subject.clone(), current]))
        } else {
            self.next()
        }
    }
}
