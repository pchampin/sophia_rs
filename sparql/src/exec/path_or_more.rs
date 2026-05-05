//! Iterator used by property paths expression (ZeroOrMore and OneOrMore)

use std::{
    collections::{HashMap, hash_map::Entry},
    sync::Arc,
};

use sophia_api::{
    dataset::DResult,
    prelude::Dataset,
    term::{Term, matcher::TermMatcher},
};
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
    iter_seeds: Box<dyn Iterator<Item = DResult<D, [ArcTerm; 2]>> + 'a>,
    iter_stack: Vec<Box<dyn Iterator<Item = DResult<D, [ArcTerm; 2]>> + 'a>>,
    seen: HashMap<[ArcTerm; 2], ()>,
    path: PropertyPathExpression,
    omatcher: SparqlMatcher,
    gmatcher: GraphMatcher,
}

impl<'a, D: Dataset + ?Sized> PathOrMore<'a, D> {
    pub fn new(
        state: Arc<ExecState<'a, D>>,
        mut iter_seeds: Box<dyn Iterator<Item = DResult<D, [ArcTerm; 2]>> + 'a>,
        path: PropertyPathExpression,
        omatcher: SparqlMatcher,
        gmatcher: GraphMatcher,
    ) -> Self {
        let (subject, current) = match iter_seeds.next() {
            Some(Ok([s, c])) => (s, Some(Ok(c))),
            Some(Err(err)) => ("dummy".into_term(), Some(Err(err))),
            None => ("dummy".into_term(), None),
        };
        let iter_stack = vec![];
        let seen = HashMap::new();
        Self {
            state,
            subject,
            current,
            iter_seeds,
            iter_stack,
            seen,
            path,
            omatcher,
            gmatcher,
        }
    }

    // Compute the next version of self.current.
    // This may also change self.subject, in which case the old value is returned.
    fn populate_next_current(&mut self) -> Option<ArcTerm> {
        debug_assert!(self.current.is_none());
        if let Some(iter) = self.iter_stack.last_mut() {
            self.current = iter.next().map(|res| res.map(|[_, o]| o));
            if self.current.is_none() {
                self.iter_stack.pop();
                self.populate_next_current()
            } else {
                None
            }
        } else {
            match self.iter_seeds.next() {
                Some(Ok([mut s, c])) => {
                    std::mem::swap(&mut self.subject, &mut s);
                    self.current = Some(Ok(c));
                    Some(s)
                }
                Some(Err(err)) => {
                    self.current = Some(Err(err));
                    None
                }
                None => None,
            }
        }
    }
}

impl<'a, D: Dataset + ?Sized> Iterator for PathOrMore<'a, D> {
    type Item = DResult<D, [ArcTerm; 2]>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = match self.current.take()? {
            Err(err) => return Some(Err(err)),
            Ok(current) => current,
        };
        let current = match self.seen.entry([self.subject.clone(), current]) {
            Entry::Occupied(_) => {
                self.populate_next_current();
                return self.next();
            }
            Entry::Vacant(e) => {
                let current = e.key()[1].clone();
                e.insert(());
                current
            }
        };
        self.iter_stack.push(self.state.path_rec(
            SparqlMatcher::Bound(current.clone().into()),
            &self.path,
            SparqlMatcher::Free,
            &self.gmatcher,
        ));
        let old_subject = self.populate_next_current();
        if self.omatcher.matches(&current) {
            let subject = old_subject.unwrap_or_else(|| self.subject.clone());
            Some(Ok([subject, current]))
        } else {
            self.next()
        }
    }
}
