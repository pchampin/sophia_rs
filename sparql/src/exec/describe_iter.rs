use std::collections::{HashSet, VecDeque};
use std::sync::Arc;

use sophia_api::dataset::{DResult, Dataset};
use sophia_api::prelude::Any;
use sophia_api::quad::Quad;
use sophia_term::ArcTerm;

use crate::exec::ExecState;
use crate::{Bindings, ResultTerm, SparqlWrapperError};

pub struct DescribeIter<'a, D: Dataset + ?Sized> {
    state: Arc<ExecState<'a, D>>,
    bindings: Bindings<'a, D>,
    candidates: VecDeque<ArcTerm>,
    iter: Box<dyn Iterator<Item = DResult<D, <D as Dataset>::Quad<'a>>> + 'a>,
    out: bool,
    seen: HashSet<ArcTerm>,
}

impl<'a, D> DescribeIter<'a, D>
where
    D: Dataset + ?Sized,
{
    pub fn new(state: Arc<ExecState<'a, D>>, bindings: Bindings<'a, D>) -> Self {
        let candidates = VecDeque::new();
        let iter = Box::new(std::iter::empty());
        let out = false;
        let seen = HashSet::new();
        Self {
            state,
            bindings,
            candidates,
            iter,
            out,
            seen,
        }
    }

    fn consider_candidates(&mut self, term: &ArcTerm) {
        match term {
            ArcTerm::BlankNode(_) => {
                if !self.seen.contains(term) {
                    self.seen.insert(term.clone());
                    self.candidates.push_back(term.clone());
                }
            }
            ArcTerm::Triple(spo) => spo.iter().for_each(|t| self.consider_candidates(t)),
            _ => {}
        }
    }

    fn next_triple(&mut self) -> Result<Option<[ArcTerm; 3]>, SparqlWrapperError<D::Error>> {
        if let Some(res) = self.iter.next() {
            let t = res
                .map_err(SparqlWrapperError::Dataset)?
                .to_spog()
                .0
                .map(|t| self.state.stash_mut().copy_term(t));
            if self.out {
                self.consider_candidates(&t[2]);
            } else {
                self.consider_candidates(&t[0]);
            }
            if self.seen.insert(ArcTerm::Triple(Arc::new(t.clone()))) {
                self.candidates
                    .push_back(ArcTerm::Triple(Arc::new(t.clone())));
            }
            Ok(Some(t))
        } else if self.out {
            self.out = false;
            debug_assert!(!self.candidates.is_empty());
            let candidate = self.candidates.pop_front().unwrap();
            self.iter = Box::new(self.state.dataset().quads_matching(
                Any,
                Any,
                [candidate],
                self.state.default_matcher().to_vec(),
            ));
            self.next_triple()
        } else if !self.candidates.is_empty() {
            self.out = true;
            let candidate = self.candidates.front().unwrap().clone();
            self.iter = Box::new(self.state.dataset().quads_matching(
                [candidate],
                Any,
                Any,
                self.state.default_matcher().to_vec(),
            ));
            self.next_triple()
        } else if let Some(res) = self.bindings.iter.next() {
            let b = res?;
            for term in b.v.into_values() {
                let term = term.unwrap();
                if self.seen.insert(term.clone()) {
                    self.candidates.push_back(term);
                }
            }
            self.next_triple()
        } else {
            Ok(None)
        }
    }
}

impl<'a, D> Iterator for DescribeIter<'a, D>
where
    D: Dataset + ?Sized,
{
    type Item = Result<[ResultTerm; 3], SparqlWrapperError<D::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_triple()
            .map(|opt| opt.map(|spo| spo.map(ResultTerm::from)))
            .transpose()
    }
}
