use std::sync::Arc;

use sophia_api::prelude::Dataset;
use sophia_term::ArcTerm;
use spargebra::algebra::{Expression, GraphPattern};

use crate::{
    SparqlWrapperError,
    binding::{Binding, BindingsIter},
    exec::ExecState,
    expression::ArcExpression,
};

pub struct LeftJoinIter<'a, D: Dataset + ?Sized> {
    b1s: BindingsIter<'a, D>,
    b1: Binding,
    b2s: BindingsIter<'a, D>,
    state: Arc<ExecState<'a, D>>,
    graph_matcher: Vec<Option<ArcTerm>>,
    right: GraphPattern,
    expression: Option<ArcExpression>,
    iter_state: IterState,
}

impl<'a, D: Dataset + ?Sized> LeftJoinIter<'a, D> {
    pub fn new(
        b1s: BindingsIter<'a, D>,
        b1: Binding,
        b2s: BindingsIter<'a, D>,
        state: &Arc<ExecState<'a, D>>,
        right: &GraphPattern,
        expression: &Option<Expression>,
        graph_matcher: &[Option<ArcTerm>],
    ) -> Self {
        let state = Arc::clone(state);
        let right = right.clone();
        let expression = expression
            .as_ref()
            .map(|e| ArcExpression::from_expr(e, &mut state.stash_mut()));
        let graph_matcher = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();
        let iter_state = IterState::B2DidntMatch;

        Self {
            b1s,
            b1,
            b2s,
            state,
            right,
            expression,
            graph_matcher,
            iter_state,
        }
    }

    fn next_b2s_needs_init(&mut self) -> Option<<Self as Iterator>::Item> {
        self.b2s = self
            .state
            .select(&self.right, &self.graph_matcher, Some(&self.b1))
            .iter;
        self.iter_state = IterState::B2DidntMatch;
        self.next_b2s_ready()
    }

    fn next_b2s_ready(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.b2s.next() {
            Some(Err(err)) => {
                self.iter_state = IterState::Finished;
                Some(Err(err))
            }
            Some(Ok(b2)) => {
                if let Some(b) = b2.merge_if_compatible(Some(&self.b1)).filter(|b| {
                    self.expression
                        .as_ref()
                        .map(|e| e.eval_truthy(b, &self.state, &self.graph_matcher))
                        .unwrap_or(true)
                }) {
                    self.iter_state = IterState::B2Matched;
                    Some(Ok(b))
                } else {
                    self.next_b2s_ready()
                }
            }
            None => {
                if matches!(self.iter_state, IterState::B2DidntMatch) {
                    match self.b1s.next() {
                        Some(Ok(mut other_b1)) => {
                            self.iter_state = IterState::B2sNeedsInit;
                            std::mem::swap(&mut self.b1, &mut other_b1);
                            Some(Ok(other_b1))
                        }
                        Some(Err(err)) => {
                            self.iter_state = IterState::Finished;
                            Some(Err(err))
                        }
                        None => {
                            self.iter_state = IterState::Finished;
                            Some(Ok(self.b1.clone()))
                        }
                    }
                } else {
                    debug_assert!(matches!(self.iter_state, IterState::B2Matched));
                    match self.b1s.next() {
                        Some(Ok(new_b1)) => {
                            self.iter_state = IterState::B2sNeedsInit;
                            self.b1 = new_b1;
                            self.next_b2s_needs_init()
                        }
                        err_or_none => {
                            self.iter_state = IterState::Finished;
                            err_or_none
                        }
                    }
                }
            }
        }
    }
}

impl<'a, D: Dataset + ?Sized> Iterator for LeftJoinIter<'a, D> {
    type Item = Result<Binding, SparqlWrapperError<<D as Dataset>::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter_state {
            IterState::Finished => None,
            IterState::B2sNeedsInit => self.next_b2s_needs_init(),
            _ => self.next_b2s_ready(),
        }
    }
}

#[derive(Clone, Debug)]
enum IterState {
    B2sNeedsInit,
    B2DidntMatch,
    B2Matched,
    Finished,
}
