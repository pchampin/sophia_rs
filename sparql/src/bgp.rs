use std::sync::Arc;

use sophia_api::dataset::DResult;
use sophia_api::prelude::*;
use spargebra::term::TriplePattern;

use crate::SparqlWrapperError;
use crate::binding::{Binding, populate_binding};
use crate::exec::ExecState;
use crate::graph_matcher::GraphMatcher;
use crate::matcher::SparqlMatcher;

pub fn make_iterator<'a, D: Dataset + ?Sized>(
    state: Arc<ExecState<'a, D>>,
    patterns: &[TriplePattern],
    graph_matcher: &GraphMatcher,
    context: Option<&Binding>,
) -> BgpIterator<'a, D> {
    // TODO one day:
    // test the following "greedy" optimization :
    // 1. first search all ground triple patterns (no var/bnode)
    //    (rationale: they have 0 or 1 result, no need to continue if one of them fail)
    // 2. then try the triple pattern with the most distinct variables
    //    (rationale: bind as much variables/bnodes as possible,
    //     so that further triples are more constrained)
    // 3. then try all triples that contain only variables bound above
    //    (rationale: same as 1. -- those triples are now ground
    //     in the current context)
    // 4. if there are any triple patterns left, restart from 2
    //
    // NB: this can be achieved by simply sorting `patterns`
    // in the corresponding order (by simply looking at the vars/bnodes in the patterns),
    // then simply calling self.bgp_rec as below
    let mut ret = BgpIterator::new(state, patterns, graph_matcher);
    match ret.reseed(Some(context.cloned().unwrap_or_default())) {
        Ok(()) => ret,
        Err(err) => BgpIterator::EmptyBgp {
            result: Some(Err(SparqlWrapperError::Dataset(err))),
        },
    }
}

#[expect(clippy::large_enum_variant)]
pub enum BgpIterator<'a, D: Dataset + ?Sized> {
    EmptyBgp {
        result: Option<Result<Binding, SparqlWrapperError<D::Error>>>,
    },
    Triple {
        state: Arc<ExecState<'a, D>>,
        graph_matcher: GraphMatcher,
        pattern: TriplePattern,
        matchers: [SparqlMatcher; 3],
        seed: Option<Binding>,
        is_bound: [bool; 3],
        quads: Box<dyn Iterator<Item = DResult<D, <D as Dataset>::Quad<'a>>> + 'a>,
        bindings: Box<BgpIterator<'a, D>>,
    },
}

impl<'a, D: Dataset + ?Sized> BgpIterator<'a, D> {
    pub fn new(
        state: Arc<ExecState<'a, D>>,
        patterns: &[TriplePattern],
        graph_matcher: &GraphMatcher,
    ) -> Self {
        let seed = None;
        if let [first, remaining @ ..] = patterns {
            let graph_matcher = graph_matcher.clone();
            let pattern = first.clone();
            let matchers = SparqlMatcher::build3(&pattern, &mut state.stash_mut());
            let is_bound = [false; 3];
            let quads = Box::new(std::iter::empty());
            let bindings = Box::new(BgpIterator::new(state.clone(), remaining, &graph_matcher));
            Self::Triple {
                state,
                graph_matcher,
                pattern,
                matchers,
                seed,
                is_bound,
                quads,
                bindings,
            }
        } else {
            Self::EmptyBgp {
                result: seed.map(Ok),
            }
        }
    }

    pub fn reseed(&mut self, new_seed: Option<Binding>) -> DResult<D, ()> {
        match self {
            Self::EmptyBgp { result } => {
                *result = new_seed.map(Ok);
            }
            Self::Triple {
                state,
                graph_matcher,
                pattern,
                matchers,
                seed,
                is_bound,
                quads,
                bindings,
            } => {
                *seed = new_seed;
                if let Some(context) = &seed {
                    let mut stash = state.stash_mut();
                    let sm = matchers[0].bound_or_else(|| {
                        SparqlMatcher::build_with(&pattern.subject, context, &mut stash)
                    });
                    let pm = matchers[1].bound_or_else(|| {
                        SparqlMatcher::build_with(&pattern.predicate, context, &mut stash)
                    });
                    let om = matchers[2].bound_or_else(|| {
                        SparqlMatcher::build_with(&pattern.object, context, &mut stash)
                    });
                    drop(stash);
                    *is_bound = [sm.is_bound(), pm.is_bound(), om.is_bound()];
                    *quads = Box::new(state.dataset().quads_matching(
                        sm,
                        pm,
                        om,
                        graph_matcher.clone(),
                    ));
                    Self::handle_new_quad(quads, seed, *is_bound, pattern, bindings, state)?;
                }
            }
        }
        Ok(())
    }

    fn handle_new_quad(
        quads: &mut Box<dyn Iterator<Item = DResult<D, <D as Dataset>::Quad<'a>>> + 'a>,
        seed: &mut Option<Binding>,
        [sb, pb, ob]: [bool; 3],
        pattern: &TriplePattern,
        bindings: &mut Box<BgpIterator<'a, D>>,
        state: &mut Arc<ExecState<'a, D>>,
    ) -> DResult<D, ()> {
        debug_assert!(seed.is_some());
        let Some(context) = seed else { unreachable!() };
        match quads.next() {
            Some(Ok(q)) => {
                let mut b = context.clone();
                let we_have_a_match = {
                    let mut stash = state.stash_mut();
                    (sb || populate_binding(&pattern.subject, q.s(), &mut b, &mut stash))
                        && (pb || populate_binding(&pattern.predicate, q.p(), &mut b, &mut stash))
                        && (ob || populate_binding(&pattern.object, q.o(), &mut b, &mut stash))
                };
                if we_have_a_match {
                    bindings.reseed(Some(b))?;
                    Ok(())
                } else {
                    Self::handle_new_quad(quads, seed, [sb, pb, ob], pattern, bindings, state)
                }
            }
            Some(Err(err)) => {
                *seed = None;
                Err(err)
            }
            None => {
                *seed = None;
                Ok(())
            }
        }
    }
}

impl<'a, D: Dataset + ?Sized> Iterator for BgpIterator<'a, D> {
    type Item = Result<Binding, SparqlWrapperError<D::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::EmptyBgp { result } => result.take(),
            Self::Triple {
                state,
                pattern,
                seed,
                is_bound,
                quads,
                bindings,
                ..
            } => {
                if seed.is_some() {
                    let opt = bindings.next();
                    if opt.is_some() {
                        opt
                    } else {
                        let res =
                            Self::handle_new_quad(quads, seed, *is_bound, pattern, bindings, state);
                        match res {
                            Ok(()) => self.next(),
                            Err(err) => Some(Err(SparqlWrapperError::Dataset(err))),
                        }
                    }
                } else {
                    None
                }
            }
        }
    }
}
