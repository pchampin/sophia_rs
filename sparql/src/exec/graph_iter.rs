use std::{
    collections::{BTreeSet, btree_set},
    sync::Arc,
};

use oxrdf::Variable;
use sophia_api::{prelude::Dataset, term::VarName};
use sophia_term::ArcTerm;
use spargebra::algebra::GraphPattern;

use crate::{
    Bindings, SparqlWrapperError,
    binding::{Binding, BindingsIter},
    exec::ExecState,
    graph_matcher::GraphMatcher,
    stash::ArcStrStashExt,
};

pub struct GraphIter<'a, D: Dataset + ?Sized> {
    state: Arc<ExecState<'a, D>>,
    context: Option<Binding>,
    variable: VarName<Arc<str>>,
    graph_names: btree_set::IntoIter<ArcTerm>,
    inner: GraphPattern,
    inner_iter: Option<BindingsIter<'a, D>>,
}

impl<'a, D: Dataset + ?Sized> GraphIter<'a, D> {
    pub fn new(
        state: &Arc<ExecState<'a, D>>,
        context: Option<&Binding>,
        variable: &Variable,
        graph_names: BTreeSet<ArcTerm>,
        inner: &GraphPattern,
    ) -> Self {
        let state = state.clone();
        let context = context.cloned();
        let variable = state.stash_mut().copy_variable(variable);
        let graph_names = graph_names.into_iter();
        let inner = inner.clone();
        let inner_iter: Option<BindingsIter<'a, D>> = Some(Box::new(std::iter::empty()));
        Self {
            state,
            context,
            variable,
            graph_names,
            inner,
            inner_iter,
        }
    }
}

impl<'a, D: Dataset + ?Sized> Iterator for GraphIter<'a, D> {
    type Item = Result<Binding, SparqlWrapperError<D::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(iter) = &mut self.inner_iter {
            if let Some(res) = iter.next() {
                Some(res)
            } else if let Some(graph_name) = self.graph_names.next() {
                let mut context = self.context.clone().unwrap_or_default();
                context
                    .v
                    .insert(self.variable.clone().unwrap(), graph_name.clone().into());
                let graph_matcher = GraphMatcher::from([Some(graph_name)]);
                let Bindings { iter, .. } =
                    self.state
                        .select(&self.inner, &graph_matcher, Some(&context));
                self.inner_iter = Some(iter);
                self.next()
            } else {
                self.inner_iter = None;
                None
            }
        } else {
            None
        }
    }
}
