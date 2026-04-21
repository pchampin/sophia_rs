use std::{
    collections::{BTreeSet, btree_set},
    sync::Arc,
};

use oxrdf::Variable;
use sophia_api::{prelude::Dataset, term::VarName};
use sophia_term::ArcTerm;
use spargebra::algebra::GraphPattern;

use crate::{
    SparqlWrapperError,
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
    current_graph_name: ArcTerm,
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
        let current_graph_name = ArcTerm::Iri(sophia_iri::IriRef::new_unchecked(Arc::from("")));
        Self {
            state,
            context,
            variable,
            graph_names,
            inner,
            inner_iter,
            current_graph_name,
        }
    }
}

impl<'a, D: Dataset + ?Sized> Iterator for GraphIter<'a, D> {
    type Item = Result<Binding, SparqlWrapperError<D::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(iter) = &mut self.inner_iter {
            if let Some(res) = iter.next() {
                Some(res.map(|mut b| {
                    b.v.entry(self.variable.clone().unwrap())
                        .or_insert_with(|| self.current_graph_name.clone().into());
                    b
                }))
            } else if let Some(graph_name) = self.graph_names.next() {
                self.current_graph_name = graph_name.clone();
                let mut context = self.context.clone().unwrap_or_default();
                context
                    .v
                    .insert(self.variable.clone().unwrap(), graph_name.clone().into());
                let graph_matcher = GraphMatcher::from([Some(graph_name)]);
                self.inner_iter = Some(
                    self.state
                        .select(&self.inner, &graph_matcher, Some(&context))
                        .iter,
                );
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
