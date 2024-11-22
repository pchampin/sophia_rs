#![allow(clippy::module_name_repetitions)]

use std::sync::Arc;

use sophia_api::prelude::*;
use sophia_term::ArcStrStash;
use sophia_term::ArcTerm;
use spargebra::algebra::Expression;
use spargebra::algebra::GraphPattern;
use spargebra::algebra::QueryDataset;
use spargebra::term::TriplePattern;
use spargebra::term::Variable;

use crate::bgp;
use crate::binding::populate_variables;
use crate::binding::Binding;
use crate::binding::Bindings;
use crate::expression::ArcExpression;
use crate::stash::ArcStrStashExt;
use crate::SparqlWrapperError;

#[derive(Clone, Debug)]
pub struct ExecState<'a, D: ?Sized> {
    stash: ArcStrStash,
    config: Arc<ExecConfig<'a, D>>,
}

#[derive(Clone, Debug)]
pub struct ExecConfig<'a, D: ?Sized> {
    pub dataset: &'a D,
    pub default_matcher: Vec<Option<ArcTerm>>,
    pub named_graphs: Vec<[Option<ArcTerm>; 1]>,
}

impl<'a, D: Dataset + ?Sized> ExecState<'a, D> {
    pub fn new(
        dataset: &'a D,
        query_dataset: &Option<QueryDataset>,
    ) -> Result<Self, SparqlWrapperError<D::Error>> {
        let mut stash = ArcStrStash::new();
        let default_matcher = match query_dataset {
            None => vec![None],
            Some(query_dataset) => query_dataset
                .default
                .iter()
                .map(|nn| {
                    Some(ArcTerm::Iri(IriRef::new_unchecked(
                        stash.copy_str(nn.as_str()),
                    )))
                })
                .collect(),
        };
        let named_graphs = match query_dataset.as_ref().and_then(|qd| qd.named.as_ref()) {
            None => dataset
                .graph_names()
                .map(|res| res.map(|t| [Some(stash.copy_term(t))]))
                .collect::<Result<Vec<_>, _>>()
                .map_err(SparqlWrapperError::Dataset)?,
            Some(_) => {
                return Err(SparqlWrapperError::NotImplemented("FROM NAMED"));
            }
        };
        let config = Arc::new(ExecConfig {
            dataset,
            default_matcher,
            named_graphs,
        });
        Ok(ExecState { stash, config })
    }

    pub fn config(&self) -> &ExecConfig<'a, D> {
        &self.config
    }

    pub fn config_cloned(&self) -> Arc<ExecConfig<'a, D>> {
        Arc::clone(&self.config)
    }

    pub fn stash_mut(&mut self) -> &mut ArcStrStash {
        &mut self.stash
    }

    pub fn select(
        &mut self,
        pattern: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        use GraphPattern::*;
        match pattern {
            Bgp { patterns } => Ok(self.bgp(patterns, graph_matcher, binding)),
            Path {
                subject,
                path,
                object,
            } => Err(SparqlWrapperError::NotImplemented("Path")),
            Join { left, right } => Err(SparqlWrapperError::NotImplemented("Join")),
            LeftJoin {
                left,
                right,
                expression,
            } => Err(SparqlWrapperError::NotImplemented("LeftJoin")),
            Filter { expr, inner } => self.filter(expr, inner, graph_matcher, binding),
            Union { left, right } => self.union(left, right, graph_matcher, binding),
            Graph { name, inner } => Err(SparqlWrapperError::NotImplemented("Graph")),
            Extend {
                inner,
                variable,
                expression,
            } => self.extend(inner, variable, expression, graph_matcher, binding),
            Minus { left, right } => Err(SparqlWrapperError::NotImplemented("Minus")),
            Values {
                variables,
                bindings,
            } => Err(SparqlWrapperError::NotImplemented("Values")),
            OrderBy { inner, expression } => Err(SparqlWrapperError::NotImplemented("OrderBy")),
            Project { inner, variables } => self.project(inner, variables, graph_matcher, binding),
            Distinct { inner } => Err(SparqlWrapperError::NotImplemented("Distinct")),
            Reduced { inner } => Err(SparqlWrapperError::NotImplemented("Reduced")),
            Slice {
                inner,
                start,
                length,
            } => self.slice(inner, *start, *length, graph_matcher, binding),
            Group {
                inner,
                variables,
                aggregates,
            } => Err(SparqlWrapperError::NotImplemented("Group")),
            Service {
                name,
                inner,
                silent,
            } => Err(SparqlWrapperError::NotImplemented("Service")),
        }
    }

    pub fn ask(
        &mut self,
        pattern: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Result<bool, SparqlWrapperError<D::Error>> {
        self.select(pattern, graph_matcher, binding)
            .map(|binding| binding.into_iter().next().is_some())
    }

    fn bgp(
        &mut self,
        patterns: &[TriplePattern],
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let variables = populate_variables(patterns, &mut self.stash, binding);
        let iter = Box::new(bgp::make_iterator(self, patterns, graph_matcher, binding));
        Bindings { variables, iter }
    }

    fn filter(
        &mut self,
        expression: &Expression,
        inner: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, binding)?;
        let arc_expr = ArcExpression::from_expr(expression, &mut self.stash);
        // config and graph_matcher will be moved in the closure;
        let config = Arc::clone(&self.config);
        let graph_matcher = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();
        // note that config must be an Arc clone,
        // so that we don't "leak" the lifetime of `self` in the return value;
        // for the same reason, we clone the ArcTerms in graph_matcher
        // before passing them to the closure.
        let iter = Box::new(iter.filter(move |resb| {
            match resb {
                Err(_) => true,
                Ok(b) => arc_expr
                    .eval(b, &config, &graph_matcher)
                    .and_then(|e| e.is_truthy())
                    .unwrap_or(false),
            }
        }));
        Ok(Bindings { variables, iter })
    }

    fn union(
        &mut self,
        left: &GraphPattern,
        right: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings {
            variables: lv,
            iter: li,
        } = self.select(left, graph_matcher, binding)?;
        let Bindings {
            variables: rv,
            iter: ri,
        } = self.select(right, graph_matcher, binding)?;
        let mut variables = lv.clone();
        for v in rv {
            if lv.iter().all(|i| *i != v) {
                variables.push(v)
            }
        }
        let iter = Box::new(li.chain(ri));
        Ok(Bindings { variables, iter })
    }

    fn extend(
        &mut self,
        inner: &GraphPattern,
        variable: &Variable,
        expression: &Expression,
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let variable = self.stash.copy_variable(variable);
        let Bindings {
            mut variables,
            iter,
        } = self.select(inner, graph_matcher, binding)?;
        if variables.contains(&variable) {
            return Err(SparqlWrapperError::Override(variable.unwrap()));
        }
        let arc_expr = ArcExpression::from_expr(expression, &mut self.stash);
        variables.push(variable.clone());
        // config, varkey, and graph_matcher will be moved in the closure;
        let config = Arc::clone(&self.config);
        let varkey = variable.unwrap();
        let graph_matcher = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();
        // note that config must be an Arc clone,
        // so that we don't "leak" the lifetime of `self` in the return value;
        // for the same reason, we clone the ArcTerms in graph_matcher
        // before passing them to the closure.
        let iter = Box::new(iter.map(move |resb| {
            resb.map(|mut b| {
                if let Some(val) = arc_expr.eval(&b, &config, &graph_matcher) {
                    b.v.insert(varkey.clone(), val.into_term());
                }
                b
            })
        }));
        Ok(Bindings { variables, iter })
    }

    fn project(
        &mut self,
        inner: &GraphPattern,
        variables: &[Variable],
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let new_variables = variables
            .iter()
            .map(|v| self.stash.copy_variable(v))
            .collect();
        let mut bindings = self.select(inner, graph_matcher, binding)?;
        bindings.variables = new_variables;
        Ok(bindings)
    }

    fn slice(
        &mut self,
        inner: &GraphPattern,
        start: usize,
        length: Option<usize>,
        graph_matcher: &[Option<ArcTerm>],
        binding: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let mut bindings = self.select(inner, graph_matcher, binding)?;
        let skipped = bindings.iter.skip(start);
        bindings.iter = match length {
            Some(n) => Box::new(skipped.take(n)),
            None => Box::new(skipped),
        };
        Ok(bindings)
    }
}

impl<'a, D: Dataset + ?Sized> From<Arc<ExecConfig<'a, D>>> for ExecState<'a, D> {
    fn from(config: Arc<ExecConfig<'a, D>>) -> Self {
        let stash = ArcStrStash::new();
        ExecState { stash, config }
    }
}
