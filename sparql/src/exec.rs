#![allow(clippy::module_name_repetitions)]

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::sync::Arc;

use sophia_api::prelude::*;
use sophia_api::term::VarName;
use sophia_iri::resolve::BaseIri;
use sophia_term::ArcStrStash;
use sophia_term::ArcTerm;
use spargebra::algebra::Expression;
use spargebra::algebra::GraphPattern;
use spargebra::algebra::OrderExpression;
use spargebra::algebra::QueryDataset;
use spargebra::term::NamedNodePattern;
use spargebra::term::TriplePattern;
use spargebra::term::Variable;

use crate::SparqlWrapperError;
use crate::bgp;
use crate::binding::Binding;
use crate::binding::Bindings;
use crate::binding::populate_variables;
use crate::expression::ArcExpression;
use crate::stash::ArcStrStashExt;

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
    pub base_iri: Option<BaseIri<String>>,
    pub now: chrono::DateTime<chrono::FixedOffset>,
}

impl<'a, D: Dataset + ?Sized> ExecState<'a, D> {
    pub fn new(
        dataset: &'a D,
        query_dataset: &Option<QueryDataset>,
        base_iri: &Option<oxiri::Iri<String>>,
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
        let base_iri = base_iri.clone().map(Into::into);
        let now = chrono::Local::now().fixed_offset();
        let config = Arc::new(ExecConfig {
            dataset,
            default_matcher,
            named_graphs,
            base_iri,
            now,
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
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        use GraphPattern::*;
        match pattern {
            Bgp { patterns } => Ok(self.bgp(patterns, graph_matcher, context)),
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
            Filter { expr, inner } => self.filter(expr, inner, graph_matcher, context),
            Union { left, right } => self.union(left, right, graph_matcher, context),
            Graph { name, inner } => self.graph(name, inner, context),
            Extend {
                inner,
                variable,
                expression,
            } => self.extend(inner, variable, expression, graph_matcher, context),
            Minus { left, right } => Err(SparqlWrapperError::NotImplemented("Minus")),
            Values {
                variables,
                bindings,
            } => Err(SparqlWrapperError::NotImplemented("Values")),
            OrderBy { inner, expression } => {
                self.order_by(inner, expression, graph_matcher, context)
            }
            Project { inner, variables } => self.project(inner, variables, graph_matcher, context),
            Distinct { inner } => self.distinct(inner, graph_matcher, context),
            Reduced { inner } => Err(SparqlWrapperError::NotImplemented("Reduced")),
            Slice {
                inner,
                start,
                length,
            } => self.slice(inner, *start, *length, graph_matcher, context),
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
        context: Option<&Binding>,
    ) -> Result<bool, SparqlWrapperError<D::Error>> {
        self.select(pattern, graph_matcher, context)
            .map(|binding| binding.into_iter().next().is_some())
    }

    fn bgp(
        &mut self,
        patterns: &[TriplePattern],
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let variables = populate_variables(patterns, &mut self.stash, context);
        let iter = Box::new(bgp::make_iterator(self, patterns, graph_matcher, context));
        Bindings { variables, iter }
    }

    fn distinct(
        &mut self,
        inner: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, context)?;
        let mut seen = HashSet::new();
        // config and graph_matcher will be moved in the closure;
        let config = Arc::clone(&self.config);
        let graph_matcher = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();
        // note that config must be an Arc clone,
        // so that we don't "leak" the lifetime of `self` in the return value;
        // for the same reason, we clone the ArcTerms in graph_matcher
        // before passing them to the closure.
        let variables2 = variables.clone();
        let iter = Box::new(iter.filter(move |resb| match resb {
            Err(_) => true,
            Ok(b) => {
                let hashable: Vec<_> = variables2
                    .iter()
                    .map(|v| b.v.get(v.as_str()).map(|t| t.inner().clone()))
                    .collect();
                seen.insert(hashable)
            }
        }));
        Ok(Bindings { variables, iter })
    }

    fn filter(
        &mut self,
        expression: &Expression,
        inner: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, context)?;
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
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings {
            variables: lv,
            iter: li,
        } = self.select(left, graph_matcher, context)?;
        let Bindings {
            variables: rv,
            iter: ri,
        } = self.select(right, graph_matcher, context)?;
        let mut variables = lv.clone();
        for v in rv {
            if lv.iter().all(|i| *i != v) {
                variables.push(v)
            }
        }
        let iter = Box::new(li.chain(ri));
        Ok(Bindings { variables, iter })
    }

    fn graph(
        &mut self,
        name: &NamedNodePattern,
        inner: &GraphPattern,
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        match name {
            NamedNodePattern::NamedNode(nn) => {
                let graph_matcher = vec![Some(ArcTerm::Iri(IriRef::new_unchecked(
                    self.stash.copy_str(nn.as_str()),
                )))];
                self.select(inner, &graph_matcher, context)
            }
            NamedNodePattern::Variable(var) => {
                if let Some(name) = context.and_then(|b| b.v.get(var.as_str())) {
                    let graph_matcher = vec![Some(name.inner().clone())];
                    self.select(inner, &graph_matcher, context)
                } else {
                    let graph_names = self
                        .config()
                        .dataset
                        .graph_names()
                        .map(|res| res.map(|t| self.stash.copy_term(t)))
                        .collect::<Result<BTreeSet<_>, _>>()
                        .map_err(SparqlWrapperError::Dataset)?;
                    if graph_names.is_empty() {
                        Ok(Bindings::empty())
                    } else {
                        self.graph_rec(var.as_str(), graph_names.into_iter(), inner, context)
                    }
                }
            }
        }
    }

    fn graph_rec(
        &mut self,
        var: &str,
        mut graph_names: std::collections::btree_set::IntoIter<ArcTerm>,
        inner: &GraphPattern,
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        if let Some(name) = graph_names.next() {
            let mut b = context.cloned().unwrap_or_else(Binding::default);
            b.v.insert(self.stash.copy_str(var), name.clone().into());
            let graph_matcher = vec![Some(name)];
            let Bindings { variables, iter } = self.select(inner, &graph_matcher, Some(&b))?;
            let iter = Box::new(iter.chain(Box::new(
                self.graph_rec(var, graph_names, inner, context)?.iter,
            )));
            Ok(Bindings { variables, iter })
        } else {
            Ok(Bindings::empty())
        }
    }

    fn extend(
        &mut self,
        inner: &GraphPattern,
        variable: &Variable,
        expression: &Expression,
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let variable = self.stash.copy_variable(variable);
        let Bindings {
            mut variables,
            iter,
        } = self.select(inner, graph_matcher, context)?;
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

    fn order_by(
        &mut self,
        inner: &GraphPattern,
        expression: &[OrderExpression],
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let criteria: Vec<_> = expression
            .iter()
            .map(|oe| match oe {
                OrderExpression::Asc(e) => (ArcExpression::from_expr(e, &mut self.stash), false),
                OrderExpression::Desc(e) => (ArcExpression::from_expr(e, &mut self.stash), true),
            })
            .collect();

        // config and graph_matcher will be moved in the closure;
        let config = Arc::clone(&self.config);
        let graph_matcher2 = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();
        // note that config must be an Arc clone,
        // so that we don't "leak" the lifetime of `self` in the return value;
        // for the same reason, we clone the ArcTerms in graph_matcher
        // before passing them to the closure.

        fn cmp_bindings_with<D: Dataset + ?Sized>(
            b1: &Binding,
            b2: &Binding,
            criteria: &[(ArcExpression, bool)],
            config: &Arc<ExecConfig<D>>,
            graph_matcher: &[Option<ArcTerm>],
        ) -> Ordering {
            match criteria {
                [] => Ordering::Equal,
                [(expr, desc), rest @ ..] => {
                    let v1 = expr.eval(b1, config, graph_matcher);
                    let v2 = expr.eval(b2, config, graph_matcher);
                    let o = match (v1, v2) {
                        (None, None) => Ordering::Equal,
                        (None, Some(_)) => Ordering::Less,
                        (Some(v1), v2) => v1.sparql_order_by(&v2),
                    };
                    let o = if *desc { o.reverse() } else { o };
                    o.then_with(|| cmp_bindings_with(b1, b2, rest, config, graph_matcher))
                }
            }
        }

        let Bindings { variables, iter } = self.select(inner, graph_matcher, context)?;
        let mut bindings = iter.collect::<Result<Vec<_>, _>>()?;
        bindings.sort_unstable_by(|b1, b2| {
            cmp_bindings_with(b1, b2, &criteria, &config, &graph_matcher2)
        });
        let iter = Box::new(bindings.into_iter().map(Ok));
        Ok(Bindings { variables, iter })
    }

    fn project(
        &mut self,
        inner: &GraphPattern,
        variables: &[Variable],
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let new_variables: Vec<VarName<Arc<str>>> = variables
            .iter()
            .map(|v| self.stash.copy_variable(v))
            .collect();
        let filtered_context = context.map(|b| {
            let v: std::collections::HashMap<_, _> = new_variables
                .iter()
                .filter_map(|v| b.v.get(v.as_str()).map(|t| (v.clone().unwrap(), t.clone())))
                .collect();
            let b = Default::default();
            Binding { v, b }
        });
        let mut bindings = self.select(inner, graph_matcher, filtered_context.as_ref())?;
        bindings.variables = new_variables;
        Ok(bindings)
    }

    fn slice(
        &mut self,
        inner: &GraphPattern,
        start: usize,
        length: Option<usize>,
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let mut bindings = self.select(inner, graph_matcher, context)?;
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
