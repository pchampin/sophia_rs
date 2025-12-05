#![allow(clippy::module_name_repetitions)]

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;

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
use crate::binding::{Binding, Bindings, populate_variables};
use crate::expression::ArcExpression;
use crate::stash::ArcStrStashExt;

#[derive(Debug)]
pub struct ExecState<'a, D: ?Sized> {
    stash: Mutex<ArcStrStash>,
    config: Arc<ExecConfig<'a, D>>,
}

#[derive(Clone, Debug)]
pub struct ExecConfig<'a, D: ?Sized> {
    pub dataset: &'a D,
    pub default_matcher: Vec<Option<ArcTerm>>,
    pub base_iri: Option<BaseIri<String>>,
    pub now: chrono::DateTime<chrono::FixedOffset>,
}

impl<'a, D: Dataset + ?Sized> ExecState<'a, D> {
    pub fn new(
        dataset: &'a D,
        query_dataset: &Option<QueryDataset>,
        base_iri: &Option<oxiri::Iri<String>>,
    ) -> Result<Arc<Self>, SparqlWrapperError<D::Error>> {
        let stash = Mutex::new(ArcStrStash::new());
        let default_matcher = match query_dataset {
            None => vec![None],
            Some(query_dataset) => query_dataset
                .default
                .iter()
                .map(|nn| {
                    Some(ArcTerm::Iri(IriRef::new_unchecked(
                        stash.lock().unwrap().copy_str(nn.as_str()),
                    )))
                })
                .collect(),
        };
        let base_iri = base_iri.clone().map(Into::into);
        let now = chrono::Local::now().fixed_offset();
        let config = Arc::new(ExecConfig {
            dataset,
            default_matcher,
            base_iri,
            now,
        });
        Ok(Arc::new(ExecState { stash, config }))
    }

    pub fn config(&self) -> &ExecConfig<'a, D> {
        &self.config
    }

    pub fn config_cloned(&self) -> Arc<ExecConfig<'a, D>> {
        Arc::clone(&self.config)
    }

    pub fn stash_mut(&self) -> MutexGuard<'_, ArcStrStash> {
        self.stash.lock().unwrap()
    }

    /// Evaluates `pattern` on the active graph identified by `graph_matcher`.
    ///
    /// ### About `graph_matcher`
    ///
    /// SPARQL uses a single "active graph" at any time.
    /// However, this implementation allows `graph_matcher` to match several graphs in the underlying dataset.
    /// This is useful when using multiple `FROM` clauses (as opposed to `FROM NAMED`),
    /// to compose an ad-hoc default graph.
    /// Conceptually, the (unique) active graph is this composed default graph,
    /// but technically, this graph is not materialized,
    /// and the multiple named graphs that compose it are queried instead.
    ///
    /// ### About `context`
    ///
    /// If provided, `context` is a binding that all resulting bindings are intended to be merged into
    /// (and thereforme, compatible with).
    /// Note that this is only an optimization hint used by some evaluation sub-functions (e.g. [`ExecState::bgp`]).
    /// This function **does not** guarantee that the returned bindings are actually compatible with the `context` or are merged with it.
    /// It is still the responsibility of the caller function to ensure that.
    pub fn select(
        self: &Arc<Self>,
        pattern: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        use GraphPattern::*;
        #[expect(unused_variables)]
        match pattern {
            Bgp { patterns } => Ok(self.bgp(patterns, graph_matcher, context)),
            Path {
                subject,
                path,
                object,
            } => Err(SparqlWrapperError::NotImplemented("Path")),
            Join { left, right } => self.join(left, right, graph_matcher, context),
            LeftJoin {
                left,
                right,
                expression,
            } => Err(SparqlWrapperError::NotImplemented("LeftJoin")),
            Filter { expr, inner } => self.filter(expr, inner, graph_matcher),
            Union { left, right } => self.union(left, right, graph_matcher, context),
            Graph { name, inner } => self.graph(name, inner, context),
            Extend {
                inner,
                variable,
                expression,
            } => self.extend(inner, variable, expression, graph_matcher),
            Minus { left, right } => Err(SparqlWrapperError::NotImplemented("Minus")),
            Values {
                variables,
                bindings,
            } => Err(SparqlWrapperError::NotImplemented("Values")),
            OrderBy { inner, expression } => self.order_by(inner, expression, graph_matcher),
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
        self: &Arc<Self>,
        pattern: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
    ) -> Result<bool, SparqlWrapperError<D::Error>> {
        self.select(pattern, graph_matcher, None)
            .map(|binding| binding.into_iter().next().is_some())
    }

    /// Evaluates `patterns` on the active graph identified by `graph_matcher`.
    ///
    /// The hint `context` is used by this method;
    /// the returned bindings are all compatible with it, and include it.
    fn bgp(
        self: &Arc<Self>,
        patterns: &[TriplePattern],
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let variables = populate_variables(patterns, &mut self.stash_mut(), context);
        let iter = Box::new(bgp::make_iterator(
            self.clone(),
            patterns,
            graph_matcher,
            context,
        ));
        Bindings { variables, iter }
    }

    fn join(
        self: &Arc<Self>,
        left: &GraphPattern,
        right: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings {
            mut variables,
            iter: mut iter1,
        } = self.select(left, graph_matcher, context)?;

        let first1 = iter1.next().transpose()?;

        let Bindings {
            variables: vars2,
            iter: iter2,
        } = self.select(right, graph_matcher, first1.as_ref())?;

        for v in vars2 {
            if !variables.contains(&v) {
                variables.push(v);
            }
        }
        if first1.is_none() {
            return Ok(Bindings::empty_with(variables));
        }
        // right and graph_matcher will be moved in the closure;
        // note that they must be cloned, so that we don't "leak" the lifetime of `self` in the return value
        let state = Arc::clone(self);
        let right = right.clone();
        let graph_matcher = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();

        let iter = Box::new(
            iter2
                .filter_map(move |resb| {
                    resb.map(|b| b.merge_if_compatible(first1.as_ref()))
                        .transpose()
                })
                .chain(iter1.flat_map(move |resb1| match resb1 {
                    Ok(b1) => match state.select(&right, &graph_matcher, Some(&b1)) {
                        Ok(bs) => {
                            MyIterator::PassThrough(bs.iter.filter_map(move |resb2| {
                                resb2
                                    .map(|b2| b2.merge_if_compatible(Some(&b1)))
                                    .transpose()
                            }))
                        }
                        Err(err) => MyIterator::Err(err),
                    },
                    Err(err) => MyIterator::Err(err),
                })),
        );

        Ok(Bindings { variables, iter })
    }

    fn distinct(
        self: &Arc<Self>,
        inner: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, context)?;
        let mut seen = HashSet::new();
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
        self: &Arc<Self>,
        expression: &Expression,
        inner: &GraphPattern,
        graph_matcher: &[Option<ArcTerm>],
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, None)?;
        let arc_expr = ArcExpression::from_expr(expression, &mut self.stash_mut());
        // self (as state) and graph_matcher will be moved in the closure;
        // they must be cloned to avoid leaking the lifetime of `self`
        let state = self.clone();
        let graph_matcher = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();
        let iter = Box::new(iter.filter(move |resb| {
            match resb {
                Err(_) => true,
                Ok(b) => arc_expr
                    .eval(b, &state, &graph_matcher)
                    .and_then(|e| e.is_truthy())
                    .unwrap_or(false),
            }
        }));
        Ok(Bindings { variables, iter })
    }

    fn union(
        self: &Arc<Self>,
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
        self: &Arc<Self>,
        name: &NamedNodePattern,
        inner: &GraphPattern,
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        match name {
            NamedNodePattern::NamedNode(nn) => {
                let graph_matcher = vec![Some(ArcTerm::Iri(IriRef::new_unchecked(
                    self.stash_mut().copy_str(nn.as_str()),
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
                        .map(|res| res.map(|t| self.stash_mut().copy_term(t)))
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
        self: &Arc<Self>,
        var: &str,
        mut graph_names: std::collections::btree_set::IntoIter<ArcTerm>,
        inner: &GraphPattern,
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        if let Some(name) = graph_names.next() {
            let mut b = context.cloned().unwrap_or_else(Binding::default);
            b.v.insert(self.stash_mut().copy_str(var), name.clone().into());
            let graph_matcher = vec![Some(name)];
            let Bindings { variables, iter } = self.select(inner, &graph_matcher, Some(&b))?;
            let iter = Box::new(iter.chain(
                self.graph_rec(var, graph_names, inner, context)?.iter,
            ));
            Ok(Bindings { variables, iter })
        } else {
            Ok(Bindings::empty())
        }
    }

    fn extend(
        self: &Arc<Self>,
        inner: &GraphPattern,
        variable: &Variable,
        expression: &Expression,
        graph_matcher: &[Option<ArcTerm>],
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let variable = self.stash_mut().copy_variable(variable);
        let Bindings {
            mut variables,
            iter,
        } = self.select(inner, graph_matcher, None)?;
        if variables.contains(&variable) {
            return Err(SparqlWrapperError::Override(variable.unwrap()));
        }
        let arc_expr = ArcExpression::from_expr(expression, &mut self.stash_mut());
        variables.push(variable.clone());
        // self (as state) and graph_matcher will be moved in the closure;
        // they must be cloned to avoid leaking the lifetime of `self`
        let state = Arc::clone(self);
        let varkey = variable.unwrap();
        let graph_matcher = graph_matcher.iter().map(Clone::clone).collect::<Vec<_>>();

        let iter = Box::new(iter.map(move |resb| {
            resb.map(|mut b| {
                if let Some(val) = arc_expr.eval(&b, &state, &graph_matcher) {
                    b.v.insert(varkey.clone(), val.into_term());
                }
                b
            })
        }));
        Ok(Bindings { variables, iter })
    }

    fn order_by(
        self: &Arc<Self>,
        inner: &GraphPattern,
        expression: &[OrderExpression],
        graph_matcher: &[Option<ArcTerm>],
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let criteria: Vec<_> = expression
            .iter()
            .map(|oe| match oe {
                OrderExpression::Asc(e) => {
                    (ArcExpression::from_expr(e, &mut self.stash_mut()), false)
                }
                OrderExpression::Desc(e) => {
                    (ArcExpression::from_expr(e, &mut self.stash_mut()), true)
                }
            })
            .collect();

        fn cmp_bindings_with<D: Dataset + ?Sized>(
            b1: &Binding,
            b2: &Binding,
            criteria: &[(ArcExpression, bool)],
            state: &Arc<ExecState<D>>,
            graph_matcher: &[Option<ArcTerm>],
        ) -> Ordering {
            match criteria {
                [] => Ordering::Equal,
                [(expr, desc), rest @ ..] => {
                    let v1 = expr.eval(b1, state, graph_matcher);
                    let v2 = expr.eval(b2, state, graph_matcher);
                    let o = match (v1, v2) {
                        (None, None) => Ordering::Equal,
                        (None, Some(_)) => Ordering::Less,
                        (Some(v1), v2) => v1.sparql_order_by(&v2),
                    };
                    let o = if *desc { o.reverse() } else { o };
                    o.then_with(|| cmp_bindings_with(b1, b2, rest, state, graph_matcher))
                }
            }
        }

        let Bindings { variables, iter } = self.select(inner, graph_matcher, None)?;
        let mut bindings = iter.collect::<Result<Vec<_>, _>>()?;
        bindings
            .sort_unstable_by(|b1, b2| cmp_bindings_with(b1, b2, &criteria, self, graph_matcher));
        let iter = Box::new(bindings.into_iter().map(Ok));
        Ok(Bindings { variables, iter })
    }

    fn project(
        self: &Arc<Self>,
        inner: &GraphPattern,
        variables: &[Variable],
        graph_matcher: &[Option<ArcTerm>],
        context: Option<&Binding>,
    ) -> Result<Bindings<'a, D>, SparqlWrapperError<D::Error>> {
        let variables: Vec<VarName<Arc<str>>> = variables
            .iter()
            .map(|v| self.stash_mut().copy_variable(v))
            .collect();
        let variables2 = variables.clone(); // for the closure
        let filtered_context = context.map(|b| b.clone().project(&variables));
        let Bindings { iter, .. } = self.select(inner, graph_matcher, filtered_context.as_ref())?;
        let iter = Box::new(iter.map(move |resb| resb.map(|b| b.project(&variables2))));
        Ok(Bindings { variables, iter })
    }

    fn slice(
        self: &Arc<Self>,
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

/// A utility iterator used by ExecState for Join and LeftJoin
enum MyIterator<T, E, I> {
    #[expect(dead_code)]
    FallBack(I, T),
    PassThrough(I),
    Err(E),
    Finish,
}

impl<T, E, I> Iterator for MyIterator<T, E, I>
where
    I: Iterator<Item = Result<T, E>>,
{
    type Item = Result<T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut tmp = MyIterator::Finish;
        std::mem::swap(self, &mut tmp);
        match tmp {
            MyIterator::FallBack(mut iter, fallback) => {
                let ret = iter.next();
                if ret.is_some() {
                    *self = MyIterator::PassThrough(iter);
                    ret
                } else {
                    Some(Ok(fallback))
                }
            }
            MyIterator::PassThrough(mut iter) => {
                let ret = iter.next();
                if ret.is_some() {
                    *self = MyIterator::PassThrough(iter);
                }
                ret
            }
            MyIterator::Err(err) => Some(Err(err)),
            MyIterator::Finish => None,
        }
    }
}
