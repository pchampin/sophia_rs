#![allow(clippy::module_name_repetitions)]

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::MutexGuard;

use resiter::FlatMap;
use resiter::Map;
use sophia_api::dataset::DResult;
use sophia_api::prelude::*;
use sophia_api::term::VarName;
use sophia_api::term::matcher::Not;
use sophia_api::term::matcher::TermMatcher;
use sophia_iri::resolve::BaseIri;
use sophia_term::ArcStrStash;
use sophia_term::ArcTerm;
use spargebra::algebra::Expression;
use spargebra::algebra::GraphPattern;
use spargebra::algebra::OrderExpression;
use spargebra::algebra::PropertyPathExpression;
use spargebra::algebra::QueryDataset;
use spargebra::term::GroundTerm;
use spargebra::term::NamedNodePattern;
use spargebra::term::TermPattern;
use spargebra::term::TriplePattern;
use spargebra::term::Variable;

use crate::ResultTerm;
use crate::SparqlWrapperError;
use crate::bgp;
use crate::binding::BindingsIter;
use crate::binding::collect_variables;
use crate::binding::populate_bindings_arcterm;
use crate::binding::{Binding, Bindings, populate_variables};
use crate::expression::ArcExpression;
use crate::matcher::SparqlMatcher;
use crate::stash::ArcStrStashExt;

mod join_iter;
mod left_join_iter;
mod path_or_more;

#[derive(Debug)]
pub struct ExecState<'a, D: ?Sized> {
    stash: Mutex<ArcStrStash>,
    config: ExecConfig<'a, D>,
}

#[derive(Clone, Debug)]
pub struct ExecConfig<'a, D: ?Sized> {
    pub dataset: &'a D,
    pub default_matcher: Arc<[Option<ArcTerm>]>,
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
            None => Arc::from([None]),
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
        let config = ExecConfig {
            dataset,
            default_matcher,
            base_iri,
            now,
        };
        Ok(Arc::new(ExecState { stash, config }))
    }

    pub fn config(&self) -> &ExecConfig<'a, D> {
        &self.config
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
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        use GraphPattern::*;
        #[expect(unused_variables)]
        match pattern {
            Bgp { patterns } => self.bgp(patterns, graph_matcher, context),
            Path {
                subject,
                path,
                object,
            } => self.path(subject, path, object, graph_matcher, context),
            Join { left, right } => self.join(left, right, graph_matcher, context),
            LeftJoin {
                left,
                right,
                expression,
            } => self.left_join(left, right, expression, graph_matcher),
            Filter { expr, inner } => self.filter(expr, inner, graph_matcher),
            Union { left, right } => self.union(left, right, graph_matcher, context),
            Graph { name, inner } => self.graph(name, inner, context),
            Extend {
                inner,
                variable,
                expression,
            } => self.extend(inner, variable, expression, graph_matcher),
            Minus { left, right } => self.minus(left, right, graph_matcher, context),
            Values {
                variables,
                bindings,
            } => self.values(variables, bindings, context),
            OrderBy { inner, expression } => self.order_by(inner, expression, graph_matcher),
            Project { inner, variables } => self.project(inner, variables, graph_matcher, context),
            Distinct { inner } => self.distinct(inner, graph_matcher, context),
            Reduced { inner } => self.reduced(inner, graph_matcher, context),
            Slice {
                inner,
                start,
                length,
            } => self.slice(inner, *start, *length, graph_matcher, context),
            Group {
                inner,
                variables,
                aggregates,
            } => Bindings::err(SparqlWrapperError::NotImplemented("Group")),
            Service {
                name,
                inner,
                silent,
            } => Bindings::err(SparqlWrapperError::NotImplemented("Service")),
        }
    }

    pub fn ask(
        self: &Arc<Self>,
        pattern: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
    ) -> Result<bool, SparqlWrapperError<D::Error>> {
        match self.select(pattern, graph_matcher, None).iter.next() {
            None => Ok(false),
            Some(Ok(_)) => Ok(true),
            Some(Err(err)) => Err(err),
        }
    }

    /// Evaluates `patterns` on the active graph identified by `graph_matcher`.
    ///
    /// The hint `context` is used by this method;
    /// the returned bindings are all compatible with it, and include it.
    fn bgp(
        self: &Arc<Self>,
        patterns: &[TriplePattern],
        graph_matcher: &Arc<[Option<ArcTerm>]>,
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

    fn path(
        self: &Arc<Self>,
        subject: &TermPattern,
        path: &PropertyPathExpression,
        object: &TermPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        static EMPTY: LazyLock<Binding> = LazyLock::new(Binding::default);
        // self (as state), subject and object will be moved in a closure;
        // they must be cloned to avoid leaking the lifetime of `self`
        let state = self.clone();
        let subject2 = subject.clone();
        let object2 = object.clone();

        let mut variables = HashSet::new();
        collect_variables(subject, &mut variables, &mut self.stash_mut());
        collect_variables(object, &mut variables, &mut self.stash_mut());
        let variables: Vec<_> = variables.into_iter().collect();

        let context = context.unwrap_or(&EMPTY);
        let smatcher = SparqlMatcher::build(subject.into(), context, &mut self.stash_mut());
        let omatcher = SparqlMatcher::build(object.into(), context, &mut self.stash_mut());

        let iter = Box::new(
            self.path_rec(smatcher, path, omatcher, graph_matcher)
                .filter_map(move |res| {
                    res.map(|[s, o]| {
                        let mut b = Binding::default();
                        let compatible = populate_bindings_arcterm(
                            (&subject2).into(),
                            s,
                            &mut b,
                            &mut state.stash_mut(),
                        ) && populate_bindings_arcterm(
                            (&object2).into(),
                            o,
                            &mut b,
                            &mut state.stash_mut(),
                        );
                        compatible.then_some(b)
                    })
                    .map_err(SparqlWrapperError::Dataset)
                    .transpose()
                }),
        );
        Bindings { variables, iter }
    }

    fn path_rec(
        self: &Arc<Self>,
        smatcher: SparqlMatcher,
        path: &PropertyPathExpression,
        omatcher: SparqlMatcher,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
    ) -> Box<dyn Iterator<Item = DResult<D, [ArcTerm; 2]>> + 'a> {
        use PropertyPathExpression::*;
        match path {
            NamedNode(predicate) => {
                // self (as state) will be moved in a closure;
                // it must be cloned to avoid leaking the lifetime of `self`
                let state = self.clone();
                let predicate =
                    IriRef::new_unchecked(self.stash_mut().copy_str(predicate.as_str()));
                Box::new(
                    state
                        .config()
                        .dataset
                        .quads_matching(smatcher, [predicate], omatcher, graph_matcher.to_vec())
                        .map_ok(move |q| {
                            let mut stash = state.stash_mut();
                            [stash.copy_term(q.s()), stash.copy_term(q.o())]
                        }),
                )
            }
            Reverse(path) => Box::new(
                self.path_rec(omatcher, path, smatcher, graph_matcher)
                    .map_ok(|[o, s]| [s, o]),
            ),
            Sequence(path1, path2) => {
                // self (as state), graph_matcher (as g_matcher) and path2 will be moved in a closure;
                // they must be cloned to avoid leaking the lifetime of `self`
                let state = self.clone();
                let gmatcher = graph_matcher.clone();
                let path2 = path2.clone();
                Box::new(
                    self.path_rec(smatcher, path1, SparqlMatcher::Free, graph_matcher)
                        .flat_map_ok(move |[s, i]| {
                            state
                                .path_rec(
                                    SparqlMatcher::Bound(i.into()),
                                    &path2,
                                    omatcher.clone(),
                                    &gmatcher,
                                )
                                .map_ok(move |[_, o]| [s.clone(), o])
                        })
                        .map(Result::unwrap),
                )
            }
            Alternative(path1, path2) => Box::new(
                self.path_rec(smatcher.clone(), path1, omatcher.clone(), graph_matcher)
                    .chain(self.path_rec(smatcher, path2, omatcher, graph_matcher)),
            ),
            ZeroOrMore(path) => {
                // self (as state), graph_matcher (as g_matcher) and path will be moved in a closure;
                // they must be cloned to avoid leaking the lifetime of `self`
                let state = self.clone();
                let gmatcher = graph_matcher.clone();
                let path = PropertyPathExpression::clone(path);

                Box::new(
                    self.path_zero(smatcher, graph_matcher)
                        .flat_map_ok(move |node| {
                            path_or_more::PathOrMore::new(
                                state.clone(),
                                node.clone(),
                                node,
                                path.clone(),
                                omatcher.clone(),
                                gmatcher.clone(),
                            )
                        })
                        .map(Result::flatten),
                )
            }
            OneOrMore(path) => {
                // self (as state), graph_matcher (as g_matcher) and path will be moved in a closure;
                // they must be cloned to avoid leaking the lifetime of `self`
                let state = self.clone();
                let gmatcher = graph_matcher.clone();
                let path = PropertyPathExpression::clone(path);
                Box::new(
                    self.path_rec(smatcher, &path, SparqlMatcher::Free, graph_matcher)
                        .flat_map_ok(move |[s, o]| {
                            path_or_more::PathOrMore::new(
                                state.clone(),
                                s,
                                o,
                                path.clone(),
                                omatcher.clone(),
                                gmatcher.clone(),
                            )
                        })
                        .map(Result::flatten),
                )
            }
            ZeroOrOne(path) => {
                // self (as state), graph_matcher (as g_matcher) and path will be moved in a closure;
                // they must be cloned to avoid leaking the lifetime of `self`
                let state = self.clone();
                let gmatcher = graph_matcher.clone();
                let path = PropertyPathExpression::clone(path);

                Box::new(
                    self.path_zero(smatcher, graph_matcher)
                        .flat_map_ok(move |t| {
                            let iter_zero = omatcher
                                .matches(&t)
                                .then(|| Ok([t.clone(), t.clone()]))
                                .into_iter();
                            let iter_one = state.path_rec(
                                SparqlMatcher::Bound(t.into()),
                                &path,
                                omatcher.clone(),
                                &gmatcher.clone(),
                            );
                            iter_zero.chain(iter_one)
                        })
                        .map(Result::flatten),
                )
            }
            NegatedPropertySet(predicates) => {
                // self (as state) will be moved in a closure;
                // it must be cloned to avoid leaking the lifetime of `self`
                let state = self.clone();
                let predicates: Vec<_> = predicates
                    .iter()
                    .map(|iri| IriRef::new_unchecked(self.stash_mut().copy_str(iri.as_str())))
                    .collect();
                Box::new(
                    state
                        .config()
                        .dataset
                        .quads_matching(smatcher, Not(predicates), omatcher, graph_matcher.to_vec())
                        .map_ok(move |q| {
                            let mut stash = state.stash_mut();
                            [stash.copy_term(q.s()), stash.copy_term(q.o())]
                        }),
                )
            }
        }
    }

    /// Iter subjects for property paths 'ZeroOrMOre' and 'ZeroOrOne'
    fn path_zero(
        self: &Arc<Self>,
        smatcher: SparqlMatcher,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
    ) -> Box<dyn Iterator<Item = DResult<D, ArcTerm>>> {
        if let SparqlMatcher::Bound(t) = smatcher {
            Box::new(std::iter::once(Ok(t.unwrap())))
        } else {
            let active_graph = self
                .config()
                .dataset
                .partial_union_graph(&graph_matcher[..]);
            let nodes: Result<HashSet<_>, _> = active_graph
                .subjects_matching(smatcher.clone())
                .chain(active_graph.objects_matching(smatcher))
                .map_ok(|n| self.stash_mut().copy_term(n))
                .collect();
            match nodes {
                Err(err) => Box::new(std::iter::once(Err(err))),
                Ok(nodes) => Box::new(nodes.into_iter().map(Ok)),
            }
        }
    }

    fn join(
        self: &Arc<Self>,
        left: &GraphPattern,
        right: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let (variables, iter1, first1, iter2) =
            self.prepare_join(left, right, graph_matcher, context);
        let first1 = match first1 {
            None => return Bindings::empty_with(variables),
            Some(Err(err)) => return Bindings::err_with(err, variables),
            Some(Ok(first1)) => first1,
        };
        // right and graph_matcher will be moved in the closure;
        // note that they must be cloned, so that we don't "leak" the lifetime of `self` in the return value
        let state = Arc::clone(self);
        let right = right.clone();
        let graph_matcher = graph_matcher.clone();

        let do_join = |b1, b2s: BindingsIter<'a, D>| {
            b2s.filter_map(move |resb| resb.map(|b2| b2.merge_if_compatible(Some(&b1))).transpose())
        };

        let iter = Box::new(do_join(first1, iter2).chain(iter1.flat_map(
            move |resb1| match resb1 {
                Ok(b1) => {
                    let b2s = state.select(&right, &graph_matcher, Some(&b1));
                    join_iter::JoinIter::PassThrough(do_join(b1, b2s.iter))
                }
                Err(err) => join_iter::JoinIter::Err(err),
            },
        )));
        Bindings { variables, iter }
    }

    fn left_join(
        self: &Arc<Self>,
        left: &GraphPattern,
        right: &GraphPattern,
        expression: &Option<Expression>,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
    ) -> Bindings<'a, D> {
        let (variables, iter1, first1, iter2) = self.prepare_join(left, right, graph_matcher, None);
        let first1 = match first1 {
            None => return Bindings::empty_with(variables),
            Some(Err(err)) => return Bindings::err_with(err, variables),
            Some(Ok(first1)) => first1,
        };
        let iter = Box::new(left_join_iter::LeftJoinIter::new(
            iter1,
            first1,
            iter2,
            self,
            right,
            expression,
            graph_matcher.clone(),
        ));
        Bindings { variables, iter }
    }

    fn distinct(
        self: &Arc<Self>,
        inner: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, context);
        #[allow(clippy::mutable_key_type)]
        // ResultTerm appears to have interior mutability, but that does not change their hash
        let mut seen = HashSet::new();
        let iter = Box::new(iter.filter(move |resb| match resb {
            Err(_) => true,
            Ok(b) => {
                let hashable: Vec<_> = b.v.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                seen.insert(hashable)
            }
        }));
        Bindings { variables, iter }
    }

    fn reduced(
        self: &Arc<Self>,
        inner: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, context);
        let mut seen = None;
        let iter = Box::new(iter.filter(move |resb| match resb {
            Err(_) => true,
            Ok(b) => {
                if seen.as_ref() == Some(&b.v) {
                    false
                } else {
                    seen = Some(b.v.clone());
                    true
                }
            }
        }));
        Bindings { variables, iter }
    }

    fn filter(
        self: &Arc<Self>,
        expression: &Expression,
        inner: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
    ) -> Bindings<'a, D> {
        let Bindings { variables, iter } = self.select(inner, graph_matcher, None);
        let arc_expr = ArcExpression::from_expr(expression, &mut self.stash_mut());
        // self (as state) and graph_matcher will be moved in the closure;
        // they must be cloned to avoid leaking the lifetime of `self`
        let state = self.clone();
        let graph_matcher = graph_matcher.clone();
        let iter = Box::new(iter.filter(move |resb| match resb {
            Err(_) => true,
            Ok(b) => arc_expr.eval_truthy(b, &state, &graph_matcher),
        }));
        Bindings { variables, iter }
    }

    fn union(
        self: &Arc<Self>,
        left: &GraphPattern,
        right: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let Bindings {
            variables: lv,
            iter: li,
        } = self.select(left, graph_matcher, context);
        let Bindings {
            variables: rv,
            iter: ri,
        } = self.select(right, graph_matcher, context);
        let mut variables = lv.clone();
        for v in rv {
            if lv.iter().all(|i| *i != v) {
                variables.push(v)
            }
        }
        let iter = Box::new(li.chain(ri));
        Bindings { variables, iter }
    }

    fn graph(
        self: &Arc<Self>,
        name: &NamedNodePattern,
        inner: &GraphPattern,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        match name {
            NamedNodePattern::NamedNode(nn) => {
                let graph_matcher = Arc::from([Some(ArcTerm::Iri(IriRef::new_unchecked(
                    self.stash_mut().copy_str(nn.as_str()),
                )))]);
                self.select(inner, &graph_matcher, context)
            }
            NamedNodePattern::Variable(var) => {
                if let Some(name) = context.and_then(|b| b.v.get(var.as_str())) {
                    let graph_matcher = Arc::from([Some(name.inner().clone())]);
                    self.select(inner, &graph_matcher, context)
                } else {
                    let res = self
                        .config()
                        .dataset
                        .graph_names()
                        .map(|res| res.map(|t| self.stash_mut().copy_term(t)))
                        .collect::<Result<BTreeSet<_>, _>>()
                        .map_err(SparqlWrapperError::Dataset);
                    match res {
                        Err(err) => Bindings::err(err),
                        Ok(graph_names) if graph_names.is_empty() => Bindings::empty(),
                        Ok(graph_names) => {
                            self.graph_rec(var.as_str(), graph_names.into_iter(), inner, context)
                        }
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
    ) -> Bindings<'a, D> {
        if let Some(name) = graph_names.next() {
            let mut b = context.cloned().unwrap_or_else(Binding::default);
            b.v.insert(self.stash_mut().copy_str(var), name.clone().into());
            let graph_matcher = Arc::from([Some(name)]);
            let Bindings { variables, iter } = self.select(inner, &graph_matcher, Some(&b));
            let iter = Box::new(iter.chain(self.graph_rec(var, graph_names, inner, context).iter));
            Bindings { variables, iter }
        } else {
            Bindings::empty()
        }
    }

    fn extend(
        self: &Arc<Self>,
        inner: &GraphPattern,
        variable: &Variable,
        expression: &Expression,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
    ) -> Bindings<'a, D> {
        let variable = self.stash_mut().copy_variable(variable);
        let Bindings {
            mut variables,
            iter,
        } = self.select(inner, graph_matcher, None);
        if variables.contains(&variable) {
            return Bindings::err_with(SparqlWrapperError::Override(variable.unwrap()), variables);
        }
        let arc_expr = ArcExpression::from_expr(expression, &mut self.stash_mut());
        variables.push(variable.clone());
        // self (as state) and graph_matcher will be moved in the closure;
        // they must be cloned to avoid leaking the lifetime of `self`
        let state = Arc::clone(self);
        let varkey = variable.unwrap();
        let graph_matcher = graph_matcher.clone();

        let iter = Box::new(iter.map(move |resb| {
            resb.map(|mut b| {
                if let Some(val) = arc_expr.eval(&b, &state, &graph_matcher) {
                    b.v.insert(varkey.clone(), val.into_term());
                }
                b
            })
        }));
        Bindings { variables, iter }
    }

    fn minus(
        self: &Arc<Self>,
        left: &GraphPattern,
        right: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let Bindings {
            variables,
            iter: iter1,
        } = self.select(left, graph_matcher, context);

        // right and graph_matcher will be moved in the closure;
        // note that they must be cloned, so that we don't "leak" the lifetime of `self` in the return value
        let state = Arc::clone(self);
        let right = right.clone();
        let graph_matcher = graph_matcher.clone();

        let iter = Box::new(iter1.filter_map(move |rb1| match rb1 {
            Err(err) => Some(Err(err)),
            Ok(b1) => {
                let bs2 = state.select(&right, &graph_matcher, None);
                for rb2 in bs2.iter {
                    match rb2 {
                        Err(err) => return Some(Err(err)),
                        Ok(b2) => {
                            if b1.compatible(&b2).unwrap_or(false) {
                                return None;
                            }
                        }
                    }
                }
                Some(Ok(b1))
            }
        }));
        Bindings { variables, iter }
    }

    fn values(
        self: &Arc<Self>,
        variables: &[Variable],
        bindings: &[Vec<Option<GroundTerm>>],
        _context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let variables: Vec<VarName<Arc<str>>> = variables
            .iter()
            .map(|v| VarName::new_unchecked(self.stash_mut().copy_str(v.as_str())))
            .collect();
        let bindings: Vec<Vec<Option<ResultTerm>>> = bindings
            .iter()
            .map(|v| {
                v.iter()
                    .map(|opt| {
                        opt.as_ref()
                            .map(|gterm| self.stash_mut().copy_ground_term(gterm))
                    })
                    .collect()
            })
            .collect();
        let vars = variables.clone();
        let iter = Box::new(bindings.into_iter().map(move |bs| {
            let v: HashMap<Arc<str>, ResultTerm> = vars
                .iter()
                .zip(bs)
                .filter_map(|(var, opt)| opt.map(|rterm| (var.clone().unwrap(), rterm)))
                .collect();
            let b = HashMap::default();
            Ok(Binding { v, b })
        }));
        Bindings { variables, iter }
    }

    fn order_by(
        self: &Arc<Self>,
        inner: &GraphPattern,
        expression: &[OrderExpression],
        graph_matcher: &Arc<[Option<ArcTerm>]>,
    ) -> Bindings<'a, D> {
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
            graph_matcher: &Arc<[Option<ArcTerm>]>,
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

        let Bindings { variables, iter } = self.select(inner, graph_matcher, None);
        let res = iter.collect::<Result<Vec<_>, _>>();
        match res {
            Err(err) => Bindings::err_with(err, variables),
            Ok(mut bindings) => {
                bindings.sort_unstable_by(|b1, b2| {
                    cmp_bindings_with(b1, b2, &criteria, self, graph_matcher)
                });
                let iter = Box::new(bindings.into_iter().map(Ok));
                Bindings { variables, iter }
            }
        }
    }

    fn project(
        self: &Arc<Self>,
        inner: &GraphPattern,
        variables: &[Variable],
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let variables: Vec<VarName<Arc<str>>> = variables
            .iter()
            .map(|v| self.stash_mut().copy_variable(v))
            .collect();
        let variables2 = variables.clone(); // for the closure
        let filtered_context = context.map(|b| b.clone().project(&variables));
        let Bindings { iter, .. } = self.select(inner, graph_matcher, filtered_context.as_ref());
        let iter = Box::new(iter.map(move |resb| resb.map(|b| b.project(&variables2))));
        Bindings { variables, iter }
    }

    fn slice(
        self: &Arc<Self>,
        inner: &GraphPattern,
        start: usize,
        length: Option<usize>,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> Bindings<'a, D> {
        let mut bindings = self.select(inner, graph_matcher, context);
        if start > 0 {
            // skip the first 'start' items, *unless* they are errors
            bindings.iter = Box::new(bindings.iter.enumerate().filter_map(move |(i, res)| {
                if i < start {
                    res.is_err().then_some(res)
                } else {
                    Some(res)
                }
            }));
        }
        if let Some(n) = length {
            bindings.iter = Box::new(bindings.iter.take(n));
        }
        bindings
    }

    #[expect(clippy::type_complexity)]
    fn prepare_join(
        self: &Arc<Self>,
        left: &GraphPattern,
        right: &GraphPattern,
        graph_matcher: &Arc<[Option<ArcTerm>]>,
        context: Option<&Binding>,
    ) -> (
        Vec<VarName<Arc<str>>>,
        BindingsIter<'a, D>,
        Option<Result<Binding, SparqlWrapperError<D::Error>>>,
        BindingsIter<'a, D>,
    ) {
        let Bindings {
            mut variables,
            iter: mut iter1,
        } = self.select(left, graph_matcher, context);

        let first1 = iter1.next();
        let context = first1.as_ref().and_then(|res| res.as_ref().ok());

        let Bindings {
            iter: iter2,
            variables: vars2,
        } = self.select(right, graph_matcher, context);

        for v in vars2 {
            if !variables.contains(&v) {
                variables.push(v);
            }
        }

        (variables, iter1, first1, iter2)
    }
}
