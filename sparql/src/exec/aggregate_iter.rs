use std::{
    collections::HashSet,
    sync::{Arc, MutexGuard},
};

use sophia_api::prelude::Dataset;
use sophia_term::ArcStrStash;
use spargebra::algebra::{AggregateExpression, AggregateFunction};

use crate::{
    BindingMap, ResultTerm,
    binding::Binding,
    exec::ExecState,
    expression::{ArcExpression, EvalResult},
    graph_matcher::GraphMatcher,
    value::{SparqlNumber, SparqlValue},
};

pub struct AggregateIter<'a, D: ?Sized, I> {
    state: Arc<ExecState<'a, D>>,
    graph_matcher: GraphMatcher,
    group_variables: Vec<Arc<str>>,
    inner: I,
    aggregators: Vec<(Arc<str>, Aggregator)>,
}

impl<'a, D: ?Sized, I> AggregateIter<'a, D, I> {
    pub fn new(
        state: Arc<ExecState<'a, D>>,
        graph_matcher: GraphMatcher,
        group_variables: Vec<Arc<str>>,
        inner: I,
        aggregators: Vec<(Arc<str>, Aggregator)>,
    ) -> Self {
        Self {
            state,
            graph_matcher,
            group_variables,
            inner,
            aggregators,
        }
    }
}

impl<'a, D: Dataset + ?Sized, I> Iterator for AggregateIter<'a, D, I>
where
    I: Iterator<Item = (Vec<Option<ResultTerm>>, Vec<BindingMap>)>,
{
    type Item = Binding;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(group_values, vbindings)| {
            let mut res: BindingMap = self
                .group_variables
                .iter()
                .cloned()
                .zip(group_values)
                .filter_map(|(key, opt)| opt.map(|val| (key, val)))
                .collect();
            for vb in vbindings {
                for (_, agg) in &mut self.aggregators {
                    agg.push(&vb, &self.state, &self.graph_matcher);
                }
            }
            res.extend(
                self.aggregators
                    .iter_mut()
                    .map(|(var, agg)| (var.clone(), agg.finalize()))
                    .filter_map(|(key, opt)| opt.map(|val| (key, val))),
            );
            Binding {
                v: res,
                b: Default::default(),
            }
        })
    }
}

pub enum Aggregator {
    CountSolutions {
        #[expect(clippy::type_complexity)]
        seen: Option<HashSet<Vec<(Arc<str>, ResultTerm)>>>,
        count: u32,
    },
    Count {
        expr: ArcExpression,
        seen: Option<HashSet<ResultTerm>>,
        count: u32,
    },
    Sum {
        expr: ArcExpression,
        seen: Option<HashSet<ResultTerm>>,
        sum: Option<SparqlNumber>,
    },
    Avg {
        expr: ArcExpression,
        seen: Option<HashSet<ResultTerm>>,
        sum: Option<SparqlNumber>,
        count: u32,
    },
    Min {
        expr: ArcExpression,
        min: Option<EvalResult>,
        err: bool,
    },
    Max {
        expr: ArcExpression,
        max: Option<EvalResult>,
        err: bool,
    },
    GroupConcat {
        separator: String,
        expr: ArcExpression,
        seen: Option<HashSet<ResultTerm>>,
        first: bool,
        concat: Option<String>,
    },
    Sample {
        expr: ArcExpression,
        sample: Option<EvalResult>,
    },
    Unsupported,
}

impl Aggregator {
    pub fn from_with(agg: &AggregateExpression, stash: &mut MutexGuard<ArcStrStash>) -> Self {
        use AggregateExpression::*;
        use AggregateFunction::*;

        match agg {
            CountSolutions { distinct } => Self::CountSolutions {
                seen: distinct.then(Default::default),
                count: 0,
            },
            FunctionCall {
                name: Count,
                expr,
                distinct,
            } => Self::Count {
                expr: ArcExpression::from_expr(expr, stash),
                seen: distinct.then(Default::default),
                count: 0,
            },
            FunctionCall {
                name: Sum,
                expr,
                distinct,
            } => Self::Sum {
                expr: ArcExpression::from_expr(expr, stash),
                seen: distinct.then(Default::default),
                sum: Some(0.into()),
            },
            FunctionCall {
                name: Avg,
                expr,
                distinct,
            } => Self::Avg {
                expr: ArcExpression::from_expr(expr, stash),
                seen: distinct.then(Default::default),
                sum: Some(0.into()),
                count: 0,
            },
            FunctionCall {
                name: Min, expr, ..
            } => Self::Min {
                expr: ArcExpression::from_expr(expr, stash),
                min: None,
                err: false,
            },
            FunctionCall {
                name: Max, expr, ..
            } => Self::Max {
                expr: ArcExpression::from_expr(expr, stash),
                max: None,
                err: false,
            },
            FunctionCall {
                name: GroupConcat { separator },
                expr,
                distinct,
            } => Self::GroupConcat {
                separator: separator.clone().unwrap_or_else(|| " ".into()),
                expr: ArcExpression::from_expr(expr, stash),
                seen: distinct.then(Default::default),
                first: true,
                concat: Some("".into()),
            },
            FunctionCall {
                name: Sample, expr, ..
            } => Self::Sample {
                expr: ArcExpression::from_expr(expr, stash),
                sample: None,
            },
            FunctionCall {
                name: Custom(_), ..
            } => Self::Unsupported,
        }
    }

    fn push<D>(
        &mut self,
        vbinding: &BindingMap,
        state: &Arc<ExecState<'_, D>>,
        graph_matcher: &GraphMatcher,
    ) where
        D: Dataset + ?Sized,
    {
        match self {
            Aggregator::CountSolutions { seen, count } => {
                let do_count = match seen {
                    None => true,
                    Some(seen) => {
                        let mut v: Vec<(Arc<str>, ResultTerm)> = vbinding
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect();
                        v.sort();
                        seen.insert(v)
                    }
                };
                if do_count {
                    *count += 1
                }
            }
            Aggregator::Count { expr, seen, count } => {
                if let Some(res) = expr.eval(vbinding, state, graph_matcher) {
                    let do_count = match seen {
                        None => true,
                        Some(seen) => seen.insert(res.into_term()),
                    };
                    if do_count {
                        *count += 1
                    }
                }
            }
            Aggregator::Sum { expr, seen, sum } => {
                if let Some(sumval) = &*sum {
                    if let Some(res) = expr.eval(vbinding, state, graph_matcher)
                        && let Some(n) = res.as_number("Sum")
                    {
                        let do_add = match seen {
                            None => true,
                            Some(seen) => seen.insert(res.clone().into_term()),
                        };
                        if do_add {
                            *sum = sumval + n;
                        }
                    } else {
                        *sum = None;
                    }
                }
            }
            Aggregator::Avg {
                expr,
                seen,
                sum,
                count,
            } => {
                if let Some(sumval) = &*sum {
                    if let Some(res) = expr.eval(vbinding, state, graph_matcher)
                        && let Some(n) = res.as_number("Sum")
                    {
                        let do_add = match seen {
                            None => true,
                            Some(seen) => seen.insert(res.clone().into_term()),
                        };
                        if do_add {
                            *sum = sumval + n;
                            *count += 1;
                        }
                    } else {
                        *sum = None;
                    }
                }
            }
            Aggregator::Min { expr, min, err } => {
                if !*err {
                    if let Some(res) = expr.eval(vbinding, state, graph_matcher) {
                        if min
                            .as_ref()
                            .filter(|minval| minval.sparql_order_by(Some(&res)).is_le())
                            .is_none()
                        {
                            // min is none (no previous value) or greater than res
                            *min = Some(res);
                        }
                    } else {
                        *err = true;
                        *min = None;
                    }
                }
            }
            Aggregator::Max { expr, max, err } => {
                if !*err {
                    if let Some(res) = expr.eval(vbinding, state, graph_matcher) {
                        if max
                            .as_ref()
                            .filter(|maxval| maxval.sparql_order_by(Some(&res)).is_ge())
                            .is_none()
                        {
                            // max is none (no previous value) or lower than res
                            *max = Some(res);
                        }
                    } else {
                        *err = true;
                        *max = None;
                    }
                }
            }
            Aggregator::GroupConcat {
                separator,
                expr,
                seen,
                concat,
                first,
            } => {
                if let Some(concatval) = concat {
                    if let Some(res) = expr.eval(vbinding, state, graph_matcher)
                        && let Some(txt) = res.as_string_lit("GroupConcat")
                    {
                        let do_add = match seen {
                            None => true,
                            Some(seen) => seen.insert(res.clone().into_term()),
                        };
                        if do_add {
                            if !*first {
                                concatval.push_str(separator);
                            }
                            concatval.push_str(txt.0);
                            *first = false;
                        }
                    } else {
                        *concat = None;
                    }
                }
            }
            Aggregator::Sample { expr, sample } => {
                if sample.is_none() {
                    *sample = expr.eval(vbinding, state, graph_matcher);
                }
            }
            Aggregator::Unsupported => {}
        }
    }

    /// Return the final value computed by this aggregator,
    /// and re-initialize the aggregator.
    ///
    /// The aggregator is then ready for computing a new value.
    fn finalize(&mut self) -> Option<ResultTerm> {
        match self {
            Aggregator::CountSolutions { seen, count } => {
                let ret = Some(SparqlNumber::from(*count).into());
                if let Some(seen) = seen {
                    seen.clear();
                }
                *count = 0;
                ret
            }
            Aggregator::Count { seen, count, .. } => {
                let ret = Some(SparqlNumber::from(*count).into());
                if let Some(seen) = seen {
                    seen.clear();
                }
                *count = 0;
                ret
            }
            Aggregator::Sum { seen, sum, .. } => {
                let ret = sum.take().map(ResultTerm::from);
                if let Some(seen) = seen {
                    seen.clear();
                }
                *sum = Some(0.into());
                ret
            }
            Aggregator::Avg {
                seen, sum, count, ..
            } => {
                let ret = sum
                    .take()
                    .and_then(|sum| &sum / &SparqlNumber::from((*count).max(1)))
                    .map(ResultTerm::from);
                if let Some(seen) = seen {
                    seen.clear();
                }
                *sum = Some(0.into());
                *count = 0;
                ret
            }
            Aggregator::Min { min, err, .. } => {
                let ret = min.take().map(ResultTerm::from);
                *err = false;
                ret
            }
            Aggregator::Max { max, err, .. } => {
                let ret = max.take().map(ResultTerm::from);
                *err = false;
                ret
            }
            Aggregator::GroupConcat {
                seen,
                concat,
                first,
                ..
            } => {
                let ret = concat.take().map(|concat| SparqlValue::from(concat).into());
                if let Some(seen) = seen {
                    seen.clear();
                }
                *first = true;
                *concat = Some("".into());
                ret
            }
            Aggregator::Sample { sample, .. } => sample.take().map(ResultTerm::from),
            Aggregator::Unsupported => None,
        }
    }
}
