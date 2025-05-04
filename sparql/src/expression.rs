//! An `ArcTerm` version of `spargebra::Expression`
use sophia_api::{
    dataset::Dataset,
    ns::xsd,
    term::{BnodeId, LanguageTag, Term, VarName},
};
use sophia_iri::IriRef;
use sophia_term::{ArcStrStash, ArcTerm, GenericLiteral};
use spargebra::algebra::{Expression, Function, GraphPattern};

use std::{cmp::Ordering, sync::Arc};

use crate::{
    ResultTerm,
    binding::Binding,
    exec::{ExecConfig, ExecState},
    function::call_function,
    stash::{ArcStrStashExt, value_ref_to_arcterm, value_to_term},
    value::{SparqlNumber, SparqlValue, XsdDateTime},
};

/// An [expression](https://www.w3.org/TR/sparql11-query/#expressions).
#[derive(Debug, Clone)]
pub(crate) enum ArcExpression {
    NamedNode(IriRef<Arc<str>>),
    Literal(GenericLiteral<Arc<str>>),
    Variable(VarName<Arc<str>>),
    /// [Logical-or](https://www.w3.org/TR/sparql11-query/#func-logical-or).
    Or(Box<Self>, Box<Self>),
    /// [Logical-and](https://www.w3.org/TR/sparql11-query/#func-logical-and).
    And(Box<Self>, Box<Self>),
    /// [RDFterm-equal](https://www.w3.org/TR/sparql11-query/#func-RDFterm-equal) and all the XSD equalities.
    Equal(Box<Self>, Box<Self>),
    /// [sameTerm](https://www.w3.org/TR/sparql11-query/#func-sameTerm).
    SameTerm(Box<Self>, Box<Self>),
    /// [op:numeric-greater-than](https://www.w3.org/TR/xpath-functions-31/#func-numeric-greater-than) and other XSD greater than operators.
    Greater(Box<Self>, Box<Self>),
    GreaterOrEqual(Box<Self>, Box<Self>),
    /// [op:numeric-less-than](https://www.w3.org/TR/xpath-functions-31/#func-numeric-less-than) and other XSD greater than operators.
    Less(Box<Self>, Box<Self>),
    LessOrEqual(Box<Self>, Box<Self>),
    /// [IN](https://www.w3.org/TR/sparql11-query/#func-in)
    In(Box<Self>, Vec<Self>),
    /// [op:numeric-add](https://www.w3.org/TR/xpath-functions-31/#func-numeric-add) and other XSD additions.
    Add(Box<Self>, Box<Self>),
    /// [op:numeric-subtract](https://www.w3.org/TR/xpath-functions-31/#func-numeric-subtract) and other XSD subtractions.
    Subtract(Box<Self>, Box<Self>),
    /// [op:numeric-multiply](https://www.w3.org/TR/xpath-functions-31/#func-numeric-multiply) and other XSD multiplications.
    Multiply(Box<Self>, Box<Self>),
    /// [op:numeric-divide](https://www.w3.org/TR/xpath-functions-31/#func-numeric-divide) and other XSD divides.
    Divide(Box<Self>, Box<Self>),
    /// [op:numeric-unary-plus](https://www.w3.org/TR/xpath-functions-31/#func-numeric-unary-plus) and other XSD unary plus.
    UnaryPlus(Box<Self>),
    /// [op:numeric-unary-minus](https://www.w3.org/TR/xpath-functions-31/#func-numeric-unary-minus) and other XSD unary minus.
    UnaryMinus(Box<Self>),
    /// [fn:not](https://www.w3.org/TR/xpath-functions-31/#func-not).
    Not(Box<Self>),
    /// [EXISTS](https://www.w3.org/TR/sparql11-query/#func-filter-exists).
    Exists(Box<GraphPattern>),
    /// [BOUND](https://www.w3.org/TR/sparql11-query/#func-bound).
    Bound(VarName<Arc<str>>),
    /// [IF](https://www.w3.org/TR/sparql11-query/#func-if).
    If(Box<Self>, Box<Self>, Box<Self>),
    /// [COALESCE](https://www.w3.org/TR/sparql11-query/#func-coalesce).
    Coalesce(Vec<Self>),
    /// A regular function call.
    FunctionCall(Function, Vec<Self>),
}

impl ArcExpression {
    pub(crate) fn from_expr(expr: &Expression, stash: &mut ArcStrStash) -> Self {
        use ArcExpression::*;
        match expr {
            Expression::NamedNode(iri) => {
                let iri = stash.copy_str(iri.as_str());
                NamedNode(IriRef::new_unchecked(iri))
            }
            Expression::Literal(lit) => Literal({
                let lex = stash.copy_str(lit.value());
                if let Some(tag) = lit.language() {
                    GenericLiteral::LanguageString(
                        lex,
                        stash.copy_language_tag(LanguageTag::new_unchecked(tag)),
                    )
                } else {
                    GenericLiteral::Typed(
                        lex,
                        stash.copy_iri(IriRef::new_unchecked(lit.datatype().as_str())),
                    )
                }
            }),
            Expression::Variable(v) => Variable(stash.copy_variable(v)),
            Expression::Or(lhs, rhs) => Or(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::And(lhs, rhs) => And(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Equal(lhs, rhs) => Equal(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::SameTerm(lhs, rhs) => SameTerm(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Greater(lhs, rhs) => Greater(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::GreaterOrEqual(lhs, rhs) => GreaterOrEqual(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Less(lhs, rhs) => Less(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::LessOrEqual(lhs, rhs) => LessOrEqual(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::In(lhs, rhs) => In(
                Box::new(Self::from_expr(lhs, stash)),
                rhs.iter()
                    .map(|e| ArcExpression::from_expr(e, stash))
                    .collect(),
            ),
            Expression::Add(lhs, rhs) => Add(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Subtract(lhs, rhs) => Subtract(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Multiply(lhs, rhs) => Multiply(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::Divide(lhs, rhs) => Divide(
                Box::new(Self::from_expr(lhs, stash)),
                Box::new(Self::from_expr(rhs, stash)),
            ),
            Expression::UnaryPlus(e) => UnaryPlus(Box::new(Self::from_expr(e, stash))),
            Expression::UnaryMinus(e) => UnaryMinus(Box::new(Self::from_expr(e, stash))),
            Expression::Not(e) => Not(Box::new(Self::from_expr(e, stash))),
            Expression::Exists(bpg) => Exists(bpg.clone()),
            Expression::Bound(v) => Bound(stash.copy_variable(v)),
            Expression::If(i, t, e) => If(
                Box::new(Self::from_expr(i, stash)),
                Box::new(Self::from_expr(t, stash)),
                Box::new(Self::from_expr(e, stash)),
            ),
            Expression::Coalesce(es) => Coalesce(
                es.iter()
                    .map(|e| ArcExpression::from_expr(e, stash))
                    .collect(),
            ),
            Expression::FunctionCall(func, args) => FunctionCall(
                func.clone(),
                args.iter()
                    .map(|e| ArcExpression::from_expr(e, stash))
                    .collect(),
            ),
        }
    }

    // NB: `config` is a reference to an Arc.
    // * why not just an Arc: to avoid the case of cloning it at each step of recursion;
    // * why not just a reference: because the Exists variant needs an Arc to build an ExecState.
    #[allow(clippy::too_many_lines)]
    pub fn eval<D>(
        &self,
        binding: &Binding,
        config: &Arc<ExecConfig<'_, D>>,
        graph_matcher: &[Option<ArcTerm>],
    ) -> Option<EvalResult>
    where
        D: Dataset + ?Sized,
    {
        use ArcExpression::*;
        match self {
            NamedNode(iri) => Some(ResultTerm::from_parts(ArcTerm::Iri(iri.clone()), None).into()),
            Literal(lit) => Some(ResultTerm::from(ArcTerm::Literal(lit.clone())).into()),
            Variable(var) => binding.v.get(var.as_str()).cloned().map(EvalResult::from),
            Or(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?.is_truthy();
                let rhs = rhs.eval(binding, config, graph_matcher)?.is_truthy();
                match (lhs, rhs) {
                    (Some(a), Some(b)) => Some(a || b),
                    (Some(true), None) | (None, Some(true)) => Some(true),
                    _ => None,
                }
                .map(EvalResult::from)
            }
            And(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?.is_truthy();
                let rhs = rhs.eval(binding, config, graph_matcher)?.is_truthy();
                match (lhs, rhs) {
                    (Some(a), Some(b)) => Some(a && b),
                    (Some(false), None) | (None, Some(false)) => Some(false),
                    _ => None,
                }
                .map(EvalResult::from)
            }
            Equal(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_eq(&rhs).map(EvalResult::from)
            }
            SameTerm(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?.into_term();
                let rhs = rhs.eval(binding, config, graph_matcher)?.into_term();
                Some(Term::eq(&lhs, rhs).into())
            }
            Greater(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_gt()))
            }
            GreaterOrEqual(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_ge()))
            }
            Less(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_lt()))
            }
            LessOrEqual(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                lhs.sparql_cmp(&rhs)
                    .map(|ord| EvalResult::from(ord.is_le()))
            }
            In(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                rhs.iter()
                    .map(|other| {
                        other
                            .eval(binding, config, graph_matcher)
                            .and_then(|other| lhs.sparql_eq(&other))
                    })
                    .find(|res| res != &Some(false))
                    .unwrap_or(Some(false))
                    .map(EvalResult::from)
                // I reproduce the behaviour of Jena:
                // the IN operator fails on the first error, even if a match exists further in the list.
                // Another reasonable behaviour would be to ignore errors, and return false if no element in rhs was equal to lhs.
                // TODO check what the spec says
            }
            Add(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number("Add#1")? + rhs.as_number("Add#2")?)
                    .map(|n| SparqlValue::from(n).into())
            }
            Subtract(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number("Subtract#1")? - rhs.as_number("Subtract#2")?)
                    .map(|n| SparqlValue::from(n).into())
            }
            Multiply(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number("Multiply#1")? * rhs.as_number("Multiply#2")?)
                    .map(|n| SparqlValue::from(n).into())
            }
            Divide(lhs, rhs) => {
                let lhs = lhs.eval(binding, config, graph_matcher)?;
                let rhs = rhs.eval(binding, config, graph_matcher)?;
                (lhs.as_number("Divide#1")? / rhs.as_number("Divide#2")?)
                    .map(|n| SparqlValue::from(n).into())
            }
            UnaryPlus(e) => {
                let e = e.eval(binding, config, graph_matcher)?;
                e.as_number("UnaryPlus")
                    .map(|n| SparqlValue::from(n.clone()).into())
            }
            UnaryMinus(e) => {
                let e = e.eval(binding, config, graph_matcher)?;
                (-e.as_number("UnaryMinus")?).map(|n| SparqlValue::from(n).into())
            }
            Not(e) => {
                let e = e.eval(binding, config, graph_matcher)?.is_truthy()?;
                Some((!e).into())
            }
            Exists(graph_pattern) => {
                let mut exec_state = ExecState::from(Arc::clone(config));
                let res = exec_state.select(graph_pattern, graph_matcher, Some(binding));
                let exists = match res {
                    Ok(mut bindings) => bindings.iter.next().is_some(),
                    Err(_) => false,
                };
                Some(exists.into())
            }
            Bound(varname) => Some(binding.v.contains_key(varname.as_str()).into()),
            If(c, t, e) => {
                if c.eval(binding, config, graph_matcher)?
                    .is_truthy()
                    .unwrap_or(false)
                {
                    t.eval(binding, config, graph_matcher)
                } else {
                    e.eval(binding, config, graph_matcher)
                }
            }
            Coalesce(exprs) => exprs
                .iter()
                .find_map(|e| e.eval(binding, config, graph_matcher)),
            FunctionCall(function, arguments) => {
                let evaluated: Vec<EvalResult> = arguments
                    .iter()
                    .map_while(|e| e.eval(binding, config, graph_matcher))
                    .collect();
                if evaluated.len() == arguments.len() {
                    call_function(function, evaluated, config.as_ref())
                } else {
                    None
                }
            }
        }
    }
}

//

#[derive(Clone, Debug)]
pub enum EvalResult {
    Term(ResultTerm),
    Value(SparqlValue),
}

impl EvalResult {
    pub fn as_term(&self) -> ArcTerm {
        match self {
            EvalResult::Term(t) => t.inner().clone(),
            EvalResult::Value(v) => value_ref_to_arcterm(v, |txt| Arc::from(txt)),
        }
    }

    pub fn as_value(&self) -> Option<&SparqlValue> {
        match self {
            EvalResult::Term(t) => t.value(),
            EvalResult::Value(v) => Some(v),
        }
    }

    pub fn as_iri(&self, diag: &str) -> Option<&IriRef<Arc<str>>> {
        let fail = || {
            if !diag.is_empty() {
                log::warn!("{diag} expects an IRI");
            }
            None
        };
        match self {
            EvalResult::Term(rt) => match rt.inner() {
                ArcTerm::Iri(iri) => Some(iri),
                _ => fail(),
            },
            EvalResult::Value(_) => fail(),
        }
    }

    pub fn as_literal(&self, diag: &str) -> Option<GenericLiteral<Arc<str>>> {
        match self.as_term() {
            ArcTerm::Literal(lit) => Some(lit.clone()),
            _ => {
                if !diag.is_empty() {
                    log::warn!("{diag} expects a literal");
                }
                None
            }
        }
    }

    pub fn as_triple(&self, diag: &str) -> Option<Arc<[ArcTerm; 3]>> {
        match self.as_term() {
            ArcTerm::Triple(triple) => Some(triple),
            _ => {
                if !diag.is_empty() {
                    log::warn!("{diag} expects a triple term");
                }
                None
            }
        }
    }

    pub fn as_number(&self, diag: &str) -> Option<&SparqlNumber> {
        match self.as_value() {
            Some(SparqlValue::Number(n)) => Some(n),
            _ => {
                if !diag.is_empty() {
                    log::warn!("{diag} expects a number");
                }
                None
            }
        }
    }

    /// Coerce to [string literal](https://www.w3.org/TR/sparql11-query/#func-string)
    #[expect(clippy::type_complexity)]
    pub fn as_string_lit(&self, diag: &str) -> Option<(&Arc<str>, Option<&LanguageTag<Arc<str>>>)> {
        let fail = || {
            if !diag.is_empty() {
                log::warn!("{diag} expects a string literal");
            }
            None
        };
        use GenericLiteral::*;
        match self {
            EvalResult::Term(t) => match t.inner() {
                ArcTerm::Literal(LanguageString(lex, tag)) => Some((lex, Some(tag))),
                ArcTerm::Literal(Typed(lex, dt)) if xsd::string == dt => Some((lex, None)),
                _ => fail(),
            },
            EvalResult::Value(SparqlValue::String(lex, tag)) => Some((lex, tag.as_ref())),
            EvalResult::Value(_) => fail(),
        }
    }

    /// Coerce to an xsd:string literal
    pub fn as_xsd_string(&self, diag: &str) -> Option<&Arc<str>> {
        let fail = || {
            if !diag.is_empty() {
                log::warn!("{diag} expects an xsd:string");
            }
            None
        };
        use GenericLiteral::*;
        match self {
            EvalResult::Term(t) => match t.inner() {
                ArcTerm::Literal(Typed(lex, dt)) if xsd::string == dt => Some(lex),
                _ => fail(),
            },
            EvalResult::Value(SparqlValue::String(lex, None)) => Some(lex),
            EvalResult::Value(_) => fail(),
        }
    }

    /// Coerce to an xsd:dateTime literal
    pub fn as_xsd_date_time(&self, diag: &str) -> Option<&XsdDateTime> {
        match self.as_value() {
            Some(SparqlValue::DateTime(dt)) => dt.as_ref(),
            _ => {
                if !diag.is_empty() {
                    log::warn!("{diag} expects an xsd:dateTime");
                }
                None
            }
        }
    }

    pub fn into_term(self) -> ResultTerm {
        match self {
            EvalResult::Term(t) => t,
            EvalResult::Value(v) => value_to_term(v, |txt| Arc::from(txt)),
        }
    }

    pub fn is_truthy(&self) -> Option<bool> {
        self.as_value().and_then(SparqlValue::is_truthy)
    }

    pub fn sparql_eq(&self, other: &Self) -> Option<bool> {
        if let (Some(s), Some(o)) = (self.as_value(), other.as_value()) {
            s.sparql_eq(o)
        } else {
            let s = self.as_term();
            let o = other.as_term();
            if Term::eq(&s, &o) {
                Some(true)
            } else if s.is_literal() && o.is_literal() {
                None // distinct unrecognized literals can not be compared
            } else {
                Some(false)
            }
        }
    }

    pub fn sparql_cmp(&self, other: &Self) -> Option<Ordering> {
        if let (Some(s), Some(o)) = (self.as_value(), other.as_value()) {
            s.partial_cmp(o)
        } else {
            let s = self.as_term();
            let o = other.as_term();
            if s.is_literal() && o.is_literal() && Term::eq(&s, &o) {
                Some(Ordering::Equal)
            } else {
                None // distinct unrecognized literals can not be compared
            }
        }
    }

    /// Determine the order between self and other for SPARQL ORDER BY.
    ///
    /// Note that this function is actually more constrained than the SPARQL spec.
    /// In SPARQL, the order is partial,
    /// while this function falls back to the total order defined by [`Term::cmp`].
    pub fn sparql_order_by(&self, other: &Option<Self>) -> Ordering {
        if let Some(val) = other {
            self.sparql_cmp(val)
                .unwrap_or_else(|| Term::cmp(&self.as_term(), val.as_term()))
        } else {
            Ordering::Greater
        }
    }
}

impl From<ResultTerm> for EvalResult {
    fn from(value: ResultTerm) -> Self {
        EvalResult::Term(value)
    }
}

impl From<GenericLiteral<Arc<str>>> for EvalResult {
    fn from(value: GenericLiteral<Arc<str>>) -> Self {
        EvalResult::Term(ArcTerm::Literal(value).into())
    }
}

impl From<SparqlValue> for EvalResult {
    fn from(value: SparqlValue) -> Self {
        EvalResult::Value(value)
    }
}

impl From<IriRef<Arc<str>>> for EvalResult {
    fn from(value: IriRef<Arc<str>>) -> Self {
        EvalResult::Term(ArcTerm::Iri(value).into())
    }
}

impl From<BnodeId<Arc<str>>> for EvalResult {
    fn from(value: BnodeId<Arc<str>>) -> Self {
        EvalResult::Term(ArcTerm::BlankNode(value).into())
    }
}

impl From<SparqlNumber> for EvalResult {
    fn from(value: SparqlNumber) -> Self {
        EvalResult::Value(value.into())
    }
}

impl From<bool> for EvalResult {
    fn from(value: bool) -> Self {
        EvalResult::Value(value.into())
    }
}

impl From<Arc<str>> for EvalResult {
    fn from(value: Arc<str>) -> Self {
        EvalResult::Value(SparqlValue::String(value, None))
    }
}

impl From<(Arc<str>, Option<LanguageTag<Arc<str>>>)> for EvalResult {
    fn from((lex, tag): (Arc<str>, Option<LanguageTag<Arc<str>>>)) -> Self {
        EvalResult::Value(SparqlValue::String(lex, tag))
    }
}
