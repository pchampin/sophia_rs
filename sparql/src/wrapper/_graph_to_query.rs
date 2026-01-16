use std::marker::PhantomData;

use sophia_api::{
    prelude::Dataset,
    source::TripleSource,
    term::{BaseDirection, SimpleTerm, Term},
    triple::Triple,
};
use spargebra::{
    Query,
    algebra::{Expression, Function, GraphPattern},
    term::{NamedNodePattern, TermPattern, TriplePattern},
};
use uuid::{Uuid, fmt::Simple};

use crate::{SparqlQuery, SparqlWrapper};

impl<'a, D: Dataset + ?Sized> SparqlWrapper<'a, D> {
    /// Make an ASK SPARQL query from a Sophia graph
    pub fn prepare_ask_from_triples<TS: TripleSource>(ts: TS) -> Result<SparqlQuery<D>, TS::Error> {
        let algebra = Query::Ask {
            dataset: None,
            pattern: Self::prepare_algebra_from_triples(ts)?,
            base_iri: None,
        };
        Ok(SparqlQuery {
            algebra,
            _phantom: PhantomData,
        })
    }

    /// Make a CONSTRUCT SPARQL query from a Sophia graph
    pub fn prepare_construct_from_triples<TS: TripleSource>(
        ts: TS,
    ) -> Result<SparqlQuery<D>, TS::Error> {
        let pattern = Self::prepare_algebra_from_triples(ts)?;
        let template = find_bgp(&pattern).clone();
        let algebra = Query::Construct {
            template,
            dataset: None,
            pattern,
            base_iri: None,
        };
        Ok(SparqlQuery {
            algebra,
            _phantom: PhantomData,
        })
    }

    /// Make a SELECT SPARQL query from a Sophia graph
    pub fn prepare_select_from_triples<TS: TripleSource>(
        ts: TS,
    ) -> Result<SparqlQuery<D>, TS::Error> {
        let algebra = Query::Select {
            dataset: None,
            pattern: Self::prepare_algebra_from_triples(ts)?,
            base_iri: None,
        };
        Ok(SparqlQuery {
            algebra,
            _phantom: PhantomData,
        })
    }

    /// Make a BGP from a Sophia graph
    fn prepare_algebra_from_triples<TS: TripleSource>(
        mut ts: TS,
    ) -> Result<GraphPattern, TS::Error> {
        let mut patterns = vec![];
        let mut filter = vec![];
        let mut bind = vec![];
        ts.for_each_triple(|tr| {
            patterns.push(Self::prepare_triple_pattern(tr, &mut filter, &mut bind))
        })?;
        let bgp = GraphPattern::Bgp { patterns };
        Ok(if !filter.is_empty() {
            let inner = Box::new(bgp);
            let mut filter = filter.into_iter();
            let first = filter.next().unwrap();
            let expr = filter.fold(first, |old, next| {
                Expression::And(Box::new(old), Box::new(next))
            });
            GraphPattern::Filter { expr, inner }
        } else {
            bgp
        })
    }

    fn prepare_triple_pattern<T: Triple>(
        t: T,
        filter: &mut Vec<Expression>,
        bind: &mut Vec<(oxrdf::Variable, Expression)>,
    ) -> TriplePattern {
        let subject = Self::prepare_term_pattern(t.s(), filter, bind);
        let predicate = Self::prepare_named_node_pattern(t.p(), filter, bind);
        let object = Self::prepare_term_pattern(t.o(), filter, bind);
        TriplePattern {
            subject,
            predicate,
            object,
        }
    }

    fn prepare_term_pattern<T: Term>(
        t: T,
        filter: &mut Vec<Expression>,
        bind: &mut Vec<(oxrdf::Variable, Expression)>,
    ) -> TermPattern {
        match t.as_simple() {
            SimpleTerm::Iri(iri_ref) => oxrdf::NamedNode::new_unchecked(iri_ref.to_string()).into(),
            SimpleTerm::BlankNode(bnode_id) => {
                oxrdf::BlankNode::new_unchecked(bnode_id.to_string()).into()
            }
            SimpleTerm::LiteralDatatype(lex, dt) => oxrdf::Literal::new_typed_literal(
                lex.to_string(),
                oxrdf::NamedNode::new_unchecked(dt.to_string()),
            )
            .into(),
            SimpleTerm::LiteralLanguage(lex, tag, None) => {
                oxrdf::Literal::new_language_tagged_literal_unchecked(
                    lex.to_string(),
                    tag.to_string(),
                )
                .into()
            }
            SimpleTerm::LiteralLanguage(lex, tag, Some(dir)) => {
                oxrdf::Literal::new_directional_language_tagged_literal_unchecked(
                    lex.to_string(),
                    tag.to_string(),
                    match dir {
                        BaseDirection::Ltr => oxrdf::BaseDirection::Ltr,
                        BaseDirection::Rtl => oxrdf::BaseDirection::Rtl,
                    },
                )
                .into()
            }
            SimpleTerm::Triple(tt) => {
                TermPattern::Triple(Box::new(Self::prepare_triple_pattern(*tt, filter, bind)))
            }
            SimpleTerm::Variable(var_name) => {
                oxrdf::Variable::new_unchecked(var_name.to_string()).into()
            }
        }
    }

    fn prepare_named_node_pattern<T: Term>(
        t: T,
        filter: &mut Vec<Expression>,
        bind: &mut Vec<(oxrdf::Variable, Expression)>,
    ) -> NamedNodePattern {
        match t.as_simple() {
            SimpleTerm::Iri(iri_ref) => {
                return oxrdf::NamedNode::new_unchecked(iri_ref.to_string()).into();
            }
            SimpleTerm::BlankNode(bnode_id) => {
                return oxrdf::Variable::new_unchecked(format!("__{}", bnode_id.as_str())).into();
            }
            SimpleTerm::Variable(var_name) => {
                return oxrdf::Variable::new_unchecked(var_name.to_string()).into();
            }
            _ => (),
        };
        let var = mint_var();
        if let Some(spo) = t.triple() {
            Self::prepare_escaped_triple_term_pattern(&var, spo, filter, bind);
        } else {
            // literal
            filter.push(Expression::SameTerm(
                Box::new(Expression::Variable(var.clone())),
                Box::new(Self::prepare_expr(t)),
            ));
        }
        var.into()
    }

    fn prepare_escaped_triple_term_pattern<T: Term>(
        var: &oxrdf::Variable,
        spo: [T; 3],
        filter: &mut Vec<Expression>,
        bind: &mut Vec<(oxrdf::Variable, Expression)>,
    ) {
        for (t, f) in
            spo.into_iter()
                .zip([Function::Subject, Function::Predicate, Function::Object])
        {
            let exp1 = Expression::FunctionCall(f, vec![var.clone().into()]);
            if let Some(tt) = t.triple() {
                let var2 = mint_var();
                bind.push((var2.clone(), exp1));
                Self::prepare_escaped_triple_term_pattern(var, tt, filter, bind);
            } else {
                let exp2 = Self::prepare_expr(t);
                if let Expression::Variable(var2) = exp2 {
                    bind.push((var2.clone(), exp1));
                } else {
                    filter.push(Expression::SameTerm(Box::new(exp1), Box::new(exp2)));
                }
            }
        }
    }

    fn prepare_expr<T: Term>(t: T) -> Expression {
        match t.as_simple() {
            SimpleTerm::Iri(iri_ref) => oxrdf::NamedNode::new_unchecked(iri_ref.to_string()).into(),
            SimpleTerm::BlankNode(bnode_id) => {
                oxrdf::Variable::new_unchecked(format!("__{}", bnode_id.as_str())).into()
            }
            SimpleTerm::LiteralDatatype(lex, dt) => oxrdf::Literal::new_typed_literal(
                lex.to_string(),
                oxrdf::NamedNode::new_unchecked(dt.to_string()),
            )
            .into(),
            SimpleTerm::LiteralLanguage(lex, tag, None) => {
                oxrdf::Literal::new_language_tagged_literal_unchecked(
                    lex.to_string(),
                    tag.to_string(),
                )
                .into()
            }
            SimpleTerm::LiteralLanguage(lex, tag, Some(dir)) => {
                oxrdf::Literal::new_directional_language_tagged_literal_unchecked(
                    lex.to_string(),
                    tag.to_string(),
                    match dir {
                        BaseDirection::Ltr => oxrdf::BaseDirection::Ltr,
                        BaseDirection::Rtl => oxrdf::BaseDirection::Rtl,
                    },
                )
                .into()
            }
            SimpleTerm::Triple(_) => unreachable!(),
            SimpleTerm::Variable(var_name) => {
                Expression::Variable(oxrdf::Variable::new_unchecked(var_name.to_string()))
            }
        }
    }
}

fn find_bgp(gp: &GraphPattern) -> &Vec<TriplePattern> {
    match gp {
        GraphPattern::Bgp { patterns } => patterns,
        GraphPattern::Filter { inner, .. } | GraphPattern::Extend { inner, .. } => find_bgp(inner),
        _ => unreachable!(),
    }
}

fn mint_var() -> oxrdf::Variable {
    let mut buf = vec![b'_'; Simple::LENGTH + 1];
    Uuid::now_v7().simple().encode_lower(&mut buf[1..]);
    let str = unsafe {
        // SAFETY: we now that buf contain only ASCII characters
        String::from_utf8_unchecked(buf)
    };
    oxrdf::Variable::new_unchecked(str)
}

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::{
        quad::Spog,
        serializer::{Stringifier, TripleSerializer},
        source::QuadSource,
        sparql::SparqlDataset,
        term::IriRef,
    };
    use sophia_isomorphism::isomorphic_graphs;
    use test_case::test_case;

    #[test_case("", true; "empty graph")]
    #[test_case("<x:s> <x:p> 1, 2.", true; "ground graphs")]
    #[test_case("?s <x:p> 1, 2.", true; "subject variable")]
    #[test_case("<x:s> ?p 1, 2.", true; "predicate variable")]
    #[test_case("<x:s> <x:p> ?v1; <x:q> 2.", true; "object variable")]
    #[test_case("<x:s> <x:p> 1, <<( <x:s2> <x:p2> ?o )>>.", true; "deep variable")]
    #[test_case("?s ?p 1, 2.", true; "multiple variables")]
    #[test_case("?s ?s 1, 2.", true; "same variable twice")]
    #[test_case("_:s <x:p> 1, 2.", true; "subject bnode")]
    #[test_case("<x:s> _:p 1, 2.", true; "predicate bnode")]
    #[test_case("<x:s> <x:p> _:v1; <x:q> 2.", true; "object bnode")]
    #[test_case("<x:s> <x:p> 1, <<( <x:s2> <x:p2> _:o )>>.", true; "deep bnode")]
    #[test_case("_:s _:p 1, 2.", true; "multiple bnodes")]
    #[test_case("_:s _:s 1, 2.", true; "same bnode twice")]
    #[test_case("1 2 3, <<(4 5 6)>>.", false; "generalized all literals")]
    #[test_case("_:s [] _:s, <<([] [] [])>>.", true; "generalized all bnodes")]
    #[test_case("<<(<x:ss> ?pp 1)>> <x:p> 2, 3.", false; "generalized triple term subject")]
    #[test_case("<x:s> <<(<x:ps> ?pp  1)>> 2, 3.", false; "generalized triple term predicate")]
    #[test_case("<x:s> <x:p> 1, <<( <<(<x:ss> <x:sp> 2)>> <x:p> 3)>>.", false; "generalized deep triple term subject")]
    #[test_case("<x:s> <x:p> 1, <<( <x:s> <<(<x:ps> <x:pp> 2)>> 3)>>.", false; "generalized deep triple term predicate")]
    #[test_case("<<(<x:s><x:p><x:o>)>><<(<x:s><x:p><x:o>)>><<(<<(<x:s><x:p><x:o>)>><<(<x:s><x:p><x:o>)>><<(<x:s><x:p><x:o>)>>)>>.", false; "generalized all triple terms")]
    #[test_case("<<(?s ?p ?o )>><<(?s ?p ?o )>><<(<<(?s ?p ?o )>><<(?s ?p ?o )>><<(?s ?p ?o )>>)>>.", false; "generalized all triple terms variables")]
    fn test(src: &str, construct: bool) -> Result<(), Box<dyn std::error::Error>> {
        let mut d: Vec<Spog<SimpleTerm<'_>>> =
            sophia_turtle::parser::gtrig::parse_str(src).collect_quads()?;
        let qa = SparqlWrapper::prepare_ask_from_triples(d.quads().to_triples()).unwrap();
        let qc = SparqlWrapper::prepare_construct_from_triples(d.quads().to_triples()).unwrap();
        let qs = SparqlWrapper::prepare_select_from_triples(d.quads().to_triples()).unwrap();

        let sd = SparqlWrapper(&d[..]);

        // ASK query must return True
        println!("{qa:#?}");
        assert!(sd.query(&qa)?.into_boolean());

        // SELECT query must return 1 binding, mapping each variable to itself
        let mut bindings = sd
            .query(&qs)?
            .into_bindings()
            .iter
            .collect::<Result<Vec<_>, _>>()?;
        assert_eq!(bindings.len(), 1);
        for (key, val) in bindings.pop().unwrap().v {
            println!("=== {key:?} -> {val:?}");
            // skip variables generated for the query (long UUIDs or starting with '_')
            if !key.starts_with("_") {
                assert!(val.is_variable());
                assert_eq!(val.variable().unwrap().as_str(), key.as_ref());
            }
        }

        // check that SELECT and ASK do NOT match if any triple is removed from the dataset
        for _ in 0..d.len() {
            let sd = SparqlWrapper(&d[1..]);
            assert!(!sd.query(&qa)?.into_boolean());
            assert!(sd.query(&qs)?.into_bindings().into_iter().next().is_none());

            d.rotate_left(1);
        }
        // CONSTRUCT query must return an isomorphic graph
        if construct {
            let mut d = make_strict(d.clone());
            let sd = SparqlWrapper(&d[..]);
            let result: Vec<[SimpleTerm; 3]> = sd.query(&qc)?.into_triples().collect_triples()?;
            println!(
                "## got:\n{}",
                sophia_turtle::serializer::nt::NTriplesSerializer::new_stringifier()
                    .serialize_graph(&result)?
                    .to_string()
            );
            println!(
                "## exp:\n{}",
                sophia_turtle::serializer::nt::NTriplesSerializer::new_stringifier()
                    .serialize_graph(&d.union_graph())?
                    .to_string()
            );
            assert!(isomorphic_graphs(&result, &d.union_graph())?);

            // check that CONSTRUCT does NOT match if any triple is removed from the dataset
            for _ in 0..d.len() {
                let sd = SparqlWrapper(&d[1..]);
                assert!(empty_ts(sd.query(&qc)?.into_triples())?);

                d.rotate_left(1);
            }
        }

        Ok(())
    }

    fn empty_ts<TS: TripleSource>(mut ts: TS) -> Result<bool, TS::Error> {
        let mut c = 0;
        ts.for_each_triple(|_| c += 1)?;
        Ok(c == 0)
    }

    /// transform a generalized dataset into a strict RDF 1.2 dataset,
    /// for the purpose of the `test` function above.
    /// It is *not* 100% robust, as:
    /// - all IRIs are assumed to be resolved
    /// - all literals, bnodes and variables are assumed to contain only IRI-friendly characters
    /// - only the lexical form of literals is retained
    /// - triple terms in any other position as object will panic
    fn make_strict(d: Vec<Spog<SimpleTerm<'_>>>) -> Vec<Spog<SimpleTerm<'_>>> {
        d.into_iter()
            .map(|([s, p, o], g)| {
                (
                    [
                        make_strict_subject(s),
                        make_strict_predicate(p),
                        make_strict_object(o),
                    ],
                    g.map(make_strict_subject),
                )
            })
            .collect()
    }

    fn make_strict_subject(t: SimpleTerm<'_>) -> SimpleTerm<'_> {
        match t {
            SimpleTerm::LiteralDatatype(lex, ..) | SimpleTerm::LiteralLanguage(lex, ..) => {
                SimpleTerm::Iri(IriRef::new_unchecked(format!("x-lit:{lex}").into()))
            }

            SimpleTerm::Triple(_) => todo!(),
            SimpleTerm::Variable(var_name) => SimpleTerm::Iri(IriRef::new_unchecked(
                format!("x-var:{}", var_name.as_str()).into(),
            )),
            _ => t,
        }
    }

    fn make_strict_predicate(t: SimpleTerm<'_>) -> SimpleTerm<'_> {
        match t {
            SimpleTerm::BlankNode(bnid) => SimpleTerm::Iri(IriRef::new_unchecked(
                format!("x-bn:{}", bnid.as_str()).into(),
            )),
            SimpleTerm::LiteralDatatype(lex, ..) | SimpleTerm::LiteralLanguage(lex, ..) => {
                SimpleTerm::Iri(IriRef::new_unchecked(format!("x-lit:{lex}").into()))
            }

            SimpleTerm::Triple(_) => todo!(),
            SimpleTerm::Variable(var_name) => SimpleTerm::Iri(IriRef::new_unchecked(
                format!("x-var:{}", var_name.as_str()).into(),
            )),
            _ => t,
        }
    }

    fn make_strict_object(t: SimpleTerm<'_>) -> SimpleTerm<'_> {
        match t {
            SimpleTerm::Triple(tr) => {
                let [s, p, o] = *tr;
                SimpleTerm::Triple(Box::new([
                    make_strict_subject(s),
                    make_strict_predicate(p),
                    make_strict_object(o),
                ]))
            }
            SimpleTerm::Variable(var_name) => SimpleTerm::Iri(IriRef::new_unchecked(
                format!("x-var:{}", var_name.as_str()).into(),
            )),
            _ => t,
        }
    }
}
