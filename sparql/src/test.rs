use crate::*;
use sophia_api::{prelude::*, sparql::Query};
use sophia_inmem::dataset::LightDataset;
use test_case::test_case;

#[allow(clippy::needless_pass_by_value)]
#[test_case(
    "SELECT ?x { ?s a ?x }",
    vec!["<http://schema.org/Event>", "<http://schema.org/Person>", ];
    "types"
)]
#[test_case(
    "SELECT ?x { [] ?x [] }",
    vec!["<http://schema.org/name>", "<http://schema.org/name>", "<http://schema.org/performerIn>", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>", ];
    "predicates"
)]
#[test_case(
    "SELECT ?x { [] ?p ?x }",
    vec!["\"Alice\"^^<http://www.w3.org/2001/XMLSchema#string>", "\"Bob's birthday party\"^^<http://www.w3.org/2001/XMLSchema#string>", "<http://schema.org/Event>", "<http://schema.org/Person>", "_:b"];
    "objects"
)]
#[test_case(
    "SELECT ?x { ?x ?y \"not in the repo\" }",
    vec![];
    "no result"
)]
#[test_case(
    "SELECT ?x { { ?x a s:Event } UNION { ?x a s:Person } }",
    vec!["<https://example.org/test#a>", "_:b"];
    "union2"
)]
#[test_case(
    "SELECT ?x { { ?x a s:Organization } UNION { ?x a s:Person } }",
    vec!["<https://example.org/test#a>"];
    "union1"
)]
#[test_case(
    "SELECT ?x { { ?x a s:Organization } UNION { ?x a s:Book } }",
    vec![];
    "union0"
)]
#[test_case(
    "SELECT ?x { GRAPH ?x { ?s ?p ?o } }",
    vec!["<https://example.org/test#g>", "<https://example.org/test#g>", "_:b"];
    "graphs"
)]
#[test_case(
    "SELECT ?x { GRAPH ?g { <#a> s:name ?x } }",
    vec!["\"Albert\"^^<http://www.w3.org/2001/XMLSchema#string>"];
    "name in g"
)]
#[test_case(
    "SELECT ?x { GRAPH ?g { ?y s:name ?x } }",
    vec!["\"Albert\"^^<http://www.w3.org/2001/XMLSchema#string>", "\"Alice\"^^<http://www.w3.org/2001/XMLSchema#string>"];
    "names in g"
)]
#[test_case(
    "SELECT ?x { GRAPH ?g { ?x s:name ?y } }",
    vec!["<https://example.org/test#a>", "<https://example.org/test#b>"];
    "named in g"
)]
#[test_case(
    "SELECT ?x { <#a> s:name ?n. BIND (?n as ?n2) GRAPH ?g { ?x s:name ?n2 } }",
    vec!["<https://example.org/test#b>"];
    "join"
)]
#[test_case(
    "SELECT ?x { VALUES ?t { s:Event s:Person } ?x a ?t. }",
    vec!["<https://example.org/test#a>", "_:b"];
    "values"
)]
#[test_case(
    "SELECT ?x { VALUES ?y { 10 20 } VALUES ?z { 1 2 } BIND (?y+?z as ?x) }",
    vec!["\"11\"^^<http://www.w3.org/2001/XMLSchema#integer>","\"12\"^^<http://www.w3.org/2001/XMLSchema#integer>", "\"21\"^^<http://www.w3.org/2001/XMLSchema#integer>", "\"22\"^^<http://www.w3.org/2001/XMLSchema#integer>"];
    "values cross-product"
)]
#[test_case(
    "SELECT ?x { ?x s:name ?n. MINUS { ?x s:performerIn [] }}",
    vec!["_:b"];
    "minus"
)]
#[test_case(
    "SELECT ?x { ?x s:name ?n. MINUS { ?y s:performerIn [] }}",
    vec!["<https://example.org/test#a>", "_:b"];
    "minus disjoint domain"
)]
fn test_select_1_and_ask(query: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = format!("BASE <https://example.org/test> PREFIX s: <http://schema.org/> {query}");
    let parsed_query = SparqlQuery::parse(&query)?;
    let bindings = dataset.query(&parsed_query)?.into_bindings();
    assert_eq!(bindings.variables(), &["x"]);
    let mut got = bindings_to_vec(bindings, 1);
    got.sort();
    assert_eq!(exp, got);

    let parsed_query = SparqlQuery::parse(&query.replace("SELECT ?x", "ASK"))?;
    let response = dataset.query(&parsed_query)?.into_boolean();
    assert_eq!(response, !exp.is_empty());
    Ok(())
}

#[test_case(
    "SELECT ?x ?y { ?z a ?x. OPTIONAL { ?z s:performerIn ?y } } ORDER BY ?x",
    vec!["<http://schema.org/Event>", "", "<http://schema.org/Person>", "_:b"];
    "left join no condition"
)]
#[test_case(
    "SELECT ?x ?y { ?x a ?z. OPTIONAL { ?x s:name ?y. FILTER (?y < \"B\") } } ORDER BY ?x",
    vec!["_:b", "", "<https://example.org/test#a>", "\"Alice\"^^<http://www.w3.org/2001/XMLSchema#string>"];
    "left join with condition"
)]
#[test_case(
    "SELECT ?x ?y { VALUES (?x ?y) { (<x:a> <x:b>) ( <x:c> <x:d>)} } ORDER BY ?x",
    vec![ "<x:a>", "<x:b>", "<x:c>", "<x:d>"];
    "values"
)]
fn test_select_2(query: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = format!("BASE <https://example.org/test> PREFIX s: <http://schema.org/> {query}");
    let parsed_query = SparqlQuery::parse(&query)?;
    let bindings = dataset.query(&parsed_query)?.into_bindings();
    assert_eq!(bindings.variables(), &["x", "y"]);
    let got = bindings_to_vec(bindings, 2);
    assert_eq!(exp, got);

    Ok(())
}

#[test_case(
    "SELECT DISTINCT ?x { VALUES ?x { \"a\" \"a\" \"b\" \"a\" } }",
    vec!["\"a\"^^<http://www.w3.org/2001/XMLSchema#string>", "\"b\"^^<http://www.w3.org/2001/XMLSchema#string>"];
    "distinct"
)]
#[test_case(
    "SELECT REDUCED ?x { VALUES ?x { \"a\" \"a\" \"b\" \"a\" } }",
    vec!["\"a\"^^<http://www.w3.org/2001/XMLSchema#string>", "\"b\"^^<http://www.w3.org/2001/XMLSchema#string>", "\"a\"^^<http://www.w3.org/2001/XMLSchema#string>"];
    "reduced"
)]
fn test_reduce(query: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = format!("BASE <https://example.org/test> PREFIX s: <http://schema.org/> {query}");
    let parsed_query = SparqlQuery::parse(&query)?;
    let bindings = dataset.query(&parsed_query)?.into_bindings();
    assert_eq!(bindings.variables(), &["x"]);
    let got = bindings_to_vec(bindings, 1);
    assert_eq!(exp, got);
    Ok(())
}

#[test_case(
    "SELECT ?s ?o { ?s :q|:z ?o }",
    vec![
        ["<x:a2>", "<x:b2>"],
        ["<x:b2>", "<x:c2>"],
        ["<x:c2>", "<x:d2>"],
        ["<x:d2>", "<x:b2>"],
        ["<x:d2>", "<x:e2>"],
    ];
    "alt"
)]
#[test_case(
    "SELECT ?s ?o { ?s :p|:s ?o }",
    vec![
        ["<x:a1>", "<x:b1>"],
        ["<x:a1>", "<x:b1>"],
        ["<x:b1>", "<x:c1>"],
        ["<x:c1>", "<x:d1>"],
        ["<x:d1>", "<x:e1>"],
    ];
    "alt with multiple paths"
)]
#[test_case(
    "SELECT ?s ?o { ?s (:p/:r)|(:r/:q) ?o }",
    vec![
        ["<x:a1>", "<x:b2>"],
        ["<x:a1>", "<x:b2>"],
        ["<x:b1>", "<x:c2>"],
        ["<x:b1>", "<x:c2>"],
        ["<x:c1>", "<x:d2>"],
        ["<x:c1>", "<x:d2>"],
        ["<x:d1>", "<x:b2>"],
        ["<x:d1>", "<x:e2>"],
        ["<x:d1>", "<x:e2>"],
    ];
    "alt of seqs"
)]
#[test_case(
    "SELECT ?s ?o { ?s (:p|:s)/:p ?o }",
    vec![
        ["<x:a1>", "<x:c1>"],
        ["<x:a1>", "<x:c1>"],
        ["<x:b1>", "<x:d1>"],
        ["<x:c1>", "<x:e1>"],
    ];
    "seq with multiple paths"
)]
#[test_case(
    "SELECT ?s ?o { ?s (:p/:r)|(:q/^:r) ?o }",
    vec![
        ["<x:a1>", "<x:b2>"],
        ["<x:a2>", "<x:b1>"],
        ["<x:b1>", "<x:c2>"],
        ["<x:b2>", "<x:c1>"],
        ["<x:c1>", "<x:d2>"],
        ["<x:c2>", "<x:d1>"],
        ["<x:d1>", "<x:e2>"],
        ["<x:d2>", "<x:b1>"],
        ["<x:d2>", "<x:e1>"],
    ];
    "alt of seqs with rev"
)]
#[test_case(
    "SELECT ?s ?o { ?s :p* ?o }",
    vec![
        ["<< <x:a1> <x:b1> <x:c1> >>", "<< <x:a1> <x:b1> <x:c1> >>"],
        ["<< <x:a2> <x:b1> <x:c2> >>", "<< <x:a2> <x:b1> <x:c2> >>"],
        ["<x:a1>", "<x:a1>"],
        ["<x:a1>", "<x:b1>"],
        ["<x:a1>", "<x:c1>"],
        ["<x:a1>", "<x:d1>"],
        ["<x:a1>", "<x:e1>"],
        ["<x:a2>", "<x:a2>"],
        ["<x:b1>", "<x:b1>"],
        ["<x:b1>", "<x:c1>"],
        ["<x:b1>", "<x:d1>"],
        ["<x:b1>", "<x:e1>"],
        ["<x:b2>", "<x:b2>"],
        ["<x:c1>", "<x:c1>"],
        ["<x:c1>", "<x:d1>"],
        ["<x:c1>", "<x:e1>"],
        ["<x:c2>", "<x:c2>"],
        ["<x:d1>", "<x:d1>"],
        ["<x:d1>", "<x:e1>"],
        ["<x:d2>", "<x:d2>"],
        ["<x:e1>", "<x:e1>"],
        ["<x:e2>", "<x:e2>"],
    ];
    "zero or more"
)]
#[test_case(
    "SELECT ?s ?o { ?s :q* ?o }",
    vec![
        ["<< <x:a1> <x:b1> <x:c1> >>", "<< <x:a1> <x:b1> <x:c1> >>"],
        ["<< <x:a2> <x:b1> <x:c2> >>", "<< <x:a2> <x:b1> <x:c2> >>"],
        ["<x:a1>", "<x:a1>"],
        ["<x:a2>", "<x:a2>"],
        ["<x:a2>", "<x:b2>"],
        ["<x:a2>", "<x:c2>"],
        ["<x:a2>", "<x:d2>"],
        ["<x:a2>", "<x:e2>"],
        ["<x:b1>", "<x:b1>"],
        ["<x:b2>", "<x:b2>"],
        ["<x:b2>", "<x:c2>"],
        ["<x:b2>", "<x:d2>"],
        ["<x:b2>", "<x:e2>"],
        ["<x:c1>", "<x:c1>"],
        ["<x:c2>", "<x:b2>"],
        ["<x:c2>", "<x:c2>"],
        ["<x:c2>", "<x:d2>"],
        ["<x:c2>", "<x:e2>"],
        ["<x:d1>", "<x:d1>"],
        ["<x:d2>", "<x:b2>"],
        ["<x:d2>", "<x:c2>"],
        ["<x:d2>", "<x:d2>"],
        ["<x:d2>", "<x:e2>"],
        ["<x:e1>", "<x:e1>"],
        ["<x:e2>", "<x:e2>"],
    ];
    "zero or more with loop"
)]
#[test_case(
    "SELECT ?s ?o { ?s :p+ ?o }",
    vec![
        ["<x:a1>", "<x:b1>"],
        ["<x:a1>", "<x:c1>"],
        ["<x:a1>", "<x:d1>"],
        ["<x:a1>", "<x:e1>"],
        ["<x:b1>", "<x:c1>"],
        ["<x:b1>", "<x:d1>"],
        ["<x:b1>", "<x:e1>"],
        ["<x:c1>", "<x:d1>"],
        ["<x:c1>", "<x:e1>"],
        ["<x:d1>", "<x:e1>"],
    ];
    "one or more"
)]
// NB: the duplicate (d2, e2) below is specific to Sophia,
// neither Jena nor Oxigraph do it, and their behavior is consistent with the spec
// BUT the spec is not entirely self-consistent
// ppeval is supposed to return a *multiset*,
// so the notion of UNION, when implicit, is arguably ambiguous
#[test_case(
    "SELECT ?s ?o { ?s :q+ ?o }",
    vec![
        ["<x:a2>", "<x:b2>"],
        ["<x:a2>", "<x:c2>"],
        ["<x:a2>", "<x:d2>"],
        ["<x:a2>", "<x:e2>"],
        ["<x:b2>", "<x:b2>"],
        ["<x:b2>", "<x:c2>"],
        ["<x:b2>", "<x:d2>"],
        ["<x:b2>", "<x:e2>"],
        ["<x:c2>", "<x:b2>"],
        ["<x:c2>", "<x:c2>"],
        ["<x:c2>", "<x:d2>"],
        ["<x:c2>", "<x:e2>"],
        ["<x:d2>", "<x:b2>"],
        ["<x:d2>", "<x:c2>"],
        ["<x:d2>", "<x:d2>"],
        ["<x:d2>", "<x:e2>"],
        ["<x:d2>", "<x:e2>"],
    ];
    "one or more with loop"
)]
#[test_case(
    "SELECT ?s ?o { ?s :p? ?o }",
    vec![
        ["<< <x:a1> <x:b1> <x:c1> >>", "<< <x:a1> <x:b1> <x:c1> >>"],
        ["<< <x:a2> <x:b1> <x:c2> >>", "<< <x:a2> <x:b1> <x:c2> >>"],
        ["<x:a1>", "<x:a1>"],
        ["<x:a1>", "<x:b1>"],
        ["<x:a2>", "<x:a2>"],
        ["<x:b1>", "<x:b1>"],
        ["<x:b1>", "<x:c1>"],
        ["<x:b2>", "<x:b2>"],
        ["<x:c1>", "<x:c1>"],
        ["<x:c1>", "<x:d1>"],
        ["<x:c2>", "<x:c2>"],
        ["<x:d1>", "<x:d1>"],
        ["<x:d1>", "<x:e1>"],
        ["<x:d2>", "<x:d2>"],
        ["<x:e1>", "<x:e1>"],
        ["<x:e2>", "<x:e2>"],
    ];
    "zero or one"
)]
#[test_case(
    "SELECT ?s ?o { ?s :q? ?o }",
    vec![
        ["<< <x:a1> <x:b1> <x:c1> >>", "<< <x:a1> <x:b1> <x:c1> >>"],
        ["<< <x:a2> <x:b1> <x:c2> >>", "<< <x:a2> <x:b1> <x:c2> >>"],
        ["<x:a1>", "<x:a1>"],
        ["<x:a2>", "<x:a2>"],
        ["<x:a2>", "<x:b2>"],
        ["<x:b1>", "<x:b1>"],
        ["<x:b2>", "<x:b2>"],
        ["<x:b2>", "<x:c2>"],
        ["<x:c1>", "<x:c1>"],
        ["<x:c2>", "<x:c2>"],
        ["<x:c2>", "<x:d2>"],
        ["<x:d1>", "<x:d1>"],
        ["<x:d2>", "<x:b2>"],
        ["<x:d2>", "<x:d2>"],
        ["<x:d2>", "<x:e2>"],
        ["<x:e1>", "<x:e1>"],
        ["<x:e2>", "<x:e2>"],
    ];
    "zero or one with loop"
)]
// NB in the test below, it is questionable whether (a1, b1) should be duplicated
// Jena produces the duplicate, but the spec mentions a set, so it should not
// Currently, we also return the duplicate because it is easier.
#[test_case(
    "SELECT ?s ?o { ?s !(:q|:t) ?o }",
    vec![
        ["<x:a1>", "<x:a2>"],
        ["<x:a1>", "<x:b1>"],
        ["<x:a1>", "<x:b1>"], 
        ["<x:b1>", "<x:b2>"],
        ["<x:b1>", "<x:c1>"],
        ["<x:c1>", "<x:c2>"],
        ["<x:c1>", "<x:d1>"],
        ["<x:d1>", "<x:d2>"],
        ["<x:d1>", "<x:e1>"],
        ["<x:e1>", "<x:e2>"],
    ];
    "negated property set"
)]
#[test_case(
    "SELECT ?s ?o { :d2 :q* ?o }",
    vec![
        ["", "<x:b2>"],
        ["", "<x:c2>"],
        ["", "<x:d2>"],
        ["", "<x:e2>"],
    ];
    "zero or more with bound subject"
)]
#[test_case(
    "SELECT ?s ?o { ?s :q* :e2 }",
    vec![
        ["<x:a2>", ""],
        ["<x:b2>", ""],
        ["<x:c2>", ""],
        ["<x:d2>", ""],
        ["<x:e2>", ""],
    ];
    "zero or more with bound object"
)]
#[test_case(
    "SELECT ?s ?o { :d2 :q* :e2 }",
    vec![
        ["", ""],
    ];
    "zero or more with both bound and success"
)]
#[test_case(
    "SELECT ?s ?o { :d2 :q* :a2 }",
    vec![];
    "zero or more with both bound and failure"
)]
// The test below is really a corner case.
// It is required by the spec by, for example, Oxigraph does not pass it (Jena does).
// I might decide to not support it in the future.
#[test_case(
    "SELECT ?s ?o { :z :q* ?o }",
    vec![
        ["", "<x:z>"],
    ];
    "zero or more with bound subject absent from graph"
)]
// NB: the test below fails (no bindings are returned).
// Strictly speaking, the spec requires it to pass,
// but this is rather a corner case.
// #[test_case(
//     "SELECT ?s ?o { ?s :q* :z}",
//     vec![
//         ["<x:z>", ""],
//     ];
//     "zero or more with bound object absent from graph"
// )]
#[test_case(
    "SELECT ?s ?o { :z :q* :z}",
    vec![
        ["", ""],
    ];
    "zero or more with both bound absent from graph"
)]
#[test_case(
    "SELECT ?s ?o { ?s :q* ?s}",
    vec![
        ["<< <x:a1> <x:b1> <x:c1> >>", ""],
        ["<< <x:a2> <x:b1> <x:c2> >>", ""],
        ["<x:a1>", ""],
        ["<x:a2>", ""],
        ["<x:b1>", ""],
        ["<x:b2>", ""],
        ["<x:c1>", ""],
        ["<x:c2>", ""],
        ["<x:d1>", ""],
        ["<x:d2>", ""],
        ["<x:e1>", ""],
        ["<x:e2>", ""],
    ];
    "zero or more with same variable"
)]
// About the test below, see "one or more with loop" about the duplicate (d2, e2)
#[test_case(
    "SELECT ?s ?o { :d2 :q+ ?o }",
    vec![
        ["", "<x:b2>"],
        ["", "<x:c2>"],
        ["", "<x:d2>"],
        ["", "<x:e2>"],
        ["", "<x:e2>"],
    ];
    "one or more with bound subject"
)]
// About the test below, see "one or more with loop" about the duplicate (d2, e2)
#[test_case(
    "SELECT ?s ?o { ?s :q+ :e2 }",
    vec![
        ["<x:a2>", ""],
        ["<x:b2>", ""],
        ["<x:c2>", ""],
        ["<x:d2>", ""],
        ["<x:d2>", ""],
    ];
    "one or more with bound object"
)]
// About the test below, see "one or more with loop" about the duplicate (d2, e2)
#[test_case(
    "SELECT ?s ?o { :d2 :q+ :e2 }",
    vec![
        ["", ""],
        ["", ""],
    ];
    "one or more with both bound and success"
)]
#[test_case(
    "SELECT ?s ?o { :d2 :q+ :a2 }",
    vec![];
    "one or more with both bound and failure"
)]
#[test_case(
    "SELECT ?s ?o { ?s :q+ ?s }",
    vec![
        ["<x:b2>", ""],
        ["<x:c2>", ""],
        ["<x:d2>", ""],
    ];
    "one or more with same variable"
)]
#[test_case(
    "SELECT ?s ?o { :d2 :q? ?o }",
    vec![
        ["", "<x:b2>"],
        ["", "<x:d2>"],
        ["", "<x:e2>"],
    ];
    "zero or one with bound subject"
)]
#[test_case(
    "SELECT ?s ?o { ?s :q? :e2 }",
    vec![
        ["<x:d2>", ""],
        ["<x:e2>", ""],
    ];
    "zero or one with bound object"
)]
#[test_case(
    "SELECT ?s ?o { :d2 :q? :e2 }",
    vec![
        ["", ""],
    ];
    "zero or one with both bound and success"
)]
#[test_case(
    "SELECT ?s ?o { :d2 :q? :c2 }",
    vec![];
    "zero or one with both bound and failure"
)]
// The test below is really a corner case.
// It is required by the spec by, for example, Oxigraph does not pass it (Jena does).
// I might decide to not support it in the future.
#[test_case(
    "SELECT ?s ?o { :z :q? ?o }",
    vec![
        ["", "<x:z>"],
    ];
    "zero or one with bound subject absent from the graph"
)]
// NB: the test below fails (no bindings are returned).
// Strictly speaking, the spec requires it to pass,
// but this is rather a corner case.
// #[test_case(
//     "SELECT ?s ?o { ?s :q? :z }",
//     vec![
//         ["<x:z>", ""],
//     ];
//     "zero or one with bound object absent from the graph"
// )]
#[test_case(
    "SELECT ?s ?o { :z :q? :z }",
    vec![
        ["", ""],
    ];
    "zero or one with both bound absent from the graph"
)]
#[test_case(
    "SELECT ?s ?o { ?s :q? ?s }",
    vec![
        ["<< <x:a1> <x:b1> <x:c1> >>", ""],
        ["<< <x:a2> <x:b1> <x:c2> >>", ""],
        ["<x:a1>", ""],
        ["<x:a2>", ""],
        ["<x:b1>", ""],
        ["<x:b2>", ""],
        ["<x:c1>", ""],
        ["<x:c2>", ""],
        ["<x:d1>", ""],
        ["<x:d2>", ""],
        ["<x:e1>", ""],
        ["<x:e2>", ""],
    ];
    "zero or one with same variable"
)]
// In the following tests, the alternative with :z is a trick,
// to prevent the query parsing to convert it to a BGP.
// The purpose is to test the property path evaluation.
#[test_case(
    "SELECT ?s ?o { <<( ?s ?p1 [] )>> (^:t/:p/:t)|:z <<( [] ?p2 ?o )>>}",
    vec![];
    "triple patterns failing"
)]
#[test_case(
    "SELECT ?s ?o { <<( ?s ?p1 [] )>> (^:t/:r/:t)|:z <<( [] ?p2 ?o )>>}",
    vec![
        ["<x:a1>", "<x:c2>"],
    ];
    "triple patterns succeeding"
)]
#[test_case(
    "SELECT ?s ?o { <<( ?s ?p1 ?v )>> (^:t/:r/:t)|:z <<( ?v ?p2 ?o )>>}",
    vec![];
    "triple patterns failing because of similar variables v"
)]
#[test_case(
    "SELECT ?s ?o { <<( ?s ?p1 _:b )>> (^:t/:r/:t)|:z <<( _:b ?p2 ?o )>>}",
    vec![];
    "triple patterns failing because of similar bnode b"
)]
#[test_case(
    "SELECT ?s ?o { <<( ?s ?o [] )>> (^:t/:r/:t)|:z <<( [] ?o [] )>>}",
    vec![
        ["<x:a1>", "<x:b1>"],
    ];
    "triple patterns with common variable"
)]
fn test_ppath(query: &str, exp: Vec<[&str; 2]>) -> TestResult {
    let dataset = dataset_ppath()?;
    let dataset = SparqlWrapper(&dataset);
    let query = format!("BASE <https://example.org/test> PREFIX : <x:> {query}");
    let parsed_query = SparqlQuery::parse(&query)?;
    let bindings = dataset.query(&parsed_query)?.into_bindings();
    assert_eq!(bindings.variables(), &["s", "o"]);
    let mut got = bindings_to_vec_of_arrays(bindings, ["s", "o"]);
    got.sort_unstable();
    assert_eq!(exp, got);
    Ok(())
}

#[test]
fn test_union() -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let parsed_query = SparqlQuery::parse(
        r"
        PREFIX s: <http://schema.org/>
        SELECT ?p ?e {
            { ?p a s:Person }
            UNION
            { ?e a s:Event }
        }
    ",
    )?;
    let bindings = dataset.query(&parsed_query)?.into_bindings();
    assert_eq!(bindings.variables(), &["p", "e"]);
    let mut got = bindings.into_iter().collect::<Result<Vec<_>, _>>()?;
    got.sort();
    assert_eq!(got.len(), 2);
    assert_eq!(got[0].len(), 2);
    assert!(got[0][0].is_none());
    assert!(got[0][1].is_some());
    assert!(got[0][1].as_ref().unwrap().is_blank_node());
    assert!(got[1][0].is_some());
    assert!(got[1][0].as_ref().unwrap().is_iri());
    assert_eq!(
        got[1][0].as_ref().unwrap().to_string(),
        "<https://example.org/test#a>"
    );
    assert!(got[1][1].is_none());
    Ok(())
}

#[test_case(1)]
#[test_case(2)]
#[test_case(3)]
#[test_case(4)]
#[test_case(5)]
#[test_case(6)]
fn test_limit_offset(limit: usize) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query0 = format!("SELECT ?p {{ [] ?p [] }} LIMIT {limit}");
    let got = bindings_to_vec(dataset.query(query0.as_str())?.into_bindings(), 1);
    assert_eq!(got.len(), limit.min(5));

    let mut offset = 0;
    let mut got = vec![];
    loop {
        let query = format!("SELECT ?p {{ [] ?p [] }} OFFSET {offset} LIMIT {limit}");
        let partial = bindings_to_vec(dataset.query(query.as_str())?.into_bindings(), 1);
        let exp_len = if offset >= 5 {
            0
        } else {
            limit.min(5 - offset)
        };
        assert_eq!(partial.len(), exp_len);
        got.extend_from_slice(&partial);
        if exp_len == 0 {
            break;
        }
        offset += limit;
    }
    got.sort();
    let exp = vec![
        "<http://schema.org/name>",
        "<http://schema.org/name>",
        "<http://schema.org/performerIn>",
        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
    ];
    assert_eq!(exp, got);
    Ok(())
}

#[allow(clippy::needless_pass_by_value)]
#[test_case("", vec!["<https://example.org/test#a>", "_:b"]; "control")]
#[test_case("FILTER (true)", vec!["<https://example.org/test#a>", "_:b"]; "always true")]
#[test_case("FILTER (false)", vec![]; "always false")]
#[test_case("FILTER (42/0)", vec![]; "error")]
#[test_case("FILTER EXISTS { ?x s:name ?e }", vec!["<https://example.org/test#a>", "_:b"]; "exists redundant")]
#[test_case("FILTER EXISTS { ?x s:performerIn ?e }", vec!["<https://example.org/test#a>"]; "exists success")]
#[test_case("FILTER EXISTS { ?x s:knows ?e }", vec![]; "exists failure")]
#[test_case("FILTER EXISTS { BIND(42 as ?x) }", vec!["<https://example.org/test#a>", "_:b"]; "exists bind")]
fn test_filter(filter: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(&format!(
        "PREFIX s: <http://schema.org/> SELECT ?x {{ ?x s:name ?n. {filter} }}"
    ))?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings, 1);
    got.sort();
    assert_eq!(exp, got);
    Ok(())
}

#[test]
fn test_expr_iri() -> TestResult {
    assert_eq!(
        eval_expr("<http://schema.org/name>")?,
        "<http://schema.org/name>"
    );
    Ok(())
}

#[test]
fn test_expr_literal() -> TestResult {
    assert_eq!(
        eval_expr("42")?,
        "\"42\"^^<http://www.w3.org/2001/XMLSchema#integer>"
    );
    Ok(())
}

#[test]
fn test_expr_variable() -> TestResult {
    let dataset = LightDataset::default();
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse("SELECT (?y as ?x) { BIND(<http://schema.org/name> as ?y)}")?;
    let bindings = dataset.query(&query)?.into_bindings();
    let got = bindings_to_vec(bindings, 1);
    assert_eq!(&["<http://schema.org/name>"], &got[..]);
    Ok(())
}

// test ||
#[test_case("true    || true   ", "true "; "T or T")]
#[test_case("true    || false  ", "true "; "T or F")]
#[test_case("false   || true   ", "true "; "F or T")]
#[test_case("false   || false  ", "false"; "F or F")]
#[test_case("true    || <tag:x>", "true "; "T or E")]
#[test_case("<tag:x> || true   ", "true "; "E or T")]
#[test_case("false   || <tag:x>", ""; "F or E")]
#[test_case("<tag:x> || false  ", ""; "E or F")]
#[test_case("<tag:x> || <tag:x>", ""; "E or E")]
// test &&
#[test_case("true    && true   ", "true "; "T and T")]
#[test_case("true    && false  ", "false"; "T and F")]
#[test_case("false   && true   ", "false"; "F and T")]
#[test_case("false   && false  ", "false"; "F and F")]
#[test_case("false   && <tag:x>", "false"; "F and E")]
#[test_case("<tag:x> && false  ", "false"; "E and F")]
#[test_case("true    && <tag:x>", ""; "T and E")]
#[test_case("<tag:x> && true   ", ""; "E and T")]
#[test_case("<tag:x> && <tag:x>", ""; "E and E")]
// test !
#[test_case("!true   ", "false"; "not T")]
#[test_case("!false  ", "true"; "not F")]
#[test_case("!<tag:x>", ""; "not E")]
// test Effective Boolean Value
#[test_case("!(!\"foo\")           ", "true "; "bool string non-empty")]
#[test_case("!(!\"\")              ", "false"; "bool string empty")]
#[test_case("!(!42)                ", "true "; "bool number non-zero")]
#[test_case("!(!0)                 ", "false"; "bool number zero")]
#[test_case("!(!\"foo\"@en)        ", "true "; "bool lang-string non-empty")]
#[test_case("!(!\"foo\"@en--ltr)   ", "true "; "bool dir-lang-string non-empty")]
#[test_case("!(!\"\"@en)           ", "false"; "bool lang-string empty")]
#[test_case("!(!\"\"@en--ltr)      ", "false"; "bool dir-lang-string empty")]
#[test_case("!(!\"1\"^^xsd:boolean)", "true"; "bool numeric-formed")]
#[test_case("!(!\"x\"^^xsd:boolean)", "false"; "bool ill-formed")]
#[test_case("!(!<tag:x>)           ", ""; "bool iri")]
// test add
#[test_case("40+2", "42"; "add int")]
#[test_case("40+2.0", "42.0"; "add dec")]
#[test_case("40+\"2\"^^xsd:float", "\"4.2e1\"^^xsd:float"; "add flt")]
#[test_case("40+2e0", "4.2e1"; "add dbl")]
#[test_case("100000000000000000000+2", "100000000000000000002"; "add bigint")]
#[test_case("40+\"2\"", ""; "add err")]
// test sub
#[test_case("40-2", "38"; "sub int")]
#[test_case("40-2.0", "38.0"; "sub dec")]
#[test_case("40-\"2\"^^xsd:float", "\"3.8e1\"^^xsd:float"; "sub flt")]
#[test_case("40-2e0", "3.8e1"; "sub dbl")]
#[test_case("100000000000000000000-2", "99999999999999999998"; "sub bigint")]
#[test_case("40-\"2\"", ""; "sub err")]
// test mul
#[test_case("40*2", "80"; "mul int")]
#[test_case("40*2.0", "80.0"; "mul dec")]
#[test_case("40*\"2\"^^xsd:float", "\"8e1\"^^xsd:float"; "mul flt")]
#[test_case("40*2e0", "8e1"; "mul dbl")]
#[test_case("100000000000000000000*2", "200000000000000000000"; "mul bigint")]
#[test_case("40*\"2\"", ""; "mul err")]
// test div
#[test_case("40/2", "20.0"; "div int")]
#[test_case("40/2.0", "20.0"; "div dec")]
#[test_case("40/\"2\"^^xsd:float", "\"2e1\"^^xsd:float"; "div flt")]
#[test_case("40/2e0", "2e1"; "div dbl")]
#[test_case("100000000000000000000/2", "50000000000000000000.0"; "div bigint")]
#[test_case("40/\"2\"", ""; "div err")]
#[test_case("40/0", ""; "div by zero")]
// test unary-plus
#[test_case("+(42)", "42"; "plus int")]
#[test_case("+(42.0)", "42.0"; "plus dec")]
#[test_case("+(\"42\"^^xsd:float)", "\"4.2e1\"^^xsd:float"; "plus flt")]
#[test_case("+(42e0)", "4.2e1"; "plus dbl")]
#[test_case("+(100000000000000000000)", "100000000000000000000"; "plus bigint")]
#[test_case("+(\"42\")", ""; "plus str")]
#[test_case("+(42/0)", ""; "plus error")]
// test unary-minus
#[test_case("-(42)", "-42"; "minus int")]
#[test_case("-(42.0)", "-42.0"; "minus dec")]
#[test_case("-(\"42\"^^xsd:float)", "-\"4.2e1\"^^xsd:float"; "minus flt")]
#[test_case("-(42e0)", "-4.2e1"; "minus dbl")]
#[test_case("-(100000000000000000000)", "-100000000000000000000"; "minus bigint")]
#[test_case("-(\"42\")", ""; "minus str")]
#[test_case("-(42/0)", ""; "minus error")]
// test if-then-else
#[test_case("if(true, \"foo\", \"bar\")", "\"foo\""; "if-then-else true")]
#[test_case("if(false, \"foo\", \"bar\")", "\"bar\""; "if-then-else false")]
#[test_case("if(\"baz\", \"foo\", \"bar\")", "\"foo\""; "if-then-else truthy string")]
#[test_case("if(\"\", \"foo\", \"bar\")", "\"bar\""; "if-then-else empty string")]
// test in
#[test_case("42 in (12, 22, 32, 42, 52)", "true"; "in success")]
#[test_case("42 in (62, 72, 82, 92, 12)", "false"; "in failure")]
#[test_case("42 in ()", "false"; "in empty list")]
#[test_case("42 in (42.0)", "true"; "in with numeric coercion")]
#[test_case("42 in (1/0, 42)", ""; "in with error")]
#[test_case("<tag:3> in (<tag:1>, <tag:2>, <tag:3>, <tag:4>)", "true"; "in success with IRIs")]
#[test_case("<tag:3> in (<tag:5>, <tag:6>, <tag:7>, <tag:8>)", "false"; "in failure with IRIs")]
#[test_case("<tag:3> in (\"tag:3\")", "false"; "in does not mix IRIs and strings")]
// test coalesce
#[test_case("coalesce(1, 2, 3)", "1"; "coalesce first")]
#[test_case("coalesce(1/0, 2, -\"3\")", "2"; "coalesce middle")]
#[test_case("coalesce(1/0, -\"2\", 3)", "3"; "coalesce last")]
#[test_case("coalesce(1/0, -\"2\", !(<tag:3>))", ""; "coalesce none")]
// test str nominal
#[test_case("str(<tag:x>)", "\"tag:x\""; "str for IRI")]
#[test_case("str(\"42\")", "\"42\""; "str for string")]
#[test_case("str(\"chat\"@en)", "\"chat\""; "str for language string")]
#[test_case("str(\"chat\"@en--ltr)", "\"chat\""; "str for directional language string")]
#[test_case("str(042)", "\"042\""; "str for number")]
#[test_case("str(042+1)", "\"43\""; "str for number computed")]
#[test_case("str(\"a\"^^xsd:integer)", "\"a\""; "str for ill-formed")]
// test str error
#[test_case("str(bnode())", ""; "str for bnode")]
#[test_case("str(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "str for triple")]
#[test_case("str(42/0)", ""; "str error")]
// test lang nominal
#[test_case("lang(\"42\")", "\"\""; "lang for string")]
#[test_case("lang(\"chat\"@en)", "\"en\""; "lang for language string")]
#[test_case("lang(\"chat\"@en--ltr)", "\"en\""; "lang for directional language string")]
#[test_case("lang(042)", "\"\""; "lang for number")]
#[test_case("lang(\"a\"^^xsd:integer)", "\"\""; "lang for ill-formed")]
// test lang error
#[test_case("lang(<tag:x>)", ""; "lang for IRI")]
#[test_case("lang(bnode())", ""; "lang for bnode")]
#[test_case("lang(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "lang for triple")]
#[test_case("lang(42/0)", ""; "lang error")]
// test langDir nominal
#[test_case("langDir(\"42\")", "\"\""; "langDir for string")]
#[test_case("langDir(\"chat\"@en)", "\"\""; "langDir for langDiruage string")]
#[test_case("langDir(\"chat\"@en--ltr)", "\"ltr\""; "langDir for directional langDiruage string")]
#[test_case("langDir(042)", "\"\""; "langDir for number")]
#[test_case("langDir(\"a\"^^xsd:integer)", "\"\""; "langDir for ill-formed")]
// test langDir error
#[test_case("langDir(<tag:x>)", ""; "langDir for IRI")]
#[test_case("langDir(bnode())", ""; "langDir for bnode")]
#[test_case("langDir(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "langDir for triple")]
#[test_case("langDir(42/0)", ""; "langDir error")]
// test hasLang
#[test_case("hasLang(<tag:x>)", "false"; "hasLang for IRI")]
#[test_case("hasLang(\"a b\")", "false"; "hasLang for string")]
#[test_case("hasLang(\"chat\"@en)", "true"; "hasLang for language string")]
#[test_case("hasLang(\"chat\"@en--ltr)", "true"; "hasLang for directional language string")]
#[test_case("hasLang(042)", "false"; "hasLang for number")]
#[test_case("hasLang(<<( <tag:s> <tag:p> <tag:o> )>>)", "false"; "hasLang for triple")]
#[test_case("hasLang(42/0)", ""; "hasLang error")]
// test hasLangDir
#[test_case("hasLangDir(<tag:x>)", "false"; "hasLangDir for IRI")]
#[test_case("hasLangDir(\"a b\")", "false"; "hasLangDir for string")]
#[test_case("hasLangDir(\"chat\"@en)", "false"; "hasLangDir for language string")]
#[test_case("hasLangDir(\"chat\"@en--ltr)", "true"; "hasLangDir for directional language string")]
#[test_case("hasLangDir(042)", "false"; "hasLangDir for number")]
#[test_case("hasLangDir(<<( <tag:s> <tag:p> <tag:o> )>>)", "false"; "hasLangDir for triple")]
#[test_case("hasLangDir(42/0)", ""; "hasLangDir error")]
// test datatype nominal
#[test_case("datatype(\"42\")", "xsd:string"; "datatype for string")]
#[test_case("datatype(\"chat\"@en)", "rdf:langString"; "datatype for language string")]
#[test_case("datatype(\"chat\"@en--ltr)", "rdf:dirLangString"; "datatype for directional language string")]
#[test_case("datatype(042)", "xsd:integer"; "datatype for number")]
#[test_case("datatype(\"a\"^^xsd:integer)", "xsd:integer"; "datatype for ill-formed")]
// test datatype error
#[test_case("datatype(<tag:x>)", ""; "datatype for IRI")]
#[test_case("datatype(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "datatype for triple")]
#[test_case("datatype(42/0)", ""; "datatype error")]
// test iri nominal
#[test_case("iri(<tag:x>)", "<tag:x>"; "iri for IRI")]
#[test_case("iri(\"tag:y\")", "<tag:y>"; "iri for string")]
// test iri error
#[test_case("iri(bnode())", ""; "iri for bnode")]
#[test_case("iri(\"a b\")", ""; "iri for string that is not an IRI")]
#[test_case("iri(\"tag:z\"@en)", ""; "iri for language string")]
#[test_case("iri(\"tag:z\"@en--ltr)", ""; "iri for directional language string")]
#[test_case("iri(042)", ""; "iri for number")]
#[test_case("iri(\"tag:t\"^^xsd:integer)", ""; "iri for iill-formed")]
#[test_case("iri(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "iri for triple")]
#[test_case("iri(42/0)", ""; "iri error")]
// test uri nominal
#[test_case("uri(<tag:x>)", "<tag:x>"; "uri for IRI")]
#[test_case("uri(\"tag:y\")", "<tag:y>"; "uri for string")]
// test uri error
#[test_case("uri(bnode())", ""; "uri for bnode")]
#[test_case("uri(\"a b\")", ""; "uri for string that is not an IRI")]
#[test_case("uri(\"tag:z\"@en)", ""; "uri for language string")]
#[test_case("uri(\"tag:z\"@en--ltr)", ""; "uri for directional language string")]
#[test_case("uri(042)", ""; "uri for number")]
#[test_case("uri(\"tag:t\"^^xsd:integer)", ""; "uri for iill-formed")]
#[test_case("uri(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "uri for triple")]
#[test_case("uri(42/0)", ""; "uri error")]
// test bnode nominal
#[test_case("isBlank(bnode())", "true"; "bnode no arg is bnode")]
#[test_case("bnode() = bnode()", "false"; "bnode no arg returns different values")]
#[test_case("isBlank(bnode(\"42\"))", "true"; "bnode for string")]
// test bnode error
#[test_case("isBlank(bnode(<tag:x>))", ""; "bnode for IRI")]
#[test_case("isBlank(bnode(\"chat\"@en))", ""; "bnode for language string")]
#[test_case("isBlank(bnode(\"chat\"@en--ltr))", ""; "bnode for directional language string")]
#[test_case("isBlank(bnode(042))", ""; "bnode for number")]
#[test_case("isBlank(bnode(<<( <tag:s> <tag:p> <tag:o> )>>))", ""; "bnode for triple")]
#[test_case("isBlank(bnode(42/0))", ""; "bnode for error")]
// test rand
#[test_case("datatype(rand())", "xsd:double"; "rand returns double")]
#[test_case("0 <= rand()", "true"; "rand lower bound")]
#[test_case("rand() < 1", "true"; "rand upper bound")]
#[test_case("rand() = rand()", "false"; "rand returns different values")]
// test abs nominal
#[test_case("abs(042)", "42"; "abs for positive integer")]
#[test_case("abs(3.14)", "3.14"; "abs for positive decimal")]
#[test_case("abs(3.14e0)", "3.14e0"; "abs for positive double")]
#[test_case("abs(\"1\"^^xsd:float)", "\"1e0\"^^xsd:float"; "abs for positive float")]
#[test_case("abs(-042)", "42"; "abs for netative integer")]
#[test_case("abs(-3.14)", "3.14"; "abs for netative decimal")]
#[test_case("abs(-3.14e0)", "3.14e0"; "abs for netative double")]
#[test_case("abs(\"-1\"^^xsd:float)", "\"1e0\"^^xsd:float"; "abs for netative float")]
#[test_case("abs(1e0/0)", "\"INF\"^^xsd:double"; "abs for positive INF")]
#[test_case("abs(-1e0/0)", "\"INF\"^^xsd:double"; "abs for negative INF")]
#[test_case("abs(0e0/0)", "\"NaN\"^^xsd:double"; "abs for NaN")]
// test abs error
#[test_case("abs(<tag:x>)", ""; "abs for IRI")]
#[test_case("abs(bnode())", ""; "abs for bnode")]
#[test_case("abs(\"42\")", ""; "abs for string")]
#[test_case("abs(\"chat\"@en)", ""; "abs for language string")]
#[test_case("abs(\"chat\"@en--ltr)", ""; "abs for directional language string")]
#[test_case("abs(\"a\"^^xsd:integer)", ""; "abs for ill formed")]
#[test_case("abs(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "abs for triple")]
// test ceil nominal
#[test_case("ceil(042)", "42"; "ceil for integer")]
#[test_case("ceil(1.0)", "1.0"; "ceil for decimal 1")]
#[test_case("ceil(1.4)", "2.0"; "ceil for decimal 1.4")]
#[test_case("ceil(1.5)", "2.0"; "ceil for decimal 1.5")]
#[test_case("ceil(1.6)", "2.0"; "ceil for decimal 1.6")]
#[test_case("ceil(-1.0)", "-1.0"; "ceil for decimal minus 1")]
#[test_case("ceil(-1.4)", "-1.0"; "ceil for decimal minus 1.4")]
#[test_case("ceil(-1.5)", "-1.0"; "ceil for decimal minus 1.5")]
#[test_case("ceil(-1.6)", "-1.0"; "ceil for decimal minus 1.6")]
#[test_case("ceil(1.0e0)", "1e0"; "ceil for double 1")]
#[test_case("ceil(1.4e0)", "2e0"; "ceil for double 1.4")]
#[test_case("ceil(1.5e0)", "2e0"; "ceil for double 1.5")]
#[test_case("ceil(1.6e0)", "2e0"; "ceil for double 1.6")]
#[test_case("ceil(-1.0e0)", "-1e0"; "ceil for double minus 1")]
#[test_case("ceil(-1.4e0)", "-1e0"; "ceil for double minus 1.4")]
#[test_case("ceil(-1.5e0)", "-1e0"; "ceil for double minus 1.5")]
#[test_case("ceil(-1.6e0)", "-1e0"; "ceil for double minus 1.6")]
#[test_case("ceil(\"1.0\"^^xsd:float)", "\"1e0\"^^xsd:float"; "ceil for float 1")]
#[test_case("ceil(\"1.4\"^^xsd:float)", "\"2e0\"^^xsd:float"; "ceil for float 1.4")]
#[test_case("ceil(\"1.5\"^^xsd:float)", "\"2e0\"^^xsd:float"; "ceil for float 1.5")]
#[test_case("ceil(\"1.6\"^^xsd:float)", "\"2e0\"^^xsd:float"; "ceil for float 1.6")]
#[test_case("ceil(\"-1.0\"^^xsd:float)", "\"-1e0\"^^xsd:float"; "ceil for float minus 1")]
#[test_case("ceil(\"-1.4\"^^xsd:float)", "\"-1e0\"^^xsd:float"; "ceil for float minus 1.4")]
#[test_case("ceil(\"-1.5\"^^xsd:float)", "\"-1e0\"^^xsd:float"; "ceil for float minus 1.5")]
#[test_case("ceil(\"-1.6\"^^xsd:float)", "\"-1e0\"^^xsd:float"; "ceil for float minus 1.6")]
// test ceil error
#[test_case("ceil(<tag:x>)", ""; "ceil for IRI")]
#[test_case("ceil(bnode())", ""; "ceil for bnode")]
#[test_case("ceil(\"42\")", ""; "ceil for string")]
#[test_case("ceil(\"chat\"@en)", ""; "ceil for language string")]
#[test_case("ceil(\"chat\"@en--ltr)", ""; "ceil for directional language string")]
#[test_case("ceil(\"a\"^^xsd:integer)", ""; "ceil for ill formed")]
#[test_case("ceil(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "ceil for triple")]
// test floor nominal
#[test_case("floor(042)", "42"; "floor for integer")]
#[test_case("floor(1.0)", "1.0"; "floor for decimal 1")]
#[test_case("floor(1.4)", "1.0"; "floor for decimal 1.4")]
#[test_case("floor(1.5)", "1.0"; "floor for decimal 1.5")]
#[test_case("floor(1.6)", "1.0"; "floor for decimal 1.6")]
#[test_case("floor(-1.0)", "-1.0"; "floor for decimal minus 1")]
#[test_case("floor(-1.4)", "-2.0"; "floor for decimal minus 1.4")]
#[test_case("floor(-1.5)", "-2.0"; "floor for decimal minus 1.5")]
#[test_case("floor(-1.6)", "-2.0"; "floor for decimal minus 1.6")]
#[test_case("floor(1.0e0)", "1e0"; "floor for double 1")]
#[test_case("floor(1.4e0)", "1e0"; "floor for double 1.4")]
#[test_case("floor(1.5e0)", "1e0"; "floor for double 1.5")]
#[test_case("floor(1.6e0)", "1e0"; "floor for double 1.6")]
#[test_case("floor(-1.0e0)", "-1e0"; "floor for double minus 1")]
#[test_case("floor(-1.4e0)", "-2e0"; "floor for double minus 1.4")]
#[test_case("floor(-1.5e0)", "-2e0"; "floor for double minus 1.5")]
#[test_case("floor(-1.6e0)", "-2e0"; "floor for double minus 1.6")]
#[test_case("floor(\"1.0\"^^xsd:float)", "\"1e0\"^^xsd:float"; "floor for float 1")]
#[test_case("floor(\"1.4\"^^xsd:float)", "\"1e0\"^^xsd:float"; "floor for float 1.4")]
#[test_case("floor(\"1.5\"^^xsd:float)", "\"1e0\"^^xsd:float"; "floor for float 1.5")]
#[test_case("floor(\"1.6\"^^xsd:float)", "\"1e0\"^^xsd:float"; "floor for float 1.6")]
#[test_case("floor(\"-1.0\"^^xsd:float)", "\"-1e0\"^^xsd:float"; "floor for float minus 1")]
#[test_case("floor(\"-1.4\"^^xsd:float)", "\"-2e0\"^^xsd:float"; "floor for float minus 1.4")]
#[test_case("floor(\"-1.5\"^^xsd:float)", "\"-2e0\"^^xsd:float"; "floor for float minus 1.5")]
#[test_case("floor(\"-1.6\"^^xsd:float)", "\"-2e0\"^^xsd:float"; "floor for float minus 1.6")]
// test floor error
#[test_case("floor(<tag:x>)", ""; "floor for IRI")]
#[test_case("floor(bnode())", ""; "floor for bnode")]
#[test_case("floor(\"42\")", ""; "floor for string")]
#[test_case("floor(\"chat\"@en)", ""; "floor for language string")]
#[test_case("floor(\"chat\"@en--ltr)", ""; "floor for directional language string")]
#[test_case("floor(\"a\"^^xsd:integer)", ""; "floor for ill formed")]
#[test_case("floor(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "floor for triple")]
// test round nominal
#[test_case("round(042)", "42"; "round for integer")]
#[test_case("round(1.0)", "1.0"; "round for decimal 1")]
#[test_case("round(1.4)", "1.0"; "round for decimal 1.4")]
#[test_case("round(1.5)", "2.0"; "round for decimal 1.5")]
#[test_case("round(1.6)", "2.0"; "round for decimal 1.6")]
#[test_case("round(-1.0)", "-1.0"; "round for decimal minus 1")]
#[test_case("round(-1.4)", "-1.0"; "round for decimal minus 1.4")]
#[test_case("round(-1.5)", "-2.0"; "round for decimal minus 1.5")]
#[test_case("round(-1.6)", "-2.0"; "round for decimal minus 1.6")]
#[test_case("round(1.0e0)", "1e0"; "round for double 1")]
#[test_case("round(1.4e0)", "1e0"; "round for double 1.4")]
#[test_case("round(1.5e0)", "2e0"; "round for double 1.5")]
#[test_case("round(1.6e0)", "2e0"; "round for double 1.6")]
#[test_case("round(-1.0e0)", "-1e0"; "round for double minus 1")]
#[test_case("round(-1.4e0)", "-1e0"; "round for double minus 1.4")]
#[test_case("round(-1.5e0)", "-2e0"; "round for double minus 1.5")]
#[test_case("round(-1.6e0)", "-2e0"; "round for double minus 1.6")]
#[test_case("round(\"1.0\"^^xsd:float)", "\"1e0\"^^xsd:float"; "round for float 1")]
#[test_case("round(\"1.4\"^^xsd:float)", "\"1e0\"^^xsd:float"; "round for float 1.4")]
#[test_case("round(\"1.5\"^^xsd:float)", "\"2e0\"^^xsd:float"; "round for float 1.5")]
#[test_case("round(\"1.6\"^^xsd:float)", "\"2e0\"^^xsd:float"; "round for float 1.6")]
#[test_case("round(\"-1.0\"^^xsd:float)", "\"-1e0\"^^xsd:float"; "round for float minus 1")]
#[test_case("round(\"-1.4\"^^xsd:float)", "\"-1e0\"^^xsd:float"; "round for float minus 1.4")]
#[test_case("round(\"-1.5\"^^xsd:float)", "\"-2e0\"^^xsd:float"; "round for float minus 1.5")]
#[test_case("round(\"-1.6\"^^xsd:float)", "\"-2e0\"^^xsd:float"; "round for float minus 1.6")]
// test round error
#[test_case("round(<tag:x>)", ""; "round for IRI")]
#[test_case("round(bnode())", ""; "round for bnode")]
#[test_case("round(\"42\")", ""; "round for string")]
#[test_case("round(\"chat\"@en)", ""; "round for language string")]
#[test_case("round(\"chat\"@en--ltr)", ""; "round for directional language string")]
#[test_case("round(\"a\"^^xsd:integer)", ""; "round for ill formed")]
#[test_case("round(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "round for triple")]
// test concat nominal
#[test_case("concat(\"foo\", \"bar\")", "\"foobar\"")]
#[test_case("concat(\"foo\"@en, \"bar\"@en)", "\"foobar\"@en")]
#[test_case("concat(\"foo\"@en--ltr, \"bar\"@en--ltr)", "\"foobar\"@en--ltr")]
#[test_case("concat(\"foo\"@en, \"bar\")", "\"foobar\"")]
#[test_case("concat(\"foo\", \"bar\"@en)", "\"foobar\"")]
#[test_case("concat(\"foo\"@en, \"bar\"@es)", "\"foobar\"")]
#[test_case("concat(\"foo\"@en, \"bar\"@en--ltr)", "\"foobar\"")]
#[test_case("concat(\"abc\")", "\"abc\"")]
#[test_case("concat(\"abc\"@en)", "\"abc\"@en")]
#[test_case("concat(\"abc\"@en--ltr)", "\"abc\"@en--ltr")]
#[test_case("concat()", "\"\"")]
#[test_case("concat(\"a\", \"b\", \"c\")", "\"abc\"")]
#[test_case("concat(\"a\", \"b\", \"c\", \"d\")", "\"abcd\"")]
// test concat nominal error
#[test_case("concat(<tag:x>)", ""; "concat for IRI")]
#[test_case("concat(bnode())", ""; "concat for bnode")]
#[test_case("concat(042)", ""; "concat for number")]
#[test_case("concat(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "concat for triple")]
#[test_case("concat(\"x\", <tag:x>)", ""; "concat for string and IRI")]
#[test_case("concat(\"x\", bnode())", ""; "concat for string and bnode")]
#[test_case("concat(\"x\", 042)", ""; "concat for string and number")]
#[test_case("concat(\"x\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "concat for string and triple")]
#[test_case("concat(<tag:x>, \"x\")", ""; "concat for IRI and string")]
#[test_case("concat(bnode(), \"x\")", ""; "concat for bnode and string")]
#[test_case("concat(042, \"x\")", ""; "concat for number and string")]
#[test_case("concat(<<( <tag:s> <tag:p> <tag:o> )>>, \"x\")", ""; "concat for triple and string")]
// test langMatches nominal
#[test_case("langMatches(\"en\", \"*\")", "true"; "langMatches en star true")]
#[test_case("langMatches(\"EN\", \"en\")", "true"; "langMatches en en true")]
#[test_case("langMatches(\"en-UK\", \"en\")", "true"; "langMatches enuk en")]
#[test_case("langMatches(\"en-uk\", \"en-UK\")", "true"; "langMatches enuk enuk")]
#[test_case("langMatches(\"en-US\", \"en-UK\")", "false"; "langMatches enus enuk")]
#[test_case("langMatches(\"en\", \"en-UK\")", "false"; "langMatches en enuk")]
#[test_case("langMatches(\"es\", \"en\")", "false"; "langMatches es en")]
#[test_case("langMatches(\"enx\", \"en\")", "false"; "langMatches enx es")]
// test langMatches error
#[test_case("langMatches(<tag:x>, \"en\")", ""; "langMatches for IRI")]
#[test_case("langMatches(bnode(), \"en\")", ""; "langMatches for bnode")]
#[test_case("langMatches(\"\", \"en\")", ""; "langMatches for empty string")]
#[test_case("langMatches(\"en\"@en, \"en\")", ""; "langMatches for language string")]
#[test_case("langMatches(\"en\"@en--ltr, \"en\")", ""; "langMatches for directional language string")]
#[test_case("langMatches(42, \"en\")", ""; "langMatches for number")]
#[test_case("langMatches(<<( <tag:s> <tag:p> <tag:o> )>>, \"en\")", ""; "langMatches for triple")]
#[test_case("langMatches(\"en\", <tag:x>)", ""; "langMatches for IRI as range")]
#[test_case("langMatches(\"en\", bnode())", ""; "langMatches for bnode as range")]
#[test_case("langMatches(\"en\", \"\")", ""; "langMatches for empty string as range")]
#[test_case("langMatches(\"en\", \"en\"@en)", ""; "langMatches for language string as range")]
#[test_case("langMatches(\"en\", \"en\"@en--ltr)", ""; "langMatches for directional language string as range")]
#[test_case("langMatches(\"en\", 42)", ""; "langMatches for number as range")]
#[test_case("langMatches(\"en\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "langMatches for triple as range")]
// test subStr/2
#[test_case("subStr(<tag:x>, 2)", ""; "subStr/2 for IRI")]
#[test_case("subStr(bnode(), 2)", ""; "subStr/2 for bnode")]
#[test_case("subStr(\"foobar\", 2)", "\"oobar\""; "subStr/2 for string")]
#[test_case("subStr(\"foobar\"@en, 2)", "\"oobar\"@en"; "subStr/2 for language string")]
#[test_case("subStr(\"foobar\"@en--ltr, 2)", "\"oobar\"@en--ltr"; "subStr/2 for directional language string")]
#[test_case("subStr(42, 2)", ""; "subStr/2 for number")]
#[test_case("subStr(<<( <tag:s> <tag:p> <tag:o> )>>, 2)", ""; "subStr/2 for triple")]
#[test_case("subStr(\"foobar\", <tag:x>)", ""; "subStr/2 for IRI as startLoc")]
#[test_case("subStr(\"foobar\", bnode())", ""; "subStr/2 for bnode as startLoc")]
#[test_case("subStr(\"foobar\", \"42\")", ""; "subStr/2 for string as startLoc")]
#[test_case("subStr(\"foobar\", \"42\"@en)", ""; "subStr/2 for language string as startLoc")]
#[test_case("subStr(\"foobar\", \"42\"@en--ltr)", ""; "subStr/2 for directional language string as startLoc")]
#[test_case("subStr(\"foobar\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "subStr/2 for triple as startLoc")]
// test subStr/3
#[test_case("subStr(<tag:x>, 2, 3)", ""; "subStr/3 for IRI")]
#[test_case("subStr(bnode(), 2, 3)", ""; "subStr/3 for bnode")]
#[test_case("subStr(\"foobar\", 2, 3)", "\"oob\""; "subStr/3 for string")]
#[test_case("subStr(\"foobar\"@en, 2, 3)", "\"oob\"@en"; "subStr/3 for language string")]
#[test_case("subStr(\"foobar\"@en--ltr, 2, 3)", "\"oob\"@en--ltr"; "subStr/3 for directional language string")]
#[test_case("subStr(42, 2, 3)", ""; "subStr/3 for number")]
#[test_case("subStr(<<( <tag:s> <tag:p> <tag:o> )>>, 2, 3)", ""; "subStr/3 for triple")]
#[test_case("subStr(\"foobar\", <tag:x>, 3)", ""; "subStr/3 for IRI as startLoc")]
#[test_case("subStr(\"foobar\", bnode(), 3)", ""; "subStr/3 for bnode as startLoc")]
#[test_case("subStr(\"foobar\", \"42\", 3)", ""; "subStr/3 for string as startLoc")]
#[test_case("subStr(\"foobar\", \"42\"@en, 3)", ""; "subStr/3 for language string as startLoc")]
#[test_case("subStr(\"foobar\", \"42\"@en--ltr, 3)", ""; "subStr/3 for directional language string as startLoc")]
#[test_case("subStr(\"foobar\", <<( <tag:s> <tag:p> <tag:o> )>>, 3)", ""; "subStr/3 for triple as startLoc")]
#[test_case("subStr(\"foobar\", 2, <tag:x>)", ""; "subStr/3 for IRI as end")]
#[test_case("subStr(\"foobar\", 2, bnode())", ""; "subStr/3 for bnode as end")]
#[test_case("subStr(\"foobar\", 2, \"42\")", ""; "subStr/3 for string as end")]
#[test_case("subStr(\"foobar\", 2, \"42\"@en)", ""; "subStr/3 for language string as end")]
#[test_case("subStr(\"foobar\", 2, \"42\"@en--ltr)", ""; "subStr/3 for directional language string as end")]
#[test_case("subStr(\"foobar\", 2, <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "subStr/3 for triple as end")]
// test strLen
#[test_case("strLen(<tag:x>)", ""; "strLen for IRI")]
#[test_case("strLen(bnode())", ""; "strLen for bnode")]
#[test_case("strLen(\"foobar\")", "6"; "strLen for string")]
#[test_case("strLen(\"foobar\"@en)", "6"; "strLen for language string")]
#[test_case("strLen(\"foobar\"@en--ltr)", "6"; "strLen for directional language string")]
#[test_case("strLen(42)", ""; "strLen for number")]
#[test_case("strLen(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strLen for triple")]
// test replace
#[test_case("replace(<tag:x>, \".\", \"Z\")", ""; "replace with arg being IRI")]
#[test_case("replace(bnode(), \".\", \"Z\")", ""; "replace with arg being bnode")]
#[test_case("replace(\"foobar\", \".\", \"Z\")", "\"ZZZZZZ\""; "replace with arg being string")]
#[test_case("replace(\"foobar\"@en, \".\", \"Z\")", "\"ZZZZZZ\"@en"; "replace with arg being language string")]
#[test_case("replace(\"foobar\"@en-ltr, \".\", \"Z\")", "\"ZZZZZZ\"@en-ltr"; "replace with arg being directional language string")]
#[test_case("replace(42, \".\", \"Z\")", ""; "replace with arg being number")]
#[test_case("replace(\"2025-01-18T12:34:56\"^^xsd:dateTime, \".\", \"Z\")", ""; "replace with arg being valid date")]
#[test_case("replace(\"2023-02-29T12:34:56\"^^xsd:dateTime, \".\", \"Z\")", ""; "replace with arg being invalid date")]
#[test_case("replace(<<( <tag:s> <tag:p> <tag:o> )>>, \".\", \"Z\")", ""; "replace with arg being triple")]
#[test_case("replace(\"foobarbaz\", <tag:x>, \"Z\")", ""; "replace with pattern being IRI")]
#[test_case("replace(\"foobarbaz\", bnode(), \"Z\")", ""; "replace with pattern being bnode")]
#[test_case("replace(\"foobarbaz\", \"foobar\", \"Z\")", "\"Zbaz\""; "replace with pattern being string")]
#[test_case("replace(\"foobarbaz\", \"foobar\"@en, \"Z\")", ""; "replace with pattern being language string")]
#[test_case("replace(\"foobarbaz\", \"foobar\"@en--ltr, \"Z\")", ""; "replace with pattern being directional language string")]
#[test_case("replace(\"foobarbaz\", 42, \"Z\")", ""; "replace with pattern being number")]
#[test_case("replace(\"foobarbaz\", \"2025-01-18T12:34:56\"^^xsd:dateTime, \"Z\")", ""; "replace with pattern being valid date")]
#[test_case("replace(\"foobarbaz\", \"foobar\"^^xsd:dateTime, \"Z\")", ""; "replace with pattern being invalid date")]
#[test_case("replace(\"foobarbaz\", <<( <tag:s> <tag:p> <tag:o> )>>, \"Z\")", ""; "replace with pattern being triple")]
#[test_case("replace(\"foobarbaz\", \"A\", <tag:x>)", ""; "replace with replacement being IRI")]
#[test_case("replace(\"foobarbaz\", \"A\", bnode())", ""; "replace with replacement being bnode")]
#[test_case("replace(\"foobarbaz\", \"A\", \"foobar\")", "\"foobarbaz\""; "replace with replacement being string")]
#[test_case("replace(\"foobarbaz\", \"A\", \"foobar\"@en)", ""; "replace with replacement being language string")]
#[test_case("replace(\"foobarbaz\", \"A\", \"foobar\"@en--ltr)", ""; "replace with replacement being directional language string")]
#[test_case("replace(\"foobarbaz\", \"A\", 42)", ""; "replace with replacement being number")]
#[test_case("replace(\"foobarbaz\", \"A\", \"2025-01-18T12:34:56\"^^xsd:dateTime)", ""; "replace with replacement being valid date")]
#[test_case("replace(\"foobarbaz\", \"A\", \"foobar\"^^xsd:dateTime)", ""; "replace with replacement being invalid date")]
#[test_case("replace(\"foobarbaz\", \"A\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "replace with replacement being triple")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", <tag:x>)", ""; "replace with flags being IRI")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", bnode())", ""; "replace with flags being bnode")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", \"i\")", "\"foobZrbZz\""; "replace with flags being string")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", \"i\"@en)", ""; "replace with flags being language string")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", \"i\"@en--ltr)", ""; "replace with flags being directional language string")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", 42)", ""; "replace with flags being number")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", \"2025-01-18T12:34:56\"^^xsd:dateTime)", ""; "replace with flags being valid date")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", \"i\"^^xsd:dateTime)", ""; "replace with flags being invalid date")]
#[test_case("replace(\"foobarbaz\", \"A\", \"Z\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "replace with flags being triple")]
// test uCase
#[test_case("uCase(<tag:x>)", ""; "uCase for IRI")]
#[test_case("uCase(bnode())", ""; "uCase for bnode")]
#[test_case("uCase(\"fooBAR\")", "\"FOOBAR\""; "uCase for string")]
#[test_case("uCase(\"fooBAR\"@en)", "\"FOOBAR\"@en"; "uCase for language string")]
#[test_case("uCase(\"fooBAR\"@en--ltr)", "\"FOOBAR\"@en--ltr"; "uCase for directional language string")]
#[test_case("uCase(42)", ""; "uCase for number")]
#[test_case("uCase(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "uCase for triple")]
// test lCase
#[test_case("lCase(<tag:x>)", ""; "lCase for IRI")]
#[test_case("lCase(bnode())", ""; "lCase for bnode")]
#[test_case("lCase(\"fooBAR\")", "\"foobar\""; "lCase for string")]
#[test_case("lCase(\"fooBAR\"@en)", "\"foobar\"@en"; "lCase for language string")]
#[test_case("lCase(\"fooBAR\"@en--ltr)", "\"foobar\"@en--ltr"; "lCase for directional language string")]
#[test_case("lCase(42)", ""; "lCase for number")]
#[test_case("lCase(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "lCase for triple")]
// test encode_for_uri
#[test_case("encode_for_uri(<tag:x>)", ""; "encode_for_uri for IRI")]
#[test_case("encode_for_uri(bnode())", ""; "encode_for_uri for bnode")]
#[test_case("encode_for_uri(\"hello world\")", "\"hello%20world\""; "encode_for_uri for string")]
#[test_case("encode_for_uri(\"hello world\"@en)", "\"hello%20world\""; "encode_for_uri for language string")]
#[test_case("encode_for_uri(\"hello world\"@en--ltr)", "\"hello%20world\""; "encode_for_uri for directional language string")]
#[test_case("encode_for_uri(42)", ""; "encode_for_uri for number")]
#[test_case("encode_for_uri(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "encode_for_uri for triple")]
// test contains
#[test_case("contains(<tag:xy>, <tag:x>)", ""; "contains for IRIs")]
#[test_case("contains(bnode(), bnode())", ""; "contains for bnodes")]
#[test_case("contains(\"hello world\", \"world\")", "true"; "contains for strings")]
#[test_case("contains(\"hello world\"@en, \"world\"@en)", "true"; "contains for language strings")]
#[test_case("contains(\"hello world\"@en--ltr, \"world\"@en--ltr)", "true"; "contains for directional language strings")]
#[test_case("contains(42, 2)", ""; "contains for numbers")]
#[test_case("contains(<<( <tag:s> <tag:p> <tag:o> )>>, <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "contains for triples")]
#[test_case("contains(<tag:x>, \"x\")", ""; "contains in IRI")]
#[test_case("contains(bnode(), \"\")", ""; "contains in bnode")]
#[test_case("contains(\"hello world\"@en, \"kittie\")", "false"; "contains in language string")]
#[test_case("contains(\"hello world\"@en--ltr, \"kittie\")", "false"; "contains in directional language string")]
#[test_case("contains(42, \"2\")", ""; "contains in number")]
#[test_case("contains(<<( <tag:s> <tag:p> <tag:o> )>>, \"tag:s\")", ""; "contains in triple")]
#[test_case("contains(\"tag:x\", <tag:x>)", ""; "contains IRI")]
#[test_case("contains(\"_:x\", bnode())", ""; "contains bnode")]
#[test_case("contains(\"hello world\", \"world\"@en)", ""; "contains language string")]
#[test_case("contains(\"hello world\", \"world\"@en--ltr)", ""; "contains directional language string")]
#[test_case("contains(\"42\", 42)", ""; "contains number")]
#[test_case("contains(\"<<( <tag:s> <tag:p> <tag:o> )>>\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "contains triple")]
// test strStarts
#[test_case("strStarts(<tag:xy>, <tag:x>)", ""; "strStarts for IRIs")]
#[test_case("strStarts(bnode(), bnode())", ""; "strStarts for bnodes")]
#[test_case("strStarts(\"hello world\", \"hello\")", "true"; "strStarts for strings")]
#[test_case("strStarts(\"hello world\"@en, \"hello\"@en)", "true"; "strStarts for language strings")]
#[test_case("strStarts(\"hello world\"@en--ltr, \"hello\"@en--ltr)", "true"; "strStarts for directional language strings")]
#[test_case("strStarts(42, 4)", ""; "strStarts for numbers")]
#[test_case("strStarts(<<( <tag:s> <tag:p> <tag:o> )>>, <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strStarts for triples")]
#[test_case("strStarts(<tag:x>, \"x\")", ""; "strStarts in IRI")]
#[test_case("strStarts(bnode(), \"\")", ""; "strStarts in bnode")]
#[test_case("strStarts(\"hello world\"@en, \"kittie\")", "false"; "strStarts in language string")]
#[test_case("strStarts(\"hello world\"@en--ltr, \"kittie\")", "false"; "strStarts in directional language string")]
#[test_case("strStarts(42, \"4\")", ""; "strStarts in number")]
#[test_case("strStarts(<<( <tag:s> <tag:p> <tag:o> )>>, \"tag:s\")", ""; "strStarts in triple")]
#[test_case("strStarts(\"tag:x\", <tag:x>)", ""; "strStarts IRI")]
#[test_case("strStarts(\"_:x\", bnode())", ""; "strStarts bnode")]
#[test_case("strStarts(\"hello world\", \"hello\"@en)", ""; "strStarts language string")]
#[test_case("strStarts(\"hello world\", \"hello\"@en--ltr)", ""; "strStarts directional language string")]
#[test_case("strStarts(\"42\", 42)", ""; "strStarts number")]
#[test_case("strStarts(\"<<( <tag:s> <tag:p> <tag:o> )>>\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strStarts triple")]
// test strEnds
#[test_case("strEnds(<tag:xy>, <ag:xy>)", ""; "strEnds for IRIs")]
#[test_case("strEnds(bnode(), bnode())", ""; "strEnds for bnodes")]
#[test_case("strEnds(\"hello world\", \"world\")", "true"; "strEnds for strings")]
#[test_case("strEnds(\"hello world\"@en, \"world\"@en)", "true"; "strEnds for language strings")]
#[test_case("strEnds(\"hello world\"@en--ltr, \"world\"@en--ltr)", "true"; "strEnds for directional language strings")]
#[test_case("strEnds(42, 2)", ""; "strEnds for numbers")]
#[test_case("strEnds(<<( <tag:s> <tag:p> <tag:o> )>>, <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strEnds for triples")]
#[test_case("strEnds(<tag:x>, \"t\")", ""; "strEnds in IRI")]
#[test_case("strEnds(bnode(), \"\")", ""; "strEnds in bnode")]
#[test_case("strEnds(\"hello world\"@en, \"kittie\")", "false"; "strEnds in language string")]
#[test_case("strEnds(\"hello world\"@en--ltr, \"kittie\")", "false"; "strEnds in directional language string")]
#[test_case("strEnds(42, \"2\")", ""; "strEnds in number")]
#[test_case("strEnds(<<( <tag:s> <tag:p> <tag:o> )>>, \"tag:s\")", ""; "strEnds in triple")]
#[test_case("strEnds(\"tag:x\", <tag:x>)", ""; "strEnds IRI")]
#[test_case("strEnds(\"_:x\", bnode())", ""; "strEnds bnode")]
#[test_case("strEnds(\"hello world\", \"world\"@en)", ""; "strEnds language string")]
#[test_case("strEnds(\"hello world\", \"world\"@en--ltr)", ""; "strEnds directional language string")]
#[test_case("strEnds(\"42\", 42)", ""; "strEnds number")]
#[test_case("strEnds(\"<<( <tag:s> <tag:p> <tag:o> )>>\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strEnds triple")]
// test strBefore
#[test_case("strBefore(<tag:xy>, <ag:xy>)", ""; "strBefore for IRIs")]
#[test_case("strBefore(bnode(), bnode())", ""; "strBefore for bnodes")]
#[test_case("strBefore(\"hello world\", \"world\")", "\"hello \""; "strBefore for strings")]
#[test_case("strBefore(\"hello world\"@en, \"world\"@en)", "\"hello \"@en"; "strBefore for language strings")]
#[test_case("strBefore(\"hello world\"@en--ltr, \"world\"@en--ltr)", "\"hello \"@en--ltr"; "strBefore for directional language strings")]
#[test_case("strBefore(42, 2)", ""; "strBefore for numbers")]
#[test_case("strBefore(<<( <tag:s> <tag:p> <tag:o> )>>, <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strBefore for triples")]
#[test_case("strBefore(<tag:x>, \"x\")", ""; "strBefore in IRI")]
#[test_case("strBefore(bnode(), \"\")", ""; "strBefore in bnode")]
#[test_case("strBefore(\"hello world\"@en, \"kittie\")", "\"\""; "strBefore in language string")]
#[test_case("strBefore(\"hello world\"@en--ltr, \"kittie\")", "\"\""; "strBefore in drectional language string")]
#[test_case("strBefore(42, \"2\")", ""; "strBefore in number")]
#[test_case("strBefore(<<( <tag:s> <tag:p> <tag:o> )>>, \"tag:s\")", ""; "strBefore in triple")]
#[test_case("strBefore(\"tag:x\", <tag:x>)", ""; "strBefore IRI")]
#[test_case("strBefore(\"_:x\", bnode())", ""; "strBefore bnode")]
#[test_case("strBefore(\"hello world\", \"world\"@en)", ""; "strBefore language string")]
#[test_case("strBefore(\"hello world\", \"world\"@en--ltr)", ""; "strBefore directional language string")]
#[test_case("strBefore(\"42\", 42)", ""; "strBefore number")]
#[test_case("strBefore(\"<<( <tag:s> <tag:p> <tag:o> )>>\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strBefore triple")]
// test strAfter
#[test_case("strAfter(<tag:xy>, <ag:xy>)", ""; "strAfter for IRIs")]
#[test_case("strAfter(bnode(), bnode())", ""; "strAfter for bnodes")]
#[test_case("strAfter(\"hello world\", \"hello\")", "\" world\""; "strAfter for strings")]
#[test_case("strAfter(\"hello world\"@en, \"hello\"@en)", "\" world\"@en"; "strAfter for language strings")]
#[test_case("strAfter(\"hello world\"@en--ltr, \"hello\"@en--ltr)", "\" world\"@en--ltr"; "strAfter for directional language strings")]
#[test_case("strAfter(42, 4)", ""; "strAfter for numbers")]
#[test_case("strAfter(<<( <tag:s> <tag:p> <tag:o> )>>, <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strAfter for triples")]
#[test_case("strAfter(<tag:x>, \"t\")", ""; "strAfter in IRI")]
#[test_case("strAfter(bnode(), \"\")", ""; "strAfter in bnode")]
#[test_case("strAfter(\"hello world\"@en, \"kittie\")", "\"\""; "strAfter in language string")]
#[test_case("strAfter(\"hello world\"@en--ltr, \"kittie\")", "\"\""; "strAfter in directional language string")]
#[test_case("strAfter(42, \"4\")", ""; "strAfter in number")]
#[test_case("strAfter(<<( <tag:s> <tag:p> <tag:o> )>>, \"tag:s\")", ""; "strAfter in triple")]
#[test_case("strAfter(\"tag:x\", <tag:x>)", ""; "strAfter IRI")]
#[test_case("strAfter(\"_:x\", bnode())", ""; "strAfter bnode")]
#[test_case("strAfter(\"hello world\", \"hello\"@en)", ""; "strAfter language string")]
#[test_case("strAfter(\"hello world\", \"hello\"@en--ltr)", ""; "strAfter directional language string")]
#[test_case("strAfter(\"42\", 42)", ""; "strAfter number")]
#[test_case("strAfter(\"<<( <tag:s> <tag:p> <tag:o> )>>\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strAfter triple")]
// test year
#[test_case("year(<tag:x>)", ""; "year for IRI")]
#[test_case("year(bnode())", ""; "year for bnode")]
#[test_case("year(\"foobar\")", ""; "year for string")]
#[test_case("year(\"foobar\"@en)", ""; "year for language string")]
#[test_case("year(\"foobar\"@en--ltr)", ""; "year for directional language string")]
#[test_case("year(42)", ""; "year for number")]
#[test_case("year(\"2025-01-18T12:34:56\"^^xsd:dateTime)", "2025"; "year for valid date")]
#[test_case("year(\"2023-02-29T12:34:56\"^^xsd:dateTime)", ""; "year for invalid date")]
#[test_case("year(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "year for triple")]
// test month
#[test_case("month(<tag:x>)", ""; "month for IRI")]
#[test_case("month(bnode())", ""; "month for bnode")]
#[test_case("month(\"foobar\")", ""; "month for string")]
#[test_case("month(\"foobar\"@en)", ""; "month for language string")]
#[test_case("month(\"foobar\"@en--ltr)", ""; "month for drectional language string")]
#[test_case("month(42)", ""; "month for number")]
#[test_case("month(\"2025-01-18T12:34:56\"^^xsd:dateTime)", "1"; "month for valid date")]
#[test_case("month(\"2023-02-29T12:34:56\"^^xsd:dateTime)", ""; "month for invalid date")]
#[test_case("month(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "month for triple")]
// test day
#[test_case("day(<tag:x>)", ""; "day for IRI")]
#[test_case("day(bnode())", ""; "day for bnode")]
#[test_case("day(\"foobar\")", ""; "day for string")]
#[test_case("day(\"foobar\"@en)", ""; "day for language string")]
#[test_case("day(\"foobar\"@en--ltr)", ""; "day for directional language string")]
#[test_case("day(42)", ""; "day for number")]
#[test_case("day(\"2025-01-18T12:34:56\"^^xsd:dateTime)", "18"; "day for valid date")]
#[test_case("day(\"2023-02-29T12:34:56\"^^xsd:dateTime)", ""; "day for invalid date")]
#[test_case("day(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "day for triple")]
// test hours
#[test_case("hours(<tag:x>)", ""; "hours for IRI")]
#[test_case("hours(bnode())", ""; "hours for bnode")]
#[test_case("hours(\"foobar\")", ""; "hours for string")]
#[test_case("hours(\"foobar\"@en)", ""; "hours for language string")]
#[test_case("hours(\"foobar\"@en--ltr)", ""; "hours for directional language string")]
#[test_case("hours(42)", ""; "hours for number")]
#[test_case("hours(\"2025-01-18T12:34:56\"^^xsd:dateTime)", "12"; "hours for valid date")]
#[test_case("hours(\"2023-02-29T12:34:56\"^^xsd:dateTime)", ""; "hours for invalid date")]
#[test_case("hours(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "hours for triple")]
// test minutes
#[test_case("minutes(<tag:x>)", ""; "minutes for IRI")]
#[test_case("minutes(bnode())", ""; "minutes for bnode")]
#[test_case("minutes(\"foobar\")", ""; "minutes for string")]
#[test_case("minutes(\"foobar\"@en)", ""; "minutes for language string")]
#[test_case("minutes(\"foobar\"@en--ltr)", ""; "minutes for directional language string")]
#[test_case("minutes(42)", ""; "minutes for number")]
#[test_case("minutes(\"2025-01-18T12:34:56\"^^xsd:dateTime)", "34"; "minutes for valid date")]
#[test_case("minutes(\"2023-02-29T12:34:56\"^^xsd:dateTime)", ""; "minutes for invalid date")]
#[test_case("minutes(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "minutes for triple")]
// test seconds
#[test_case("seconds(<tag:x>)", ""; "seconds for IRI")]
#[test_case("seconds(bnode())", ""; "seconds for bnode")]
#[test_case("seconds(\"foobar\")", ""; "seconds for string")]
#[test_case("seconds(\"foobar\"@en)", ""; "seconds for language string")]
#[test_case("seconds(\"foobar\"@en--ltr)", ""; "seconds for directional language string")]
#[test_case("seconds(42)", ""; "seconds for number")]
#[test_case("seconds(\"2025-01-18T12:34:56\"^^xsd:dateTime)", "56.0"; "seconds for valid date")]
#[test_case("seconds(\"2023-02-29T12:34:56\"^^xsd:dateTime)", ""; "seconds for invalid date")]
#[test_case("seconds(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "seconds for triple")]
// test timezone
#[test_case("timezone(<tag:x>)", ""; "timezone for IRI")]
#[test_case("timezone(bnode())", ""; "timezone for bnode")]
#[test_case("timezone(\"foobar\")", ""; "timezone for string")]
#[test_case("timezone(\"foobar\"@en)", ""; "timezone for language string")]
#[test_case("timezone(\"foobar\"@en--ltr)", ""; "timezone for directional language string")]
#[test_case("timezone(42)", ""; "timezone for number")]
#[test_case("timezone(\"2025-01-18T12:34:56Z\"^^xsd:dateTime)", "\"PT0S\"^^xsd:dayTimeDuration"; "timezone for valid date")]
#[test_case("timezone(\"2023-02-29T12:34:56Z\"^^xsd:dateTime)", ""; "timezone for invalid date")]
#[test_case("timezone(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "timezone for triple")]
// test tz
#[test_case("tz(<tag:x>)", ""; "tz for IRI")]
#[test_case("tz(bnode())", ""; "tz for bnode")]
#[test_case("tz(\"foobar\")", ""; "tz for string")]
#[test_case("tz(\"foobar\"@en)", ""; "tz for language string")]
#[test_case("tz(\"foobar\"@en--ltr)", ""; "tz for directional language string")]
#[test_case("tz(42)", ""; "tz for number")]
#[test_case("tz(\"2025-01-18T12:34:56Z\"^^xsd:dateTime)", "\"Z\""; "tz for valid date")]
#[test_case("tz(\"2023-02-29T12:34:56Z\"^^xsd:dateTime)", ""; "tz for invalid date")]
#[test_case("tz(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "tz for triple")]
// test now
#[test_case("datatype(now()) = xsd:dateTime", "true"; "now returns an xsd:dateTime")]
#[test_case("now() = now()", "true"; "now always returns the same value in one query execution")]
// test uuid
#[test_case("isIri(uuid())", "true"; "uuid returns an IRI")]
#[test_case("regex(str(uuid()), \"^urn:uuid:[0-9a-f-]+$\", \"i\")", "true"; "uuid returns a UUID URN")]
#[test_case("uuid() != uuid()", "true"; "uuid always returns a different value")]
// test strUuid
#[test_case("datatype(strUuid()) = xsd:string", "true"; "strUuid returns a string")]
#[test_case("regex(strUuid(), \"^[0-9a-f-]+$\", \"i\")", "true"; "strUuid returns a UUID")]
#[test_case("strUuid() != strUuid()", "true"; "strUuid always returns a different value")]
// test md5
#[test_case("md5(<tag:x>)", ""; "md5 for IRI")]
#[test_case("md5(\"a b\")", "\"0cc9cd4dd26c5137b675a0d819cb9ab0\""; "md5 for string")]
#[test_case("md5(\"chat\"@en)", ""; "md5 for language string")]
#[test_case("md5(\"chat\"@en--ltr)", ""; "md5 for directional language string")]
#[test_case("md5(042)", ""; "md5 for number")]
#[test_case("md5(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "md5 for triple")]
#[test_case("md5(42/0)", ""; "md5 error")]
// test sha1
#[test_case("sha1(<tag:x>)", ""; "sha1 for IRI")]
#[test_case("sha1(\"a b\")", "\"7dbde93504122a707f849f2c12bdd9de71b41929\""; "sha1 for string")]
#[test_case("sha1(\"chat\"@en)", ""; "sha1 for language string")]
#[test_case("sha1(\"chat\"@en--ltr)", ""; "sha1 for directional language string")]
#[test_case("sha1(042)", ""; "sha1 for number")]
#[test_case("sha1(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "sha1 for triple")]
#[test_case("sha1(42/0)", ""; "sha1 error")]
// test sha256
#[test_case("sha256(<tag:x>)", ""; "sha256 for IRI")]
#[test_case("sha256(\"a b\")", "\"c8687a08aa5d6ed2044328fa6a697ab8e96dc34291e8c2034ae8c38e6fcc6d65\""; "sha256 for string")]
#[test_case("sha256(\"chat\"@en)", ""; "sha256 for language string")]
#[test_case("sha256(\"chat\"@en--ltr)", ""; "sha256 for directional language string")]
#[test_case("sha256(042)", ""; "sha256 for number")]
#[test_case("sha256(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "sha256 for triple")]
#[test_case("sha256(42/0)", ""; "sha256 error")]
// test sha384
#[test_case("sha384(<tag:x>)", ""; "sha384 for IRI")]
#[test_case("sha384(\"a b\")", "\"6a6a7cf361ea861673606c8b77e34a30a24753102b3d1ff2337bd11d533c301fe6f49c9e4c1ecf54f3dcb7a833f66c27\""; "sha384 for string")]
#[test_case("sha384(\"chat\"@en)", ""; "sha384 for language string")]
#[test_case("sha384(\"chat\"@en--ltr)", ""; "sha384 for directional language string")]
#[test_case("sha384(042)", ""; "sha384 for number")]
#[test_case("sha384(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "sha384 for triple")]
#[test_case("sha384(42/0)", ""; "sha384 error")]
// test sha512
#[test_case("sha512(<tag:x>)", ""; "sha512 for IRI")]
#[test_case("sha512(\"a b\")", "\"7d42b489f17d3adadff1f4e395c03885165ea5ca63ef99a6f075b04c01011c11e14f9527b4f056eafc9f3958b91513a59b788e012263a6f792858c11007d250c\""; "sha512 for string")]
#[test_case("sha512(\"chat\"@en)", ""; "sha512 for language string")]
#[test_case("sha512(\"chat\"@en--ltr)", ""; "sha512 for directional language string")]
#[test_case("sha512(042)", ""; "sha512 for number")]
#[test_case("sha512(<<( <tag:s> <tag:p> <tag:o> )>>)", ""; "sha512 for triple")]
#[test_case("sha512(42/0)", ""; "sha512 error")]
// test strLang
#[test_case("strLang(<tag:xy>, \"en\")", ""; "strLang for lex IRIs")]
#[test_case("strLang(bnode(), \"en\")", ""; "strLang for lex bnodes")]
#[test_case("strLang(\"hello world\", \"en\")", "\"hello world\"@en"; "strLang for lex strings")]
#[test_case("strLang(\"hello world\"@en, \"en\")", ""; "strLang for lex language strings")]
#[test_case("strLang(\"hello world\"@en--ltr, \"en\")", ""; "strLang for lex directional language strings")]
#[test_case("strLang(42, \"en\")", ""; "strLang for lex numbers")]
#[test_case("strLang(<<( <tag:s> <tag:p> <tag:o> )>>, \"en\")", ""; "strLang for lex triples")]
#[test_case("strLang(\"hello world\", <tag:x>)", ""; "strLang for lang IRI")]
#[test_case("strLang(\"hello world\", bnode())", ""; "strLang for lang bnode")]
#[test_case("strLang(\"hello world\", \"en\"@fr)", ""; "strLang for lang language string")]
#[test_case("strLang(\"hello world\", \"en\"@fr--ltr)", ""; "strLang for lang directional language string")]
#[test_case("strLang(\"hello world\", 42)", ""; "strLang for lang number")]
#[test_case("strLang(\"hello world\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strLang for lang triple")]
// test strLangDir
#[test_case("strLangDir(<tag:xy>, \"en\", \"ltr\")", ""; "strLangDir for lex IRIs")]
#[test_case("strLangDir(bnode(), \"en\", \"ltr\")", ""; "strLangDir for lex bnodes")]
#[test_case("strLangDir(\"hello world\", \"en\", \"ltr\")", "\"hello world\"@en--ltr"; "strLangDir for lex strings")]
#[test_case("strLangDir(\"hello world\"@en, \"en\", \"ltr\")", ""; "strLangDir for lex language strings")]
#[test_case("strLangDir(\"hello world\"@en--ltr, \"en\", \"ltr\")", ""; "strLangDir for lex directional language strings")]
#[test_case("strLangDir(42, \"en\", \"ltr\")", ""; "strLangDir for lex numbers")]
#[test_case("strLangDir(<<( <tag:s> <tag:p> <tag:o> )>>, \"en\", \"ltr\")", ""; "strLangDir for lex triples")]
#[test_case("strLangDir(\"hello world\", <tag:x>, \"ltr\")", ""; "strLangDir for lang IRI")]
#[test_case("strLangDir(\"hello world\", bnode(), \"ltr\")", ""; "strLangDir for lang bnode")]
#[test_case("strLangDir(\"hello world\", \"en\"@fr, \"ltr\")", ""; "strLangDir for lang language string")]
#[test_case("strLangDir(\"hello world\", \"en\"@fr--ltr, \"ltr\")", ""; "strLangDir for lang directional language string")]
#[test_case("strLangDir(\"hello world\", 42, \"ltr\")", ""; "strLangDir for lang number")]
#[test_case("strLangDir(\"hello world\", <<( <tag:s> <tag:p> <tag:o> )>>, \"ltr\")", ""; "strLangDir for lang triple")]
#[test_case("strLangDir(\"hello world\", \"en\", <tag:x>)", ""; "strLangDir for dir IRI")]
#[test_case("strLangDir(\"hello world\", \"en\", bnode())", ""; "strLangDir for dir bnode")]
#[test_case("strLangDir(\"hello world\", \"en\", \"en\"@fr)", ""; "strLangDir for dir language string")]
#[test_case("strLangDir(\"hello world\", \"en\", \"en\"@fr--ltr)", ""; "strLangDir for dir directional language string")]
#[test_case("strLangDir(\"hello world\", \"en\", 42)", ""; "strLangDir for dir number")]
#[test_case("strLangDir(\"hello world\", \"en\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strLangDir for dir triple")]
// test strDt
#[test_case("strDt(<tag:xy>, <tag:dt>)", ""; "strDt for lex IRIs")]
#[test_case("strDt(bnode(), <tag:dt>)", ""; "strDt for lex bnodes")]
#[test_case("strDt(\"hello world\", <tag:dt>)", "\"hello world\"^^<tag:dt>"; "strDt for lex strings")]
#[test_case("strDt(\"hello world\"@en, <tag:dt>)", ""; "strDt for lex language strings")]
#[test_case("strDt(\"hello world\"@en--ltr, <tag:dt>)", ""; "strDt for lex directional language strings")]
#[test_case("strDt(42, <tag:dt>)", ""; "strDt for lex numbers")]
#[test_case("strDt(<<( <tag:s> <tag:p> <tag:o> )>>, <tag:dt>)", ""; "strDt for lex triples")]
#[test_case("strDt(\"hello world\", bnode())", ""; "strDt for lang bnode")]
#[test_case("strDt(\"hello world\", \"en\")", ""; "strDt for lang string")]
#[test_case("strDt(\"hello world\", \"en\"@fr)", ""; "strDt for lang language string")]
#[test_case("strDt(\"hello world\", \"en\"@fr--ltr)", ""; "strDt for lang directional language string")]
#[test_case("strDt(\"hello world\", 42)", ""; "strDt for lang number")]
#[test_case("strDt(\"hello world\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "strDt for lang triple")]
// test isIri
#[test_case("isIri(<tag:x>)", "true"; "isIri for IRI")]
#[test_case("isIri(\"a b\")", "false"; "isIri for string")]
#[test_case("isIri(\"chat\"@en)", "false"; "isIri for language string")]
#[test_case("isIri(\"chat\"@en--ltr)", "false"; "isIri for directional language string")]
#[test_case("isIri(042)", "false"; "isIri for number")]
#[test_case("isIri(<<( <tag:s> <tag:p> <tag:o> )>>)", "false"; "isIri for triple")]
#[test_case("isIri(42/0)", ""; "isIri error")]
// test iBlank
#[test_case("isBlank(<tag:x>)", "false"; "isBlank for IRI")]
#[test_case("isBlank(\"a b\")", "false"; "isBlank for string")]
#[test_case("isBlank(\"chat\"@en)", "false"; "isBlank for language string")]
#[test_case("isBlank(\"chat\"@en--ltr)", "false"; "isBlank for directional language string")]
#[test_case("isBlank(042)", "false"; "isBlank for number")]
#[test_case("isBlank(<<( <tag:s> <tag:p> <tag:o> )>>)", "false"; "isBlank for triple")]
#[test_case("isBlank(42/0)", ""; "isBlank error")]
// test isLieteral
#[test_case("isLiteral(<tag:x>)", "false"; "isLiteral for IRI")]
#[test_case("isLiteral(\"a b\")", "true"; "isLiteral for string")]
#[test_case("isLiteral(\"chat\"@en)", "true"; "isLiteral for language string")]
#[test_case("isLiteral(\"chat\"@en--ltr)", "true"; "isLiteral for directional language string")]
#[test_case("isLiteral(042)", "true"; "isLiteral for number")]
#[test_case("isLiteral(<<( <tag:s> <tag:p> <tag:o> )>>)", "false"; "isLiteral for triple")]
#[test_case("isLiteral(42/0)", ""; "isLiteral error")]
// test isNumeric
#[test_case("isNumeric(<tag:x>)", "false"; "isNumeric for IRI")]
#[test_case("isNumeric(\"a b\")", "false"; "isNumeric for string")]
#[test_case("isNumeric(\"chat\"@en)", "false"; "isNumeric for language string")]
#[test_case("isNumeric(\"chat\"@en--ltr)", "false"; "isNumeric for directional language string")]
#[test_case("isNumeric(042)", "true"; "isNumeric for integer")]
#[test_case("isNumeric(3.14)", "true"; "isNumeric for decimal")]
#[test_case("isNumeric(3.14e0)", "true"; "isNumeric for double")]
#[test_case("isNumeric(\"1\"^^xsd:float)", "true"; "isNumeric for float")]
#[test_case("isNumeric(\"a\"^^xsd:integer)", "false"; "isNumeric for malformed")]
#[test_case("isNumeric(<<( <tag:s> <tag:p> <tag:o> )>>)", "false"; "isNumeric for triple")]
#[test_case("isNumeric(42/0)", ""; "isNumeric error")]
// test regex
#[test_case("regex(<tag:x>, \".\")", ""; "regex with text being IRI")]
#[test_case("regex(bnode(), \".\")", ""; "regex with text being bnode")]
#[test_case("regex(\"foobar\", \".\")", "true"; "regex with text being string")]
#[test_case("regex(\"foobar\"@en, \".\")", "true"; "regex with text being language string")]
#[test_case("regex(\"foobar\"@en--ltr, \".\")", "true"; "regex with text being directional language string")]
#[test_case("regex(42, \".\")", ""; "regex with text being number")]
#[test_case("regex(\"2025-01-18T12:34:56\"^^xsd:dateTime, \".\")", ""; "regex with text being valid date")]
#[test_case("regex(\"2023-02-29T12:34:56\"^^xsd:dateTime, \".\")", ""; "regex with text being invalid date")]
#[test_case("regex(<<( <tag:s> <tag:p> <tag:o> )>>, \".\")", ""; "regex with text being triple")]
#[test_case("regex(\"foobarbaz\", <tag:x>)", ""; "regex with pattern being IRI")]
#[test_case("regex(\"foobarbaz\", bnode())", ""; "regex with pattern being bnode")]
#[test_case("regex(\"foobarbaz\", \"foobar\")", "true"; "regex with pattern being string")]
#[test_case("regex(\"foobarbaz\", \"foobar\"@en)", ""; "regex with pattern being language string")]
#[test_case("regex(\"foobarbaz\", \"foobar\"@en--ltr)", ""; "regex with pattern being directional language string")]
#[test_case("regex(\"foobarbaz\", 42)", ""; "regex with pattern being number")]
#[test_case("regex(\"foobarbaz\", \"2025-01-18T12:34:56\"^^xsd:dateTime)", ""; "regex with pattern being valid date")]
#[test_case("regex(\"foobarbaz\", \"foobar\"^^xsd:dateTime)", ""; "regex with pattern being invalid date")]
#[test_case("regex(\"foobarbaz\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "regex with pattern being triple")]
#[test_case("regex(\"foobarbaz\", \".\", <tag:x>)", ""; "regex with flags being IRI")]
#[test_case("regex(\"foobarbaz\", \".\", bnode())", ""; "regex with flags being bnode")]
#[test_case("regex(\"foobarbaz\", \".\", \"i\")", "true"; "regex with flags being string")]
#[test_case("regex(\"foobarbaz\", \".\", \"i\"@en)", ""; "regex with flags being language string")]
#[test_case("regex(\"foobarbaz\", \".\", \"i\"@en--ltr)", ""; "regex with flags being directional language string")]
#[test_case("regex(\"foobarbaz\", \".\", 42)", ""; "regex with flags being number")]
#[test_case("regex(\"foobarbaz\", \".\", \"2025-01-18T12:34:56\"^^xsd:dateTime)", ""; "regex with flags being valid date")]
#[test_case("regex(\"foobarbaz\", \".\", \"i\"^^xsd:dateTime)", ""; "regex with flags being invalid date")]
#[test_case("regex(\"foobarbaz\", \".\", <<( <tag:s> <tag:p> <tag:o> )>>)", ""; "regex with flags being triple")]
// test triple
#[test_case("triple(<tag:s>, <tag:p>, <tag:o>)", "<<( <tag:s> <tag:p> <tag:o> )>>"; "triple IRIs")]
#[test_case("triple(<tag:s>, <tag:p>, \"o\")", "<<( <tag:s> <tag:p> \"o\" )>>"; "triple literal object")]
#[test_case("isTriple(triple(bnode(), <tag:p>, <tag:o>))", "true"; "triple bnode subject")]
#[test_case("isTriple(triple(<tag:s>, <tag:p>, bnode()))", "true"; "triple bnode object")]
#[test_case("triple(\"s\", <tag:p>, <tag:o>)", ""; "triple literal subject errs")]
#[test_case("triple(<tag:s>, \"p\", <tag:o>)", ""; "triple literal predicate errs")]
#[test_case("triple(<tag:s>, bnode(), <tag:o>)", ""; "triple bnode predicate errs")]
// test subject
#[test_case("subject(<tag:x>)", ""; "subject for IRI")]
#[test_case("subject(bnode())", ""; "subject for bnode")]
#[test_case("subject(\"a b\")", ""; "subject for string")]
#[test_case("subject(\"chat\"@en)", ""; "subject for language string")]
#[test_case("subject(\"chat\"@en--ltr)", ""; "subject for directional language string")]
#[test_case("subject(042)", ""; "subject for number")]
#[test_case("subject(<<( <tag:s> <tag:p> <tag:o> )>>)", "<tag:s>"; "subject for triple")]
#[test_case("subject(42/0)", ""; "subject error")]
// test predicate
#[test_case("predicate(<tag:x>)", ""; "predicate for IRI")]
#[test_case("predicate(bnode())", ""; "predicate for bnode")]
#[test_case("predicate(\"a b\")", ""; "predicate for string")]
#[test_case("predicate(\"chat\"@en)", ""; "predicate for language string")]
#[test_case("predicate(\"chat\"@en--ltr)", ""; "predicate for directional language string")]
#[test_case("predicate(042)", ""; "predicate for number")]
#[test_case("predicate(<<( <tag:s> <tag:p> <tag:o> )>>)", "<tag:p>"; "predicate for triple")]
#[test_case("predicate(42/0)", ""; "predicate error")]
// test object
#[test_case("object(<tag:x>)", ""; "object for IRI")]
#[test_case("object(bnode())", ""; "object for bnode")]
#[test_case("object(\"a b\")", ""; "object for string")]
#[test_case("object(\"chat\"@en)", ""; "object for language string")]
#[test_case("object(\"chat\"@en--ltr)", ""; "object for directional language string")]
#[test_case("object(042)", ""; "object for number")]
#[test_case("object(<<( <tag:s> <tag:p> <tag:o> )>>)", "<tag:o>"; "object for triple")]
#[test_case("object(42/0)", ""; "object error")]
// test isTriple
#[test_case("isTriple(<tag:x>)", "false"; "isTriple for IRI")]
#[test_case("isTriple(bnode())", "false"; "isTriple for bnode")]
#[test_case("isTriple(\"a b\")", "false"; "isTriple for string")]
#[test_case("isTriple(\"chat\"@en)", "false"; "isTriple for language string")]
#[test_case("isTriple(\"chat\"@en--ltr)", "false"; "isTriple for directional language string")]
#[test_case("isTriple(042)", "false"; "isTriple for number")]
#[test_case("isTriple(<<( <tag:s> <tag:p> <tag:o> )>>)", "true"; "isTriple for triple")]
#[test_case("isTriple(42/0)", ""; "isTriple error")]
// test xsd:boolean cast
#[test_case("xsd:boolean()", ""; "xsd:boolean with 0 args")]
#[test_case("xsd:boolean(1)", "true"; "xsd:boolean with 1 arg")]
#[test_case("xsd:boolean(1, 2)", ""; "xsd:boolean with 2 args")]
// test xsd:double cast
#[test_case("xsd:double()", ""; "xsd:double with 0 args")]
#[test_case("xsd:double(1)", "1e0"; "xsd:double with 1 arg")]
#[test_case("xsd:double(1, 2)", ""; "xsd:double with 2 args")]
// test xsd:float cast
#[test_case("xsd:float()", ""; "xsd:float with 0 args")]
#[test_case("xsd:float(1)", "\"1e0\"^^xsd:float"; "xsd:float with 1 arg")]
#[test_case("xsd:float(1, 2)", ""; "xsd:float with 2 args")]
// test xsd:decimal cast
#[test_case("xsd:decimal()", ""; "xsd:decimal with 0 args")]
#[test_case("xsd:decimal(1)", "1.0"; "xsd:decimal with 1 arg")]
#[test_case("xsd:decimal(1, 2)", ""; "xsd:decimal with 2 args")]
// test xsd:integer cast
#[test_case("xsd:integer()", ""; "xsd:integer with 0 args")]
#[test_case("xsd:integer(1)", "1"; "xsd:integer with 1 arg")]
#[test_case("xsd:integer(1, 2)", ""; "xsd:integer with 2 args")]
// test xsd:dateTime cast
#[test_case("xsd:dateTime()", ""; "xsd:dateTime with 0 args")]
#[test_case("xsd:dateTime(\"2025-05-20T01:02:03\")", "\"2025-05-20T01:02:03\"^^xsd:dateTime"; "xsd:dateTime with 1 arg")]
#[test_case("xsd:dateTime(1, 2)", ""; "xsd:dateTime with 2 args")]
// test xsd:string cast
#[test_case("xsd:string()", ""; "xsd:string with 0 args")]
#[test_case("xsd:string(1)", "\"1\""; "xsd:string with 1 arg")]
#[test_case("xsd:string(1, 2)", ""; "xsd:string with 2 args")]
fn test_expr(expr: &str, result: &str) -> TestResult {
    let exp = if result.is_empty() {
        String::new()
    } else {
        eval_expr(result)?
    };
    assert_eq!(eval_expr(dbg!(expr))?, exp);
    Ok(())
}

#[test_case("42", "042", Some(true))]
#[test_case("42", "42.0", Some(true))]
#[test_case("42", "42e0", Some(true))]
#[test_case("42.0", "42e0", Some(true))]
#[test_case("42", "43", Some(false))]
#[test_case("\"a\"", "\"\"", Some(false))]
#[test_case("\"a\"@en", "\"\"@en", Some(false))]
#[test_case("\"a\"@en", "\"a\"@fr", Some(false))]
#[test_case("\"a\"@en", "\"a\"@en--ltr", Some(false))]
#[test_case("true", "false", Some(false))]
#[test_case(
    "\"2024-03-25T00:00:00\"^^xsd:dateTime",
    "\"2024-03-25T00:00:00+00:00\"^^xsd:dateTime",
    None
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:00+00:00\"^^xsd:dateTime",
    Some(true)
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T01:00:00+01:00\"^^xsd:dateTime",
    Some(true)
)]
#[test_case(
    "\"2024-03-25T00:00:00\"^^xsd:dateTime",
    "\"2024-03-25T00:00:01\"^^xsd:dateTime",
    Some(false)
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:01Z\"^^xsd:dateTime",
    Some(false)
)]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:00+01:00\"^^xsd:dateTime",
    Some(false)
)]
#[test_case("<tag:x>", "<tag:y>", Some(false))]
#[test_case("\"a\"^^<tag:x>", "\"a\"^^<tag:y>", None)]
#[test_case("\"a\"^^<tag:x>", "\"b\"^^<tag:x>", None)]
#[allow(clippy::similar_names)]
fn test_expr_eq(expr1: &str, expr2: &str, exp: Option<bool>) -> TestResult {
    dbg!(expr1, expr2);
    // control: every term is equal to itself
    assert_eq!(eval_expr(&format!("{expr1} = {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} != {expr1}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr2} = {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} != {expr2}"))?, FALSE);
    // control: every recognized value is equal to itself via comparison operators
    if !expr1.contains("<tag:") {
        assert_eq!(eval_expr(&format!("{expr1} <= {expr1}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} >= {expr1}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} < {expr1}"))?, FALSE);
        assert_eq!(eval_expr(&format!("{expr1} > {expr1}"))?, FALSE);

        assert_eq!(eval_expr(&format!("{expr2} <= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr2} >= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr2} < {expr2}"))?, FALSE);
        assert_eq!(eval_expr(&format!("{expr2} > {expr2}"))?, FALSE);
    }

    let (exp_eq, exp_neq) = match exp {
        Some(true) => (TRUE, FALSE),
        Some(false) => (FALSE, TRUE),
        None => ("", ""),
    };
    assert_eq!(eval_expr(&format!("{expr1} = {expr2}"))?, exp_eq);
    assert_eq!(eval_expr(&format!("{expr1} != {expr2}"))?, exp_neq);
    assert_eq!(eval_expr(&format!("sameTerm({expr1}, {expr2})"))?, FALSE);
    assert_eq!(eval_expr(&format!("sameTerm({expr1}, {expr1})"))?, TRUE);
    assert_eq!(eval_expr(&format!("sameTerm({expr2}, {expr2})"))?, TRUE);
    if exp == Some(true) {
        assert_eq!(eval_expr(&format!("{expr1} <= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} >= {expr2}"))?, TRUE);
        assert_eq!(eval_expr(&format!("{expr1} < {expr2}"))?, FALSE);
        assert_eq!(eval_expr(&format!("{expr1} > {expr2}"))?, FALSE);
    }
    Ok(())
}

#[test_case("42", "43")]
#[test_case("42", "43.0")]
#[test_case("42", "43e0")]
#[test_case("42.0", "43e0")]
#[test_case("\"\"", "\"a\"")]
#[test_case("\"a\"", "\"ab\"")]
#[test_case("\"a\"", "\"b\"")]
#[test_case("\"10\"", "\"2\"")]
#[test_case("\"\"@en", "\"a\"@en")]
#[test_case("\"a\"@en", "\"ab\"@en")]
#[test_case("\"a\"@en", "\"b\"@en")]
#[test_case("\"10\"@en", "\"b\"@en")]
#[test_case("\"\"@en--ltr", "\"a\"@en--ltr")]
#[test_case("\"a\"@en--ltr", "\"ab\"@en--ltr")]
#[test_case("\"a\"@en--ltr", "\"b\"@en--ltr")]
#[test_case("\"10\"@en--ltr", "\"b\"@en--ltr")]
#[test_case("false", "true")]
#[test_case(
    "\"2024-03-25T00:00:00Z\"^^xsd:dateTime",
    "\"2024-03-25T00:00:01Z\"^^xsd:dateTime"
)]
fn test_expr_lt(expr1: &str, expr2: &str) -> TestResult {
    assert_eq!(eval_expr(&format!("{expr1} < {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} <= {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} != {expr2}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr1} > {expr2}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr1} >= {expr2}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr1} = {expr2}"))?, FALSE);
    //
    assert_eq!(eval_expr(&format!("{expr2} < {expr1}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr2} <= {expr1}"))?, FALSE);
    assert_eq!(eval_expr(&format!("{expr2} != {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} > {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} >= {expr1}"))?, TRUE);
    assert_eq!(eval_expr(&format!("{expr2} = {expr1}"))?, FALSE);
    Ok(())
}

#[test]
fn test_is_blank() -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(
        "PREFIX s: <http://schema.org/> SELECT ?x {{ ?x s:name ?n. FILTER (isBlank(?x)) }}",
    )?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings, 1);
    got.sort();
    assert_eq!(vec!["_:b"], got);
    Ok(())
}

fn eval_expr(expr: &str) -> TestResult<String> {
    eprintln!("eval_expr: {expr}");
    let dataset = LightDataset::default();
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(&format!(
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ({expr} as ?x) {{}}"
    ))?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings, 1);
    assert_eq!(got.len(), 1);
    Ok(got.pop().unwrap())
}

#[test_case("BIND(42 as ?x)", TRUE; "x is bound")]
#[test_case("", FALSE; "nothing bound")]
#[test_case("BIND(42 as ?x2)", FALSE; "x is not bound")]
#[test_case("BIND(42/0 as ?x)", FALSE; "x gets an error")]
fn test_bound(body: &str, exp: &str) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let got = bindings_to_vec(
        dataset
            .query(format!("SELECT (BOUND(?x) as ?b) {{ {body} }}").as_str())?
            .into_bindings(),
        1,
    );
    assert_eq!(got.len(), 1);
    assert_eq!(&got[0], exp);
    Ok(())
}

const TRUE: &str = "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>";
const FALSE: &str = "\"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>";

fn dataset_101() -> TestResult<LightDataset> {
    let dataset: LightDataset = sophia_turtle::parser::trig::parse_str(
        r#"
                BASE <https://example.org/test>
                PREFIX s: <http://schema.org/>

                <#a> a s:Person ;
                  s:name "Alice" ;
                  s:performerIn [
                    a s:Event ;
                    s:name "Bob's birthday party" ;
                  ].

                GRAPH <#g> {
                  <#b> a s:Person ;
                    s:name "Alice".
                }

                GRAPH _:g {
                  <#a> s:name "Albert".
                }

            "#,
    )
    .collect_quads()?;
    Ok(dataset)
}

fn dataset_ppath() -> TestResult<LightDataset> {
    let dataset: LightDataset = sophia_turtle::parser::trig::parse_str(
        r#"
            PREFIX : <x:>

            :a1 :p :b1.
            :b1 :p :c1.
            :c1 :p :d1.
            :d1 :p :e1.

            :a2 :q :b2.
            :b2 :q :c2.
            :c2 :q :d2.
            :d2 :q :e2, :b2.

            :a1 :r :a2.
            :b1 :r :b2.
            :c1 :r :c2.
            :d1 :r :d2.
            :e1 :r :e2.

            :a1 :s :b1.

            :d1 :t <<( :a1 :b1 :c1 )>>.
            :d2 :t <<( :a2 :b1 :c2 )>>.
        "#,
    )
    .collect_quads()?;
    Ok(dataset)
}

/// Return a flat list of all bindings, in serialized form.
fn bindings_to_vec(bindings: Bindings<LightDataset>, nbvar: usize) -> Vec<String> {
    assert_eq!(bindings.variables().len(), nbvar);
    bindings
        .into_iter()
        .flat_map(|bs| {
            bs.unwrap().into_iter().map(|o| {
                o.map(|t| {
                    if t.is_blank_node() {
                        "_:b".to_string()
                    } else {
                        t.to_string()
                    }
                })
                .unwrap_or_default()
            })
        })
        .collect()
}

/// Return a flat list of all bindings, in serialized form.
fn bindings_to_vec_of_arrays(
    bindings: Bindings<LightDataset>,
    vars: [&str; 2],
) -> Vec<[String; 2]> {
    assert_eq!(bindings.variables(), vars);
    bindings
        .into_iter()
        .map(|bs| {
            let values: [_; 2] = bs.unwrap().try_into().unwrap();
            values.map(|opt| opt.map(|t| t.to_string()).unwrap_or_else(|| "".into()))
        })
        .collect()
}

type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;
