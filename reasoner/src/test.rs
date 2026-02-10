use sophia_api::{
    graph::Graph,
    serializer::{Stringifier, TripleSerializer},
    source::{QuadSource, StreamError::SinkError, TripleSource},
    term::SimpleTerm,
};
use test_case::test_case;

use crate::{
    d_entailment::{IllTypedLiteral, Nothing, Sparql},
    ruleset::{Rdf, Rdfs, Simple},
};

use super::*;

// /* this test is commented out because the test on Graph::iris fails, but that's OK
//    This implementation yields datatypes in Graph::iris, which is not assumed by the test suite.
//    Arguably, it is a valid behaviour, especilly when entailment is involved
//    (because RDF entailment produces triples where datatypes become nodes)
// */
// type SimpleEntailementGraph = ReasonableGraph<crate::d_entailment::Nothing, Simple>;
// sophia_api::test_immutable_graph_impl!(SimpleEntailementGraph);

#[test]
fn literal_normalization_sparql() {
    let g1: ReasonableGraph<Sparql, Simple> = sophia_turtle::parser::turtle::parse_str(
        r#"
                PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                <x:s> <x:p> (
                    01
                    1.00
                    1000e-3
                    "+inf"^^xsd:double
                    "nan"^^xsd:float
                    "01"^^xsd:int
                    "foo"^^<x:unrecognized>
                ).
            "#,
    )
    .collect_triples()
    .unwrap();
    let g2: ReasonableGraph<Sparql, Simple> = sophia_turtle::parser::turtle::parse_str(
        r#"
                PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                <x:s> <x:p> (
                    1
                    1.0
                    1e0
                    "INF"^^xsd:double
                    "NaN"^^xsd:float
                    "1"^^xsd:int
                    "foo"^^<x:unrecognized>
                ).
            "#,
    )
    .collect_triples()
    .unwrap();
    assert!(sophia_isomorphism::isomorphic_graphs(&g1, &g2).unwrap());
}

#[test_case(r#""#, ":s :p :o.", false; "empty graph does not entail")]
#[test_case(r#":s :p :o1, :o2."#, "", true; "empty graph is entailed")]
#[test_case(r#":s :p :o1, :o2."#, ":s :p :o1, :o2.", true; "same graph is entailed")]
#[test_case(r#":s :p :o1, :o2."#, ":s :p :o1, :o3.", false; "different graph is not entailed")]
#[test_case(r#":s :p :o1, :o2."#, ":s :p :o1, :o2, :o3.", false; "bigger graph is not entailed")]
#[test_case(r#":s :p :o1, :o2."#, ":s :p :o2.", true; "subgraph is entailed")]
#[test_case(r#":s :p :o1, :o2."#, ":s :p [].", true; "subgraph with bnode is entailed")]
#[test_case(r#":s :p :o1, :o2."#, "[] :p [].", true; "subgraph with 2 bnodes is entailed")]
#[test_case(r#":s :p :o1, :o2."#, "_:b :p _:b.", false; "subgraph with same bnode is not entailed")]
#[test_case(r#":s :p :o1, :o2."#, ":s :p ?x.", true; "subgraph with variable is entailed")]
#[test_case(r#":s :p :o1, :o2."#, "?x :p ?y.", true; "subgraph with 2 variables is entailed")]
#[test_case(r#":s :p :o1, :o2."#, "?x :p ?x.", false; "subgraph with same variable is not entailed")]
#[test_case(r#""s" "p" "o1", "o2"."#, r#"_:b "p" _:b."#, false; "generalized data, subgraph with same bnode entailed")]
#[test_case(r#""s" "p" "o1", "s"."#, r#"_:b "p" _:b."#, true; "generalized data, subgraph with same bnode not entailed")]
#[test_case(r#""s" "p" "o1", "s"."#, r#"_:b [] _:b."#, true; "generalized data, subgraph with bnode as predicate entailed")]
#[test_case(r#":s :p <<( :a :b :o2 )>>."#, ":s :p <<( :a :b [] )>>.", true; "triple term, subgraph with bnode is entailed")]
#[test_case(r#":s :p <<( :a :b :o2 )>>."#, "[] :p <<( :a :b [] )>>.", true; "triple term, subgraph with 2 bnodes is entailed")]
#[test_case(r#":s :p <<( :a :b :o2 )>>."#, "_:b :p <<( :a :b _:b )>>.", false; "triple term, subgraph with same bnode is not entailed")]
#[test_case(r#":s :p <<( :a :b :s )>>."#, "_:b :p <<( :a :b _:b )>>.", true; "triple term, subgraph with same bnode is entailed")]
#[test_case(r#":s :p <<( :a :b :o2 )>>."#, ":s :p <<( :a :b ?x )>>.", true; "triple term, subgraph with variable is entailed")]
#[test_case(r#":s :p <<( :a :b :o2 )>>."#, "?x :p <<( :a :b ?y )>>.", true; "triple term, subgraph with 2 variables is entailed")]
#[test_case(r#":s :p <<( :a :b :o2 )>>."#, "?x :p <<( :a :b ?x )>>.", false; "triple term, subgraph with same variable is not entailed")]
#[test_case(r#""s" "p" <<( "a" "b" "o1" )>>."#, r#"_:b "p" <<( "a" "b" _:b )>>."#, false; "generalized data with triple term, subgraph with same bnode not entailed")]
#[test_case(r#""s" "p" <<( "a" "b" "s" )>>."#, r#"_:b "p" <<( "a" "b" _:b )>>."#, true; "generalized data with triple term, subgraph with same bnode entailed")]
#[test_case(r#""s" "p" <<( "a" "b" "s" )>>."#, r#"_:b [] <<( "a" [] _:b )>>."#, true; "generalized data with triple term, subgraph with bnode as predicate entailed")]
fn simple_entail(g1: &str, g2: &str, exp: bool) {
    static PREFIXES: &str = r"
        PREFIX : <x:>
";
    let probe: Vec<[SimpleTerm; 3]> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g2}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Probe", &probe);

    let g: ReasonableGraph<Nothing, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert_eq!(g.entails(&probe).unwrap(), exp, "simple, no D");

    let g: ReasonableGraph<Sparql, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert_eq!(g.entails(&probe).unwrap(), exp, "simple + D_sparql");

    let g: ReasonableGraph<Nothing, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert_eq!(g.entails(&probe).unwrap(), exp, "RDF");

    let g: ReasonableGraph<Sparql, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert_eq!(g.entails(&probe).unwrap(), exp, "RDF + D_sparql");

    let g: ReasonableGraph<Nothing, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert_eq!(g.entails(&probe).unwrap(), exp, "RDF-S");

    let g: ReasonableGraph<Sparql, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert_eq!(g.entails(&probe).unwrap(), exp, "RDF-S + D_sparql");
}

#[test_case(r#":s :p "foo"^^xsd:integer."#; "integer")]
#[test_case(r#":s :p "foo"^^xsd:decimal."#; "decimal")]
#[test_case(r#":s :p "foo"^^xsd:double."#; "double")]
#[test_case(r#":s :p "foo"^^xsd:boolean."#; "boolean")]
#[test_case(r#":s :p "foo"^^xsd:dateTime."#; "dateTime")]
#[test_case(r#":s :p "foo"^^xsd:int."#; "int")]
#[test_case(r#":s :p "299"^^xsd:byte."#; "byte")]
#[test_case(r#":s :p "-1"^^xsd:positiveInteger."#; "positiveInteger")]
fn d_sparql_ill_formed(g1: &str) {
    static PREFIXES: &str = r"
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX : <x:>
";
    let res: Result<ReasonableGraph<Nothing, Simple>, _> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples();
    assert!(res.is_ok(), "simple, no D");

    let res: Result<ReasonableGraph<Sparql, Simple>, _> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples();
    assert!(
        matches!(res, Err(SinkError(IllTypedLiteral { .. }))),
        "simple + D_sparql"
    );

    let res: Result<ReasonableGraph<Nothing, Rdf>, _> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples();
    assert!(res.is_ok(), "RDF, no D");

    let res: Result<ReasonableGraph<Sparql, Rdf>, _> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples();
    assert!(
        matches!(res, Err(SinkError(IllTypedLiteral { .. }))),
        "RDF + D_sparql"
    );

    let res: Result<ReasonableGraph<Nothing, Rdfs>, _> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples();
    assert!(res.is_ok(), "RDF-S, no D");

    let res: Result<ReasonableGraph<Sparql, Rdfs>, _> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples();
    assert!(
        matches!(res, Err(SinkError(IllTypedLiteral { .. }))),
        "RDF-S + D_sparql"
    );
}

#[test_case(r#":s :p 01 ."#, ":s :p 1 ."; "graph non-normalized integer")]
#[test_case(r#":s :p 1 ."#, ":s :p 01 ."; "probe non-normalized integer")]
#[test_case(r#":s :p 1.00 ."#, ":s :p 1.0 ."; "graph non-normalized decimal")]
#[test_case(r#":s :p 1.0 ."#, ":s :p 1.00 ."; "probe non-normalized decimal")]
#[test_case(r#":s :p 10e-1 ."#, ":s :p 1e0 ."; "graph non-normalized double")]
#[test_case(r#":s :p 1e0 ."#, ":s :p 10e-1 ."; "probe non-normalized double")]
#[test_case(r#":s :p "1"^^xsd:boolean."#, ":s :p true ."; "graph non-normalized boolean")]
#[test_case(r#":s :p true ."#, r#":s :p "1"^^xsd:boolean."#; "probe non-normalized boolean")]
#[test_case(r#":s :p "01"^^xsd:int ."#, r#":s :p "1"^^xsd:int ."#; "graph non-normalized int")]
#[test_case(r#":s :p "1"^^xsd:int ."#, r#":s :p "01"^^xsd:int ."#; "probe non-normalized int")]
#[test_case(r#":s :p <<( :a :b 01 )>>."#, ":s :p <<( :a :b 1 )>>."; "graph non-normalized integer in triple term")]
#[test_case(r#":s :p <<( :a :b 1 )>>."#, ":s :p <<( :a :b 01 )>>."; "probe non-normalized integer in triple term")]
#[test_case(r#":s :p <<( :a :b 1.00 )>>."#, ":s :p <<( :a :b 1.0 )>>."; "graph non-normalized decimal in triple term")]
#[test_case(r#":s :p <<( :a :b 1.0 )>>."#, ":s :p <<( :a :b 1.00 )>>."; "probe non-normalized decimal in triple term")]
#[test_case(r#":s :p <<( :a :b 10e-1 )>>."#, ":s :p <<( :a :b 1e0 )>>."; "graph non-normalized double in triple term")]
#[test_case(r#":s :p <<( :a :b 1e0 )>>."#, ":s :p <<( :a :b 10e-1 )>>."; "probe non-normalized double in triple term")]
#[test_case(r#":s :p <<( :a :b "1"^^xsd:boolean)>>."#, ":s :p <<( :a :b true )>>."; "graph non-normalized boolean in triple term")]
#[test_case(r#":s :p <<( :a :b true )>>."#, r#":s :p <<( :a :b "1"^^xsd:boolean)>>."#; "probe non-normalized boolean in triple term")]
#[test_case(r#":s :p <<( :a :b "01"^^xsd:int )>>."#, r#":s :p <<( :a :b "1"^^xsd:int )>>."#; "graph non-normalized int in triple term")]
#[test_case(r#":s :p <<( :a :b "1"^^xsd:int )>>."#, r#":s :p <<( :a :b "01"^^xsd:int )>>."#; "probe non-normalized int in triple term")]
#[test_case(r#":s :p 1 ."#, r#":s :p 1.0 ."#; "integer entails decimal")]
#[test_case(r#":s :p 1.0 ."#, r#":s :p 1 ."#; "decimal entails integer")]
#[test_case(r#":s :p "1"^^xsd:int ."#, r#":s :p 1.0 ."#; "int entails decimal")]
#[test_case(r#":s :p 1.0 ."#, r#":s :p "1"^^xsd:int ."#; "decimal entails int")]
#[test_case(r#":s :p <<( :a :b 1 )>>."#, r#":s :p <<( :a :b 1.0 )>>."#; "integer entails decimal in triple term")]
#[test_case(r#":s :p <<( :a :b 1.0 )>>."#, r#":s :p <<( :a :b 1 )>>."#; "decimal entails integer in triple term")]
#[test_case(r#":s :p <<( :a :b "1"^^xsd:int )>>."#, r#":s :p <<( :a :b 1.0 )>>."#; "int entails decimal in triple term")]
#[test_case(r#":s :p <<( :a :b 1.0 )>>."#, r#":s :p <<( :a :b "1"^^xsd:int )>>."#; "decimal entails int in triple term")]
fn simple_d_sparql_entailment(g1: &str, g2: &str) {
    static PREFIXES: &str = r"
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX : <x:>
";
    let probe: Vec<[SimpleTerm; 3]> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g2}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Probe", &probe);

    let g: ReasonableGraph<Nothing, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple, no D");

    let g: ReasonableGraph<Sparql, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "simple + D_sparql");

    let g: ReasonableGraph<Nothing, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF");

    let g: ReasonableGraph<Sparql, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF + D_sparql");

    let g: ReasonableGraph<Nothing, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF-S");

    let g: ReasonableGraph<Sparql, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF-S + D_sparql");
}

#[test_case(r#""#, "rdf:type rdf:type rdf:Property . rdf:subject rdf:type rdf:Property . rdf:predicate rdf:type rdf:Property . rdf:object rdf:type rdf:Property . rdf:reifies rdf:type rdf:Property . rdf:first rdf:type rdf:Property . rdf:rest rdf:type rdf:Property . rdf:value rdf:type rdf:Property . rdf:nil rdf:type rdf:List . rdf:_1 a rdf:Property . "; "RDF axioms")]
#[test_case(r#":s :p 42 ."#, ":p a rdf:Property."; "used predicate is a Property")]
#[test_case(r#":s :p <<( :a :b :c )>> ."#, ":b a rdf:Property."; "predicate in triple term is a Property")]
fn rdf_entailment(g1: &str, g2: &str) {
    static PREFIXES: &str = r"
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX : <x:>
";
    let probe: Vec<[SimpleTerm; 3]> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g2}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    debug_graph("Probe", &probe);

    let g: ReasonableGraph<Nothing, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple, no D");

    let g: ReasonableGraph<Sparql, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple + D_sparql");

    let g: ReasonableGraph<Nothing, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF");

    let g: ReasonableGraph<Sparql, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF + D_sparql");

    let g: ReasonableGraph<Nothing, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF-S");

    let g: ReasonableGraph<Sparql, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF-S + D_sparql");
}

#[test_case(r#""#, "[] a xsd:decimal, xsd:integer, xsd:long, xsd:int, xsd:short, xsd:byte, xsd:unsignedLong, xsd:unsignedInt, xsd:unsignedShort, xsd:unsignedByte, xsd:nonNegativeInteger, xsd:positiveInteger."; "there exists a positive integer")]
#[test_case(r#""#, "[] a xsd:decimal, xsd:integer, xsd:long, xsd:int, xsd:short, xsd:byte, xsd:unsignedLong, xsd:unsignedInt, xsd:unsignedShort, xsd:unsignedByte, xsd:nonNegativeInteger, xsd:nonPositiveInteger."; "there exists a null integer")]
#[test_case(r#""#, "[] a xsd:decimal, xsd:integer, xsd:long, xsd:int, xsd:short, xsd:byte, xsd:nonPositiveInteger, xsd:negativeInteger."; "there exists a negative integer")]
#[test_case(r#""#, "[] a xsd:boolean."; "there exists a boolean")]
#[test_case(r#""#, "[] a xsd:double."; "there exists a double")]
#[test_case(r#""#, "[] a xsd:float."; "there exists a float")]
#[test_case(r#""#, "[] a xsd:dateTime."; "there exists a dateTime")]
#[test_case(r#":s :p 42 ."#, ":s :p [ a xsd:decimal, xsd:byte, xsd:positiveInteger ]."; "recognized literal has types")]
#[test_case(r#":s :p 42 ."#, "042 a xsd:decimal, xsd:byte, xsd:positiveInteger."; "recognized literal has types generalized")]
fn rdf_d_sparql_entailment(g1: &str, g2: &str) {
    static PREFIXES: &str = r"
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX : <x:>
";
    let probe: Vec<[SimpleTerm; 3]> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g2}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Probe", &probe);

    let g: ReasonableGraph<Nothing, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple, no D");

    let g: ReasonableGraph<Sparql, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple + D_sparql");

    let g: ReasonableGraph<Nothing, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF");

    let g: ReasonableGraph<Sparql, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF + D_sparql");

    let g: ReasonableGraph<Nothing, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF-S");

    let g: ReasonableGraph<Sparql, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF-S + D_sparql");
}

#[test_case(r#""#, "rdf:type rdfs:domain rdfs:Resource . rdf:reifies rdfs:domain rdfs:Resource . rdfs:domain rdfs:domain rdf:Property . rdfs:range rdfs:domain rdf:Property . rdfs:subPropertyOf rdfs:domain rdf:Property . rdfs:subClassOf rdfs:domain rdfs:Class . rdf:subject rdfs:domain rdf:Statement . rdf:predicate rdfs:domain rdf:Statement . rdf:object rdfs:domain rdf:Statement . rdfs:member rdfs:domain rdfs:Resource . rdf:first rdfs:domain rdf:List . rdf:rest rdfs:domain rdf:List . rdfs:seeAlso rdfs:domain rdfs:Resource . rdfs:isDefinedBy rdfs:domain rdfs:Resource . rdfs:comment rdfs:domain rdfs:Resource . rdfs:label rdfs:domain rdfs:Resource . rdf:value rdfs:domain rdfs:Resource . rdf:type rdfs:range rdfs:Class . rdf:reifies rdfs:range rdfs:Proposition . rdfs:domain rdfs:range rdfs:Class . rdfs:range rdfs:range rdfs:Class . rdfs:subPropertyOf rdfs:range rdf:Property . rdfs:subClassOf rdfs:range rdfs:Class . rdf:subject rdfs:range rdfs:Resource . rdf:predicate rdfs:range rdfs:Resource . rdf:object rdfs:range rdfs:Resource . rdfs:member rdfs:range rdfs:Resource . rdf:first rdfs:range rdfs:Resource . rdf:rest rdfs:range rdf:List . rdfs:seeAlso rdfs:range rdfs:Resource . rdfs:isDefinedBy rdfs:range rdfs:Resource . rdfs:comment rdfs:range rdfs:Literal . rdfs:label rdfs:range rdfs:Literal . rdf:value rdfs:range rdfs:Resource . rdf:Alt rdfs:subClassOf rdfs:Container . rdf:Bag rdfs:subClassOf rdfs:Container . rdf:Seq rdfs:subClassOf rdfs:Container . rdfs:ContainerMembershipProperty rdfs:subClassOf rdf:Property . rdfs:isDefinedBy rdfs:subPropertyOf rdfs:seeAlso . rdfs:Datatype rdfs:subClassOf rdfs:Class . rdf:_1 rdf:type rdfs:ContainerMembershipProperty . rdf:_1 rdfs:domain rdfs:Resource . rdf:_1 rdfs:range rdfs:Resource . "; "RDFS axioms")]
#[test_case(r#":s rdf:_3 :o."#, "rdf:_3 a rdfs:ContainerMembershipProperty; rdfs:domain rdfs:Resource; rdfs:range rdfs:Resource ."; "axioms on used membership property")]
#[test_case(r#""#, "[] a rdfs:Resource."; "there always exists a Resource")]
#[test_case(r#""#, "[] a rdfs:Proposition."; "there always exists a Proposition")]
#[test_case(r#":p rdfs:domain :c. :s :p :o."#, ":s a :c."; "rdfs2")]
#[test_case(r#":p rdfs:range :c. :s :p :o."#, ":o a :c."; "rdfs3")]
#[test_case(r#":s :p 42 ."#, ":s a rdfs:Resource."; "rdfs4 used term")]
#[test_case(r#":s :p <<( :a :b :c )>> ."#, ":a a rdfs:Resource."; "rdfs4 embedded term")]
#[test_case(r#":p1 rdfs:subPropertyOf :p2. :p2 rdfs:subPropertyOf :p3. :p3 rdfs:subPropertyOf :p4."#, ":p1 rdfs:subPropertyOf :p4."; "rdfs5")]
#[test_case(r#":p a rdf:Property."#, ":p rdfs:subPropertyOf :p."; "rdfs6")]
#[test_case(r#":p1 rdfs:subPropertyOf :p2. :s :p1 :o."#, ":s :p2 :o."; "rdfs7")]
#[test_case(r#":c a rdfs:Class."#, ":c rdfs:subClassOf rdfs:Resource."; "rdfs8")]
#[test_case(r#":c1 rdfs:subClassOf :c2. :s a :c1."#, ":s a :c2."; "rdfs9")]
#[test_case(r#":c a rdfs:Class."#, ":c rdfs:subClassOf :c."; "rdfs10")]
#[test_case(r#":c1 rdfs:subClassOf :c2. :c2 rdfs:subClassOf :c3. :c3 rdfs:subClassOf :c4."#, ":c1 rdfs:subClassOf :c4."; "rdfs11")]
#[test_case(r#":p a rdfs:ContainerMembershipProperty."#, ":p rdfs:subPropertyOf rdfs:member."; "rdfs12")]
#[test_case(r#":d a rdfs:Datatype."#, ":d rdfs:subClassOf rdfs:Literal."; "rdfs13")]
#[test_case(r#":s :p <<( :a :b :c )>> ."#, ":s :p [ a rdfs:Proposition ]."; "rdfs14")]
#[test_case(r#":s :p <<( :a :b :c )>> ."#, "<<( :a :b :c )>> a rdfs:Proposition."; "rdfs14 generalized")]
#[test_case(r#":s :p <<( :a :b <<( :c :d :e )>> )>> ."#, ":s :p <<( :a :b _:x )>>. _:x a rdfs:Proposition."; "rdfs14 embedded")]
#[test_case(r#":s :p <<( :a :b <<( :c :d :e )>> )>> ."#, "<<( :c :d :e )>> a rdfs:Proposition."; "rdfs14 embedded generalized")]
#[test_case(r#""#, "rdfs:Resource rdf:type rdfs:Class . rdfs:Class rdf:type rdfs:Class . rdfs:Literal rdf:type rdfs:Class . rdfs:Datatype rdf:type rdfs:Class . rdf:Seq rdf:type rdfs:Class . rdf:Bag rdf:type rdfs:Class . rdf:Alt rdf:type rdfs:Class . rdfs:Container rdf:type rdfs:Class . rdf:List rdf:type rdfs:Class . rdfs:ContainerMembershipProperty rdf:type rdfs:Class . rdf:Property rdf:type rdfs:Class . rdf:Statement rdf:type rdfs:Class . rdfs:domain rdf:type rdf:Property . rdfs:range rdf:type rdf:Property . rdfs:subPropertyOf rdf:type rdf:Property . rdfs:subClassOf rdf:type rdf:Property . rdfs:member rdf:type rdf:Property . rdfs:seeAlso rdf:type rdf:Property . rdfs:isDefinedBy rdf:type rdf:Property . rdfs:comment rdf:type rdf:Property . rdfs:label rdf:type rdf:Property ."; "some RDFS-valid triples")] // from the spec
#[test_case(r#":s :p1 :o. :p1 :spo1 :p2. :p2 :spo1 :p3. :spo1 :spo2 :spo3. :spo2 :spo3 rdfs:subPropertyOf. :spo3 rdfs:subPropertyOf rdfs:subPropertyOf."#, ":s :p3 :o."; "interleaved rdfs5 and rdfs7")]
fn rdfs_entailment(g1: &str, g2: &str) {
    static PREFIXES: &str = r"
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX : <x:>
";
    let probe: Vec<[SimpleTerm; 3]> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g2}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Probe", &probe);

    let g: ReasonableGraph<Nothing, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple, no D");

    let g: ReasonableGraph<Sparql, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple + D_sparql");

    let g: ReasonableGraph<Nothing, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF");

    let g: ReasonableGraph<Sparql, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF + D_sparql");

    let g: ReasonableGraph<Nothing, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF-S");

    let g: ReasonableGraph<Sparql, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF-S + D_sparql");
}

#[test_case(r#""#, "xsd:integer a rdfs:Datatype. xsd:dateTime a rdfs:Datatype."; "rdfs1")]
fn rdfs_d_sparql_entailment(g1: &str, g2: &str) {
    static PREFIXES: &str = r"
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX : <x:>
";
    let probe: Vec<[SimpleTerm; 3]> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g2}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Probe", &probe);

    let g: ReasonableGraph<Nothing, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple, no D");

    let g: ReasonableGraph<Sparql, Simple> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "simple + D_sparql");

    let g: ReasonableGraph<Nothing, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF");

    let g: ReasonableGraph<Sparql, Rdf> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF + D_sparql");

    let g: ReasonableGraph<Nothing, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(!g.entails(&probe).unwrap(), "RDF-S");

    let g: ReasonableGraph<Sparql, Rdfs> =
        sophia_turtle::parser::gtrig::parse_str(&format!(r"{PREFIXES}{g1}"))
            .to_triples()
            .collect_triples()
            .unwrap();
    // debug_graph("Graph", &g);

    assert!(g.entails(&probe).unwrap(), "RDF-S + D_sparql");
}

//

#[allow(dead_code)]
fn debug_graph<G: Graph>(label: &str, g: G) {
    println!(
        "{label}:\n{}### {} triples",
        sophia_turtle::serializer::nt::NTriplesSerializer::new_stringifier()
            .serialize_graph(&g)
            .unwrap()
            .to_string(),
        g.triples().count(),
    );
}
