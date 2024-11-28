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
#[allow(clippy::needless_pass_by_value)]
#[test_case(
    "SELECT ?x { GRAPH ?x { ?s ?p ?o } }",
    vec!["<https://example.org/test#g>", "<https://example.org/test#g>", "_:b"];
    "graphs"
)]
#[allow(clippy::needless_pass_by_value)]
#[test_case(
    "SELECT ?x { GRAPH ?g { <#a> s:name ?x } }",
    vec!["\"Albert\"^^<http://www.w3.org/2001/XMLSchema#string>"];
    "name_in_g"
)]
fn test_select_1_and_ask(query: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = format!("BASE <https://example.org/test> PREFIX s: <http://schema.org/> {query}");
    let parsed_query = SparqlQuery::parse(&query)?;
    let bindings = dataset.query(&parsed_query)?.into_bindings();
    assert_eq!(bindings.variables(), &["x"]);
    let mut got = bindings_to_vec(bindings);
    got.sort();
    assert_eq!(exp, got);

    let parsed_query = SparqlQuery::parse(&query.replace("SELECT ?x", "ASK"))?;
    let response = dataset.query(&parsed_query)?.into_boolean();
    assert_eq!(response, !exp.is_empty());
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
    let got = bindings_to_vec(dataset.query(query0.as_str())?.into_bindings());
    assert_eq!(got.len(), limit.min(5));

    let mut offset = 0;
    let mut got = vec![];
    loop {
        let query = format!("SELECT ?p {{ [] ?p [] }} OFFSET {offset} LIMIT {limit}");
        let partial = bindings_to_vec(dataset.query(query.as_str())?.into_bindings());
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
#[test_case("FILTER EXISTS { BIND(42 as ?x) }", vec![]; "exists error")]
fn test_filter(filter: &str, exp: Vec<&str>) -> TestResult {
    let dataset = dataset_101()?;
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(&format!(
        "PREFIX s: <http://schema.org/> SELECT ?x {{ ?x s:name ?n. {filter} }}"
    ))?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings);
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
    let got = bindings_to_vec(bindings);
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
#[test_case("!(!\"\"@en)           ", "false"; "bool lang-string empty")]
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
#[test_case("str(042)", "\"042\""; "str for number")]
#[test_case("str(042+1)", "\"43\""; "str for number computed")]
#[test_case("str(\"a\"^^xsd:integer)", "\"a\""; "str for ill-formed")]
// test str error
#[test_case("str(bnode())", ""; "str for bnode")]
#[test_case("str(<< <tag:s> <tag:p> <tag:o> >>)", ""; "str for triple")]
#[test_case("str(42/0)", ""; "str error")]
// test lang nominal
#[test_case("lang(\"42\")", "\"\""; "lang for string")]
#[test_case("lang(\"chat\"@en)", "\"en\""; "lang for language string")]
#[test_case("lang(042)", "\"\""; "lang for number")]
#[test_case("lang(\"a\"^^xsd:integer)", "\"\""; "lang for ill-formed")]
// test lang error
#[test_case("lang(<tag:x>)", ""; "lang for IRI")]
#[test_case("lang(bnode())", ""; "lang for bnode")]
#[test_case("lang(<< <tag:s> <tag:p> <tag:o> >>)", ""; "lang for triple")]
#[test_case("lang(42/0)", ""; "lang error")]
// test datatype nominal
#[test_case("datatype(\"42\")", "xsd:string"; "datatype for string")]
#[test_case("datatype(\"chat\"@en)", "rdf:langString"; "datatype for language string")]
#[test_case("datatype(042)", "xsd:integer"; "datatype for number")]
#[test_case("datatype(\"a\"^^xsd:integer)", "xsd:integer"; "datatype for ill-formed")]
// test datatype error
#[test_case("datatype(<tag:x>)", ""; "datatype for IRI")]
#[test_case("datatype(<< <tag:s> <tag:p> <tag:o> >>)", ""; "datatype for triple")]
#[test_case("datatype(42/0)", ""; "datatype error")]
// test iri nominal
#[test_case("iri(<tag:x>)", "<tag:x>"; "iri for IRI")]
#[test_case("iri(\"tag:y\")", "<tag:y>"; "iri for string")]
// test iri error
#[test_case("iri(bnode())", ""; "iri for bnode")]
#[test_case("iri(\"a b\")", ""; "iri for string that is not an IRI")]
#[test_case("iri(\"tag:z\"@en)", ""; "iri for language string")]
#[test_case("iri(042)", ""; "iri for number")]
#[test_case("iri(\"tag:t\"^^xsd:integer)", ""; "iri for iill-formed")]
#[test_case("iri(<< <tag:s> <tag:p> <tag:o> >>)", ""; "iri for triple")]
#[test_case("iri(42/0)", ""; "iri error")]
// test uri nominal
#[test_case("uri(<tag:x>)", "<tag:x>"; "uri for IRI")]
#[test_case("uri(\"tag:y\")", "<tag:y>"; "uri for string")]
// test uri error
#[test_case("uri(bnode())", ""; "uri for bnode")]
#[test_case("uri(\"a b\")", ""; "uri for string that is not an IRI")]
#[test_case("uri(\"tag:z\"@en)", ""; "uri for language string")]
#[test_case("uri(042)", ""; "uri for number")]
#[test_case("uri(\"tag:t\"^^xsd:integer)", ""; "uri for iill-formed")]
#[test_case("uri(<< <tag:s> <tag:p> <tag:o> >>)", ""; "uri for triple")]
#[test_case("uri(42/0)", ""; "uri error")]
// test bnode nominal
#[test_case("isBlank(bnode())", "true"; "bnode no arg is bnode")]
#[test_case("bnode() = bnode()", "false"; "bnode no arg returns different values")]
#[test_case("isBlank(bnode(\"42\"))", "true"; "bnode for string")]
// test bnode error
#[test_case("isBlank(bnode(<tag:x>))", ""; "bnode for IRI")]
#[test_case("isBlank(bnode(\"chat\"@en))", ""; "bnode language for string")]
#[test_case("isBlank(bnode(042))", ""; "bnode for number")]
#[test_case("isBlank(bnode(<< <tag:s> <tag:p> <tag:o> >>))", ""; "bnode for triple")]
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
#[test_case("abs(1e0/0)", "\"inf\"^^xsd:double"; "abs for positive INF")]
#[test_case("abs(-1e0/0)", "\"inf\"^^xsd:double"; "abs for negative INF")]
#[test_case("abs(0e0/0)", "\"NaN\"^^xsd:double"; "abs for NaN")]
// test abs error
#[test_case("abs(<tag:x>)", ""; "abs for IRI")]
#[test_case("abs(bnode())", ""; "abs for bnode")]
#[test_case("abs(\"42\")", ""; "abs for string")]
#[test_case("abs(\"chat\"@en)", ""; "abs for language string")]
#[test_case("abs(\"a\"^^xsd:integer)", ""; "abs for ill formed")]
#[test_case("abs(<< <tag:s> <tag:p> <tag:o> >>)", ""; "abs for triple")]
// test ceil nominal
#[test_case("ceil(042)", "42"; "ceil for integer")]
#[test_case("ceil(3.14)", "4.0"; "ceil for decimal")]
#[test_case("ceil(3.14e0)", "4e0"; "ceil for double")]
#[test_case("ceil(\"1.5\"^^xsd:float)", "\"2e0\"^^xsd:float"; "ceil for float")]
// test ceil error
#[test_case("ceil(<tag:x>)", ""; "ceil for IRI")]
#[test_case("ceil(bnode())", ""; "ceil for bnode")]
#[test_case("ceil(\"42\")", ""; "ceil for string")]
#[test_case("ceil(\"chat\"@en)", ""; "ceil for language string")]
#[test_case("ceil(\"a\"^^xsd:integer)", ""; "ceil for ill formed")]
#[test_case("ceil(<< <tag:s> <tag:p> <tag:o> >>)", ""; "ceil for triple")]
// test floor nominal
#[test_case("floor(042)", "42"; "floor for integer")]
#[test_case("floor(3.14)", "3.0"; "floor for decimal")]
#[test_case("floor(3.14e0)", "3e0"; "floor for double")]
#[test_case("floor(\"1.5\"^^xsd:float)", "\"1e0\"^^xsd:float"; "floor for float")]
// test floor error
#[test_case("floor(<tag:x>)", ""; "floor for IRI")]
#[test_case("floor(bnode())", ""; "floor for bnode")]
#[test_case("floor(\"42\")", ""; "floor for string")]
#[test_case("floor(\"chat\"@en)", ""; "floor for language string")]
#[test_case("floor(\"a\"^^xsd:integer)", ""; "floor for ill formed")]
#[test_case("floor(<< <tag:s> <tag:p> <tag:o> >>)", ""; "floor for triple")]
// test round nominal
#[test_case("round(042)", "42"; "round for integer")]
#[test_case("round(3.14)", "3.0"; "round for decimal")]
#[test_case("round(3.14e0)", "3e0"; "round for double")]
#[test_case("round(\"1.5\"^^xsd:float)", "\"2e0\"^^xsd:float"; "round for float")]
// test round error
#[test_case("round(<tag:x>)", ""; "round for IRI")]
#[test_case("round(bnode())", ""; "round for bnode")]
#[test_case("round(\"42\")", ""; "round for string")]
#[test_case("round(\"chat\"@en)", ""; "round for language string")]
#[test_case("round(\"a\"^^xsd:integer)", ""; "round for ill formed")]
#[test_case("round(<< <tag:s> <tag:p> <tag:o> >>)", ""; "round for triple")]
// test concat nominal
#[test_case("concat(\"foo\", \"bar\")", "\"foobar\"")]
#[test_case("concat(\"foo\"@en, \"bar\"@en)", "\"foobar\"@en")]
#[test_case("concat(\"foo\"@en, \"bar\")", "\"foobar\"")]
#[test_case("concat(\"foo\", \"bar\"@en)", "\"foobar\"")]
#[test_case("concat(\"foo\"@en, \"bar\"@es)", "\"foobar\"")]
#[test_case("concat(\"abc\")", "\"abc\"")]
#[test_case("concat(\"abc\"@en)", "\"abc\"@en")]
#[test_case("concat()", "\"\"")]
#[test_case("concat(\"a\", \"b\", \"c\")", "\"abc\"")]
#[test_case("concat(\"a\", \"b\", \"c\", \"d\")", "\"abcd\"")]
// test concat nominal error
#[test_case("concat(<tag:x>)", ""; "concat for IRI")]
#[test_case("concat(bnode())", ""; "concat for bnode")]
#[test_case("concat(042)", ""; "concat for number")]
#[test_case("concat(<< <tag:s> <tag:p> <tag:o> >>)", ""; "concat for triple")]
#[test_case("concat(\"x\", <tag:x>)", ""; "concat for string and IRI")]
#[test_case("concat(\"x\", bnode())", ""; "concat for string and bnode")]
#[test_case("concat(\"x\", 042)", ""; "concat for string and number")]
#[test_case("concat(\"x\", << <tag:s> <tag:p> <tag:o> >>)", ""; "concat for string and triple")]
#[test_case("concat(<tag:x>, \"x\")", ""; "concat for IRI and string")]
#[test_case("concat(bnode(), \"x\")", ""; "concat for bnode and string")]
#[test_case("concat(042, \"x\")", ""; "concat for number and string")]
#[test_case("concat(<< <tag:s> <tag:p> <tag:o> >>, \"x\")", ""; "concat for triple and string")]
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
#[test_case("langMatches(42, \"en\")", ""; "langMatches for number")]
#[test_case("langMatches(<< <tag:s> <tag:p> <tag:o> >>, \"en\")", ""; "langMatches for triple")]
#[test_case("langMatches(\"en\", <tag:x>)", ""; "langMatches for IRI as range")]
#[test_case("langMatches(\"en\", bnode())", ""; "langMatches for bnode as range")]
#[test_case("langMatches(\"en\", \"\")", ""; "langMatches for empty string as range")]
#[test_case("langMatches(\"en\", \"en\"@en)", ""; "langMatches for language string as range")]
#[test_case("langMatches(\"en\", 42)", ""; "langMatches for number as range")]
#[test_case("langMatches(\"en\", << <tag:s> <tag:p> <tag:o> >>)", ""; "langMatches for triple as range")]
// test subStr/2
#[test_case("subStr(<tag:x>, 2)", ""; "subStr/2 for IRI")]
#[test_case("subStr(bnode(), 2)", ""; "subStr/2 for bnode")]
#[test_case("subStr(\"foobar\", 2)", "\"oobar\""; "subStr/2 for string")]
#[test_case("subStr(\"foobar\"@en, 2)", "\"oobar\"@en"; "subStr/2 for language string")]
#[test_case("subStr(42, 2)", ""; "subStr/2 for number")]
#[test_case("subStr(<< <tag:s> <tag:p> <tag:o> >>, 2)", ""; "subStr/2 for triple")]
#[test_case("subStr(\"foobar\", <tag:x>)", ""; "subStr/2 for IRI as startLoc")]
#[test_case("subStr(\"foobar\", bnode())", ""; "subStr/2 for bnode as startLoc")]
#[test_case("subStr(\"foobar\", \"42\")", ""; "subStr/2 for string as startLoc")]
#[test_case("subStr(\"foobar\", \"42\"@en)", ""; "subStr/2 for language string as startLoc")]
#[test_case("subStr(\"foobar\", << <tag:s> <tag:p> <tag:o> >>)", ""; "subStr/2 for triple as startLoc")]
// test subStr/3
#[test_case("subStr(<tag:x>, 2, 3)", ""; "subStr/3 for IRI")]
#[test_case("subStr(bnode(), 2, 3)", ""; "subStr/3 for bnode")]
#[test_case("subStr(\"foobar\", 2, 3)", "\"oob\""; "subStr/3 for string")]
#[test_case("subStr(\"foobar\"@en, 2, 3)", "\"oob\"@en"; "subStr/3 for language string")]
#[test_case("subStr(42, 2, 3)", ""; "subStr/3 for number")]
#[test_case("subStr(<< <tag:s> <tag:p> <tag:o> >>, 2, 3)", ""; "subStr/3 for triple")]
#[test_case("subStr(\"foobar\", <tag:x>, 3)", ""; "subStr/3 for IRI as startLoc")]
#[test_case("subStr(\"foobar\", bnode(), 3)", ""; "subStr/3 for bnode as startLoc")]
#[test_case("subStr(\"foobar\", \"42\", 3)", ""; "subStr/3 for string as startLoc")]
#[test_case("subStr(\"foobar\", \"42\"@en, 3)", ""; "subStr/3 for language string as startLoc")]
#[test_case("subStr(\"foobar\", << <tag:s> <tag:p> <tag:o> >>, 3)", ""; "subStr/3 for triple as startLoc")]
#[test_case("subStr(\"foobar\", 2, <tag:x>)", ""; "subStr/3 for IRI as end")]
#[test_case("subStr(\"foobar\", 2, bnode())", ""; "subStr/3 for bnode as end")]
#[test_case("subStr(\"foobar\", 2, \"42\")", ""; "subStr/3 for string as end")]
#[test_case("subStr(\"foobar\", 2, \"42\"@en)", ""; "subStr/3 for language string as end")]
#[test_case("subStr(\"foobar\", 2, << <tag:s> <tag:p> <tag:o> >>)", ""; "subStr/3 for triple as end")]
// test strLen
#[test_case("strLen(<tag:x>)", ""; "strLen for IRI")]
#[test_case("strLen(bnode())", ""; "strLen for bnode")]
#[test_case("strLen(\"foobar\")", "6"; "strLen for string")]
#[test_case("strLen(\"foobar\"@en)", "6"; "strLen for language string")]
#[test_case("strLen(42)", ""; "strLen for number")]
#[test_case("strLen(<< <tag:s> <tag:p> <tag:o> >>)", ""; "strLen for triple")]
// TODO test other function calls
// test isIri
#[test_case("isIri(<tag:x>)", "true"; "isIri for IRI")]
#[test_case("isIri(\"a b\")", "false"; "isIri for string")]
#[test_case("isIri(\"chat\"@en)", "false"; "isIri for language string")]
#[test_case("isIri(042)", "false"; "isIri for number")]
#[test_case("isIri(<< <tag:s> <tag:p> <tag:o> >>)", "false"; "isIri for triple")]
#[test_case("isIri(42/0)", ""; "isIri error")]
// test iBlank
#[test_case("isBlank(<tag:x>)", "false"; "isBlank for IRI")]
#[test_case("isBlank(\"a b\")", "false"; "isBlank for string")]
#[test_case("isBlank(\"chat\"@en)", "false"; "isBlank for language string")]
#[test_case("isBlank(042)", "false"; "isBlank for number")]
#[test_case("isBlank(<< <tag:s> <tag:p> <tag:o> >>)", "false"; "isBlank for triple")]
#[test_case("isBlank(42/0)", ""; "isBlank error")]
// test isLieteral
#[test_case("isLiteral(<tag:x>)", "false"; "isLiteral for IRI")]
#[test_case("isLiteral(\"a b\")", "true"; "isLiteral for string")]
#[test_case("isLiteral(\"chat\"@en)", "true"; "isLiteral for language string")]
#[test_case("isLiteral(042)", "true"; "isLiteral for number")]
#[test_case("isLiteral(<< <tag:s> <tag:p> <tag:o> >>)", "false"; "isLiteral for triple")]
#[test_case("isLiteral(42/0)", ""; "isLiteral error")]
// test isNumeric
#[test_case("isNumeric(<tag:x>)", "false"; "isNumeric for IRI")]
#[test_case("isNumeric(\"a b\")", "false"; "isNumeric for string")]
#[test_case("isNumeric(\"chat\"@en)", "false"; "isNumeric for language string")]
#[test_case("isNumeric(042)", "true"; "isNumeric for integer")]
#[test_case("isNumeric(3.14)", "true"; "isNumeric for decimal")]
#[test_case("isNumeric(3.14e0)", "true"; "isNumeric for double")]
#[test_case("isNumeric(\"1\"^^xsd:float)", "true"; "isNumeric for float")]
#[test_case("isNumeric(\"a\"^^xsd:integer)", "false"; "isNumeric for malformed")]
#[test_case("isNumeric(<< <tag:s> <tag:p> <tag:o> >>)", "false"; "isNumeric for triple")]
#[test_case("isNumeric(42/0)", ""; "isNumeric error")]
// TODO test regex
#[test_case("triple(<tag:s>, <tag:p>, <tag:o>)", "<< <tag:s> <tag:p> <tag:o> >>"; "triple IRIs")]
#[test_case("triple(<tag:s>, <tag:p>, \"o\")", "<< <tag:s> <tag:p> \"o\" >>"; "triple literal object")]
#[test_case("isTriple(triple(bnode(), <tag:p>, <tag:o>))", "true"; "triple bnode subject")]
#[test_case("isTriple(triple(<tag:s>, <tag:p>, bnode()))", "true"; "triple bnode object")]
#[test_case("triple(\"s\", <tag:p>, <tag:o>)", ""; "triple literal subject errs")]
#[test_case("triple(<tag:s>, \"p\", <tag:o>)", ""; "triple literal predicate errs")]
#[test_case("triple(<tag:s>, bnode(), <tag:o>)", ""; "triple bnode predicate errs")]
// TODO test other function: subject, predictae, object
// test isTriple
#[test_case("isTriple(<tag:x>)", "false"; "isTriple for IRI")]
#[test_case("isTriple(bnode())", "false"; "isTriple for bnode")]
#[test_case("isTriple(\"a b\")", "false"; "isTriple for string")]
#[test_case("isTriple(\"chat\"@en)", "false"; "isTriple for language string")]
#[test_case("isTriple(042)", "false"; "isTriple for number")]
#[test_case("isTriple(<< <tag:s> <tag:p> <tag:o> >>)", "true"; "isTriple for triple")]
#[test_case("isTriple(42/0)", ""; "isTriple error")]
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
        Some(fale) => (FALSE, TRUE),
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
    let mut got = bindings_to_vec(bindings);
    got.sort();
    assert_eq!(vec!["_:b"], got);
    Ok(())
}

fn eval_expr(expr: &str) -> TestResult<String> {
    eprintln!("eval_expr: {expr}");
    let dataset = LightDataset::default();
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(
        &format!("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ({expr} as ?x) {{}}")
    )?;
    let bindings = dataset.query(&query)?.into_bindings();
    let mut got = bindings_to_vec(bindings);
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

fn bindings_to_vec(bindings: Bindings<LightDataset>) -> Vec<String> {
    assert_eq!(bindings.variables().len(), 1);
    bindings
        .into_iter()
        .map(|b| {
            b.unwrap()[0]
                .as_ref()
                .map(|t| {
                    if t.is_blank_node() {
                        "_:b".to_string()
                    } else {
                        t.to_string()
                    }
                })
                .unwrap_or_default()
        })
        .collect()
}

type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;