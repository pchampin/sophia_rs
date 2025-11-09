use std::error::Error;

use crate::test::{LazyMap, ttl_samples};

use super::*;
use sophia_api::graph::Graph;
use sophia_isomorphism::isomorphic_graphs;
use test_case::test_case;

static TESTS: LazyMap = ttl_samples();

#[test_case("empty")]
#[test_case("comment")]
#[test_case("version")]
#[test_case("triple i i i")]
#[test_case("triple b i i")]
#[test_case("triple i i b")]
#[test_case("triple b i b")]
#[test_case("triple i i l")]
#[test_case("triple b i l")]
#[test_case("triple i i ld")]
#[test_case("triple b i ld")]
#[test_case("triple i i ll")]
#[test_case("triple b i ll")]
#[test_case("triple i i lb")]
#[test_case("triple b i lb")]
#[test_case("triple i i t")]
#[test_case("triple b i t")]
#[test_case("escape")]
#[test_case("escape useless")]
#[test_case("factorized triples")]
#[test_case("pretty literals")]
#[test_case("unpretty literals")]
#[test_case("lists")]
#[test_case("subject list")]
#[test_case("malformed list")]
#[test_case("bnode cycles")]
#[test_case("reified subject")]
#[test_case("reified object")]
#[test_case("reified nested")]
#[test_case("annotation")]
#[test_case("annotation nested")]
#[test_case("anon in list")]
#[test_case("rdf:nil in reified triple")]
fn roundtrip_not_pretty(key: &str) -> Result<(), Box<dyn Error>> {
    let (ttl, _) = TESTS.get(key).unwrap();
    let g1: Vec<[SimpleTerm; 3]> = crate::parser::turtle::parse_str(ttl).collect_triples()?;

    let out = TurtleSerializer::new_stringifier()
        .serialize_triples(g1.triples())?
        .to_string();
    println!("{}", &out);

    let g2: Vec<[SimpleTerm; 3]> = crate::parser::turtle::parse_str(&out).collect_triples()?;

    assert!(isomorphic_graphs(&g1, &g2)?);
    Ok(())
}

#[test_case("empty")]
#[test_case("comment")]
#[test_case("version")]
#[test_case("triple i i i")]
#[test_case("triple b i i")]
#[test_case("triple i i b")]
#[test_case("triple b i b")]
#[test_case("triple i i l")]
#[test_case("triple b i l")]
#[test_case("triple i i ld")]
#[test_case("triple b i ld")]
#[test_case("triple i i ll")]
#[test_case("triple b i ll")]
#[test_case("triple i i lb")]
#[test_case("triple b i lb")]
#[test_case("triple i i t")]
#[test_case("triple b i t")]
#[test_case("escape")]
#[test_case("escape useless")]
#[test_case("factorized triples")]
#[test_case("pretty literals")]
#[test_case("unpretty literals")]
#[test_case("lists")]
#[test_case("subject list")]
#[test_case("malformed list")]
#[test_case("bnode cycles")]
#[test_case("reified subject")]
#[test_case("reified object")]
#[test_case("reified nested")]
#[test_case("annotation")]
#[test_case("annotation nested")]
#[test_case("anon in list")]
#[test_case("rdf:nil in reified triple")]
fn roundtrip_pretty(key: &str) -> Result<(), Box<dyn Error>> {
    let (ttl, _) = TESTS.get(key).unwrap();
    let g1: Vec<[SimpleTerm; 3]> = crate::parser::turtle::parse_str(ttl).collect_triples()?;
    let ugly = TurtleSerializer::new_stringifier()
        .serialize_triples(g1.triples())?
        .to_string();
    println!("\n>>> DEBUG ugly\n{}", &ugly);

    let mut prefix_map = TurtleConfig::default_prefix_map();
    prefix_map.push((
        Prefix::new_unchecked("".into()),
        Iri::new_unchecked("http://example.org/ns/".into()),
    ));
    let config = TurtleConfig::new()
        .with_pretty(true)
        .with_own_prefix_map(prefix_map);
    let pretty = TurtleSerializer::new_stringifier_with_config(config)
        .serialize_triples(g1.triples())?
        .to_string();
    println!("\n>>> DEBUG pretty\n{}", &pretty);

    let g2: Vec<[SimpleTerm; 3]> = crate::parser::turtle::parse_str(&pretty).collect_triples()?;
    let ugly = TurtleSerializer::new_stringifier()
        .serialize_triples(g2.triples())?
        .to_string();
    println!("\n>>> DEBUG pretty→ugly\n{}", &ugly);

    assert!(isomorphic_graphs(&g1, &g2)?);
    Ok(())
}
