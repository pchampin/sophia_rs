use std::error::Error;

use crate::test::{LazyMap, trig_samples};

use super::*;
use sophia_api::{dataset::Dataset, prefix::Prefix, quad::Spog, term::SimpleTerm};
use sophia_iri::Iri;
use sophia_isomorphism::isomorphic_datasets;
use test_case::test_case;

static TESTS: LazyMap = trig_samples();

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
#[test_case("empty in bracketed default graph")]
#[test_case("comment in bracketed default graph")]
#[test_case("triple i i i in bracketed default graph")]
#[test_case("triple b i i in bracketed default graph")]
#[test_case("triple i i b in bracketed default graph")]
#[test_case("triple b i b in bracketed default graph")]
#[test_case("triple i i l in bracketed default graph")]
#[test_case("triple b i l in bracketed default graph")]
#[test_case("triple i i ld in bracketed default graph")]
#[test_case("triple b i ld in bracketed default graph")]
#[test_case("triple i i ll in bracketed default graph")]
#[test_case("triple b i ll in bracketed default graph")]
#[test_case("triple i i lb in bracketed default graph")]
#[test_case("triple b i lb in bracketed default graph")]
#[test_case("triple i i t in bracketed default graph")]
#[test_case("triple b i t in bracketed default graph")]
#[test_case("escape in bracketed default graph")]
#[test_case("escape useless in bracketed default graph")]
#[test_case("factorized triples in bracketed default graph")]
#[test_case("pretty literals in bracketed default graph")]
#[test_case("unpretty literals in bracketed default graph")]
#[test_case("lists in bracketed default graph")]
#[test_case("subject list in bracketed default graph")]
#[test_case("malformed list in bracketed default graph")]
#[test_case("bnode cycles in bracketed default graph")]
#[test_case("reified subject in bracketed default graph")]
#[test_case("reified object in bracketed default graph")]
#[test_case("reified nested in bracketed default graph")]
#[test_case("annotation in bracketed default graph")]
#[test_case("annotation nested in bracketed default graph")]
#[test_case("anon in list in bracketed default graph")]
#[test_case("rdf:nil in reified triple in bracketed default graph")]
#[test_case("triple i i i in named graph")]
#[test_case("triple b i i in named graph")]
#[test_case("triple i i b in named graph")]
#[test_case("triple b i b in named graph")]
#[test_case("triple i i l in named graph")]
#[test_case("triple b i l in named graph")]
#[test_case("triple i i ld in named graph")]
#[test_case("triple b i ld in named graph")]
#[test_case("triple i i ll in named graph")]
#[test_case("triple b i ll in named graph")]
#[test_case("triple i i lb in named graph")]
#[test_case("triple b i lb in named graph")]
#[test_case("triple i i t in named graph")]
#[test_case("triple b i t in named graph")]
#[test_case("escape in named graph")]
#[test_case("escape useless in named graph")]
#[test_case("factorized triples in named graph")]
#[test_case("pretty literals in named graph")]
#[test_case("unpretty literals in named graph")]
#[test_case("lists in named graph")]
#[test_case("subject list in named graph")]
#[test_case("malformed list in named graph")]
#[test_case("bnode cycles in named graph")]
#[test_case("reified subject in named graph")]
#[test_case("reified object in named graph")]
#[test_case("reified nested in named graph")]
#[test_case("annotation in named graph")]
#[test_case("annotation nested in named graph")]
#[test_case("anon in list in named graph")]
#[test_case("rdf:nil in reified triple in named graph")]
#[test_case("implicitly named graph iri")]
#[test_case("implicitly named graph pname")]
#[test_case("implicitly named graph bnode")]
#[test_case("implicitly named graph anon")]
#[test_case("explicitly named graph iri")]
#[test_case("explicitly named graph pname")]
#[test_case("explicitly named graph bnode")]
#[test_case("explicitly named graph anon")]
#[test_case("alternating graphs")]
fn roundtrip_not_pretty(key: &str) -> Result<(), Box<dyn Error>> {
    let (ttl, _) = TESTS.get(key).unwrap();
    let d1: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(ttl).collect_quads()?;

    let out = TriGSerializer::new_stringifier()
        .serialize_quads(d1.quads())?
        .to_string();
    println!("\n>>> DEBUG\n{}", &out);

    let d2: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(&out).collect_quads()?;

    assert!(isomorphic_datasets(&d1, &d2)?);
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
#[test_case("empty in bracketed default graph")]
#[test_case("comment in bracketed default graph")]
#[test_case("triple i i i in bracketed default graph")]
#[test_case("triple b i i in bracketed default graph")]
#[test_case("triple i i b in bracketed default graph")]
#[test_case("triple b i b in bracketed default graph")]
#[test_case("triple i i l in bracketed default graph")]
#[test_case("triple b i l in bracketed default graph")]
#[test_case("triple i i ld in bracketed default graph")]
#[test_case("triple b i ld in bracketed default graph")]
#[test_case("triple i i ll in bracketed default graph")]
#[test_case("triple b i ll in bracketed default graph")]
#[test_case("triple i i lb in bracketed default graph")]
#[test_case("triple b i lb in bracketed default graph")]
#[test_case("triple i i t in bracketed default graph")]
#[test_case("triple b i t in bracketed default graph")]
#[test_case("escape in bracketed default graph")]
#[test_case("escape useless in bracketed default graph")]
#[test_case("factorized triples in bracketed default graph")]
#[test_case("pretty literals in bracketed default graph")]
#[test_case("unpretty literals in bracketed default graph")]
#[test_case("lists in bracketed default graph")]
#[test_case("subject list in bracketed default graph")]
#[test_case("malformed list in bracketed default graph")]
#[test_case("bnode cycles in bracketed default graph")]
#[test_case("reified subject in bracketed default graph")]
#[test_case("reified object in bracketed default graph")]
#[test_case("reified nested in bracketed default graph")]
#[test_case("annotation in bracketed default graph")]
#[test_case("annotation nested in bracketed default graph")]
#[test_case("anon in list in bracketed default graph")]
#[test_case("rdf:nil in reified triple in bracketed default graph")]
#[test_case("triple i i i in named graph")]
#[test_case("triple b i i in named graph")]
#[test_case("triple i i b in named graph")]
#[test_case("triple b i b in named graph")]
#[test_case("triple i i l in named graph")]
#[test_case("triple b i l in named graph")]
#[test_case("triple i i ld in named graph")]
#[test_case("triple b i ld in named graph")]
#[test_case("triple i i ll in named graph")]
#[test_case("triple b i ll in named graph")]
#[test_case("triple i i lb in named graph")]
#[test_case("triple b i lb in named graph")]
#[test_case("triple i i t in named graph")]
#[test_case("triple b i t in named graph")]
#[test_case("escape in named graph")]
#[test_case("escape useless in named graph")]
#[test_case("factorized triples in named graph")]
#[test_case("pretty literals in named graph")]
#[test_case("unpretty literals in named graph")]
#[test_case("lists in named graph")]
#[test_case("subject list in named graph")]
#[test_case("malformed list in named graph")]
#[test_case("bnode cycles in named graph")]
#[test_case("reified subject in named graph")]
#[test_case("reified object in named graph")]
#[test_case("reified nested in named graph")]
#[test_case("annotation in named graph")]
#[test_case("annotation nested in named graph")]
#[test_case("anon in list in named graph")]
#[test_case("rdf:nil in reified triple in named graph")]
#[test_case("implicitly named graph iri")]
#[test_case("implicitly named graph pname")]
#[test_case("implicitly named graph bnode")]
#[test_case("implicitly named graph anon")]
#[test_case("explicitly named graph iri")]
#[test_case("explicitly named graph pname")]
#[test_case("explicitly named graph bnode")]
#[test_case("explicitly named graph anon")]
#[test_case("alternating graphs")]
fn roundtrip_pretty(key: &str) -> Result<(), Box<dyn Error>> {
    let (ttl, _) = TESTS.get(key).unwrap();
    let d1: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(ttl).collect_quads()?;
    let ugly = TriGSerializer::new_stringifier()
        .serialize_quads(d1.quads())?
        .to_string();
    println!("\n>>> DEBUG ugly\n{}", &ugly);

    let mut prefix_map = TriGConfig::default_prefix_map();
    prefix_map.push((
        Prefix::new_unchecked("".into()),
        Iri::new_unchecked("http://example.org/ns/".into()),
    ));
    let config = TriGConfig::new()
        .with_pretty(true)
        .with_own_prefix_map(prefix_map);
    let pretty = TriGSerializer::new_stringifier_with_config(config)
        .serialize_quads(d1.quads())?
        .to_string();
    println!("\n>>> DEBUG pretty\n{}", &pretty);

    let d2: Vec<Spog<SimpleTerm>> = crate::parser::trig::parse_str(&pretty).collect_quads()?;
    let ugly = TriGSerializer::new_stringifier()
        .serialize_quads(d2.quads())?
        .to_string();
    println!("\n>>> DEBUG pretty→ugly\n{}", &ugly);

    assert!(isomorphic_datasets(&d1, &d2)?);
    Ok(())
}
