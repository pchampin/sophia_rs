use super::*;
use crate::dataset::Dataset;
use crate::ns::{rdf, xsd};
use crate::term::BoxTerm;

pub const NS: &'static str = "http://example.org/";
lazy_static! {
    pub static ref ALICE: StaticTerm = StaticTerm::new_iri2(NS, "alice").unwrap();
    pub static ref BOB: StaticTerm = StaticTerm::new_iri2(NS, "bob").unwrap();
    pub static ref CHARLIE: StaticTerm = StaticTerm::new_iri2(NS, "charlie").unwrap();
    pub static ref KNOWS: StaticTerm = StaticTerm::new_iri2(NS, "knows").unwrap();
    pub static ref NAME: StaticTerm = StaticTerm::new_iri2(NS, "name").unwrap();
    pub static ref PERSON: StaticTerm = StaticTerm::new_iri2(NS, "Person").unwrap();
    pub static ref ALICE_LIT: StaticTerm =
        StaticTerm::new_literal_dt("Alice", xsd::string.clone()).unwrap();
    pub static ref BOB_LIT: StaticTerm =
        StaticTerm::new_literal_dt("Bob", xsd::string.clone()).unwrap();
}

fn make_dataset() -> Vec<[&'static StaticTerm; 4]> {
    vec![
        [&ALICE, &rdf::type_, &PERSON, &ALICE],
        [&ALICE, &NAME, &ALICE_LIT, &ALICE],
        [&BOB, &rdf::type_, &PERSON, &BOB],
        [&BOB, &NAME, &BOB_LIT, &BOB],
        [&BOB, &KNOWS, &ALICE, &ALICE],
    ]
}

fn map_term(t: &StaticTerm) -> StaticTerm {
    if t == &ALICE as &StaticTerm {
        CHARLIE.clone()
    } else {
        t.clone()
    }
}

fn make_mapped_dataset() -> Vec<[&'static StaticTerm; 4]> {
    vec![
        [&CHARLIE, &rdf::type_, &PERSON, &CHARLIE],
        [&CHARLIE, &NAME, &ALICE_LIT, &CHARLIE],
        [&BOB, &rdf::type_, &PERSON, &BOB],
        [&BOB, &NAME, &BOB_LIT, &BOB],
        [&BOB, &KNOWS, &CHARLIE, &CHARLIE],
    ]
}

#[test]
fn try_for_each_quad() {
    let d = make_dataset();
    let mut c = 0;
    d.quads()
        .try_for_each_quad(|q| -> Result<(), std::convert::Infallible> {
            c += 1;
            assert!(d.contains(q.s(), q.p(), q.o(), q.g())?);
            Ok(())
        })
        .unwrap();
    assert_eq!(c, d.len())
}

#[test]
fn for_some_quad() {
    let d = make_dataset();
    let mut c = 0;
    let mut quads = d.quads();
    let has_more = quads
        .for_some_quad(&mut |q| {
            c += 1;
            assert!(d.contains(q.s(), q.p(), q.o(), q.g()).unwrap());
        })
        .unwrap();
    assert!(c <= d.len());
    assert!(has_more || c == d.len());
}

#[test]
fn for_each_quads() {
    let d = make_dataset();
    let mut c = 0;
    d.quads()
        .for_each_quad(|q| {
            c += 1;
            assert!(d.contains(q.s(), q.p(), q.o(), q.g()).unwrap());
        })
        .unwrap();
    assert_eq!(c, d.len())
}

#[test]
fn in_dataset() {
    let d = make_dataset();
    let mut e: Vec<([BoxTerm; 3], Option<BoxTerm>)> = vec![];
    d.quads().in_dataset(&mut e).unwrap();
    assert_eq!(d.len(), e.len());
    for i in 0..d.len() {
        assert_eq!(d[i].s(), e[i].s());
        assert_eq!(d[i].p(), e[i].p());
        assert_eq!(d[i].o(), e[i].o());
        assert!(match (d[i].g(), e[i].g()) {
            (None, None) => true,
            (Some(td), Some(te)) => td == te,
            _ => false,
        })
    }
}

#[test]
fn filter_quads() {
    let d = make_dataset();
    let mut c = 0;
    d.quads()
        .filter_quads(|q| q.s() == &BOB as &StaticTerm)
        .for_each_quad(|q| {
            c += 1;
            assert!(d.contains(q.s(), q.p(), q.o(), q.g()).unwrap());
        })
        .unwrap();
    assert_eq!(c, 3);
}

#[test]
fn filter_map_quads() {
    let d = make_dataset();
    let e = make_mapped_dataset();
    let mut c = 0;
    d.quads()
        .filter_map_quads(|q| 
            if q.s() == &BOB as &StaticTerm {
                Some((
                    [
                        map_term(q.s()),
                        map_term(q.p()),
                        map_term(q.o()),
                    ],
                    q.g().map(map_term),
                ))
            } else {
                None
            }
        )
        .for_each_quad(|q| {
            c += 1;
            assert!(e.contains(q.s(), q.p(), q.o(), q.g()).unwrap());
        })
        .unwrap();
    assert_eq!(c, 3);
}

#[test]
fn map_quads() {
    let d = make_dataset();
    let e = make_mapped_dataset();
    let mut c = 0;
    d.quads()
        .map_quads(|q| (
            [
                map_term(q.s()),
                map_term(q.p()),
                map_term(q.o()),
            ],
            q.g().map(map_term),
        ))
        .for_each_quad(|q| {
            c += 1;
            assert!(e.contains(q.s(), q.p(), q.o(), q.g()).unwrap());
        })
        .unwrap();
    assert_eq!(c, e.len());
}

// TODO implement tests for mapping triples to quads

// TODO implement tests for mapping quads to triples
