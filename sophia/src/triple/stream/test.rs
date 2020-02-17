use super::*;
use crate::graph::Graph;
use crate::ns::{rdf, xsd};
use crate::term::BoxTerm;

pub(crate) const NS: &'static str = "http://example.org/";
lazy_static! {
    pub(crate) static ref ALICE: StaticTerm = StaticTerm::new_iri2(NS, "alice").unwrap();
    pub(crate) static ref BOB: StaticTerm = StaticTerm::new_iri2(NS, "bob").unwrap();
    pub(crate) static ref CHARLIE: StaticTerm = StaticTerm::new_iri2(NS, "charlie").unwrap();
    pub(crate) static ref KNOWS: StaticTerm = StaticTerm::new_iri2(NS, "knows").unwrap();
    pub(crate) static ref NAME: StaticTerm = StaticTerm::new_iri2(NS, "name").unwrap();
    pub(crate) static ref PERSON: StaticTerm = StaticTerm::new_iri2(NS, "Person").unwrap();
    pub(crate) static ref ALICE_LIT: StaticTerm =
        StaticTerm::new_literal_dt("Alice", xsd::string.clone()).unwrap();
    pub(crate) static ref BOB_LIT: StaticTerm =
        StaticTerm::new_literal_dt("Bob", xsd::string.clone()).unwrap();
}

fn make_graph() -> Vec<[&'static StaticTerm; 3]> {
    vec![
        [&ALICE, &rdf::type_, &PERSON],
        [&ALICE, &NAME, &ALICE_LIT],
        [&BOB, &rdf::type_, &PERSON],
        [&BOB, &NAME, &BOB_LIT],
        [&BOB, &KNOWS, &ALICE],
    ]
}

fn map_term(t: &StaticTerm) -> StaticTerm {
    if t == &ALICE as &StaticTerm {
        CHARLIE.clone()
    } else {
        t.clone()
    }
}

fn make_mapped_graph() -> Vec<[&'static StaticTerm; 3]> {
    vec![
        [&CHARLIE, &rdf::type_, &PERSON],
        [&CHARLIE, &NAME, &ALICE_LIT],
        [&BOB, &rdf::type_, &PERSON],
        [&BOB, &NAME, &BOB_LIT],
        [&BOB, &KNOWS, &CHARLIE],
    ]
}

#[test]
fn try_for_each_triple() {
    let g = make_graph();
    let mut c = 0;
    g.triples()
        .try_for_each_triple(|t| -> Result<(), std::convert::Infallible> {
            c += 1;
            assert!(g.contains(t.s(), t.p(), t.o())?);
            Ok(())
        })
        .unwrap();
    assert_eq!(c, g.len())
}

#[test]
fn for_some_triple() {
    let g = make_graph();
    let mut c = 0;
    let mut triples = g.triples();
    let has_more = triples
        .for_some_triple(&mut |t| {
            c += 1;
            assert!(g.contains(t.s(), t.p(), t.o()).unwrap());
        })
        .unwrap();
    assert!(c <= g.len());
    assert!(has_more || c == g.len());
}

#[test]
fn for_each_triple() {
    let g = make_graph();
    let mut c = 0;
    g.triples()
        .for_each_triple(|t| {
            c += 1;
            assert!(g.contains(t.s(), t.p(), t.o()).unwrap());
        })
        .unwrap();
    assert_eq!(c, g.len())
}

#[test]
fn in_graph() {
    let g = make_graph();
    let mut h: Vec<[BoxTerm; 3]> = vec![];
    g.triples().in_graph(&mut h).unwrap();
    assert_eq!(g.len(), h.len());
    for i in 0..g.len() {
        assert_eq!(g[i].s(), h[i].s());
        assert_eq!(g[i].p(), h[i].p());
        assert_eq!(g[i].o(), h[i].o());
    }
}

#[test]
fn filter_triples() {
    let g = make_graph();
    let mut c = 0;
    g.triples()
        .filter_triples(|t| t.s() == &BOB as &StaticTerm)
        .for_each_triple(|t| {
            c += 1;
            assert!(g.contains(t.s(), t.p(), t.o()).unwrap());
        })
        .unwrap();
    assert_eq!(c, 3);
}

#[test]
fn filter_map_triples() {
    let g = make_graph();
    let h = make_mapped_graph();
    let mut c = 0;
    g.triples()
        .filter_map_triples(|t| 
            if t.s() == &BOB as &StaticTerm {
                Some([
                    map_term(t.s()),
                    map_term(t.p()),
                    map_term(t.o()),
                ])
            } else {
                None
            }
        )
        .for_each_triple(|t| {
            c += 1;
            assert!(h.contains(t.s(), t.p(), t.o()).unwrap());
        })
        .unwrap();
    assert_eq!(c, 3);
}

#[test]
fn map_triples() {
    let g = make_graph();
    let h = make_mapped_graph();
    let mut c = 0;
    g.triples()
        .map_triples(|t| [
            map_term(t.s()),
            map_term(t.p()),
            map_term(t.o()),
        ])
        .for_each_triple(|t| {
            c += 1;
            assert!(h.contains(t.s(), t.p(), t.o()).unwrap());
        })
        .unwrap();
    assert_eq!(c, h.len());
}
