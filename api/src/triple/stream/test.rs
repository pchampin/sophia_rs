use super::*;
use crate::graph::Graph;
use crate::ns::{rdf, xsd};
use crate::quad::stream::QuadSource;
use crate::term::test::TestTerm;
use crate::term::CopiableTerm;
use crate::triple::Triple;
use lazy_static::lazy_static;

type BoxTerm = TestTerm<Box<str>>;
type StaticTerm = TestTerm<&'static str>;

pub const NS: &str = "http://example.org/";
lazy_static! {
    pub static ref ALICE: StaticTerm = StaticTerm::iri2(NS, "alice");
    pub static ref BOB: StaticTerm = StaticTerm::iri2(NS, "bob");
    pub static ref CHARLIE: StaticTerm = StaticTerm::iri2(NS, "charlie");
    pub static ref KNOWS: StaticTerm = StaticTerm::iri2(NS, "knows");
    pub static ref NAME: StaticTerm = StaticTerm::iri2(NS, "name");
    pub static ref PERSON: StaticTerm = StaticTerm::iri2(NS, "Person");
    pub static ref ALICE_LIT: StaticTerm = StaticTerm::lit_dt("Alice", xsd::string);
    pub static ref BOB_LIT: StaticTerm = StaticTerm::lit_dt("Bob", xsd::string);

    // Relative IRIs
    pub static ref ALICE_REF: StaticTerm = StaticTerm::iri("alice");
    pub static ref BOB_REF: StaticTerm = StaticTerm::iri("bob");
    pub static ref CHARLIE_REF: StaticTerm = StaticTerm::iri("charlie");
    pub static ref KNOWS_REF: StaticTerm = StaticTerm::iri("knows");
    pub static ref NAME_REF: StaticTerm = StaticTerm::iri("name");
    pub static ref PERSON_REF: StaticTerm = StaticTerm::iri("Person");
}

fn make_graph() -> Vec<[StaticTerm; 3]> {
    vec![
        [*ALICE, rdf::type_.into(), *PERSON],
        [*ALICE, *NAME, *ALICE_LIT],
        [*BOB, rdf::type_.into(), *PERSON],
        [*BOB, *NAME, *BOB_LIT],
        [*BOB, *KNOWS, *ALICE],
    ]
}

fn map_term(t: &StaticTerm) -> StaticTerm {
    if t == &ALICE as &StaticTerm {
        *CHARLIE
    } else {
        *t
    }
}

fn make_mapped_graph() -> Vec<[StaticTerm; 3]> {
    vec![
        [*CHARLIE, rdf::type_.into(), *PERSON],
        [*CHARLIE, *NAME, *ALICE_LIT],
        [*BOB, rdf::type_.into(), *PERSON],
        [*BOB, *NAME, *BOB_LIT],
        [*BOB, *KNOWS, *CHARLIE],
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
fn add_to_graph() {
    let g = make_graph();
    let mut h: Vec<[BoxTerm; 3]> = vec![];
    g.triples().add_to_graph(&mut h).unwrap();
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
        .filter_map_triples(|t| {
            if t.s() == &BOB as &StaticTerm {
                Some([map_term(t.s()), map_term(t.p()), map_term(t.o())])
            } else {
                None
            }
        })
        .for_each_triple(|t| {
            c += 1;
            assert!(h.contains(t.s(), t.p(), t.o()).unwrap());
        })
        .unwrap();
    assert_eq!(c, 3);
}

#[test]
fn filter_map_triples_to_quads() {
    let g = make_graph();
    let mut d = Vec::<([BoxTerm; 3], Option<BoxTerm>)>::new();
    g.triples()
        .filter_map_triples(|t| -> Option<[BoxTerm; 4]> {
            if t.s() == &BOB as &StaticTerm {
                Some([
                    t.s().copied(),
                    t.p().copied(),
                    t.o().copied(),
                    t.s().copied(),
                ])
            } else {
                None
            }
        })
        .add_to_dataset(&mut d)
        .unwrap();
    let g = &g[2..];
    assert_eq!(g.len(), d.len());
    for i in 0..g.len() {
        assert_eq!(g[i].s(), d[i].s());
        assert_eq!(g[i].p(), d[i].p());
        assert_eq!(g[i].o(), d[i].o());
    }
}

#[test]
fn filter_map_triples_iter() {
    let g = make_graph();
    let v = g
        .triples()
        .filter_map_triples(|t| {
            if t.s() == &BOB as &StaticTerm {
                Some(t.o().value().to_string())
            } else {
                None
            }
        })
        .into_iter()
        .collect::<Result<Vec<String>, _>>()
        .unwrap();
    assert_eq!(
        &v[..],
        [
            "http://example.org/Person",
            "Bob",
            "http://example.org/alice",
        ]
    );
}

#[test]
fn map_triples() {
    let g = make_graph();
    let h = make_mapped_graph();
    let mut c = 0;
    g.triples()
        .map_triples(|t| [map_term(t.s()), map_term(t.p()), map_term(t.o())])
        .for_each_triple(|t| {
            c += 1;
            assert!(h.contains(t.s(), t.p(), t.o()).unwrap());
        })
        .unwrap();
    assert_eq!(c, h.len());
}

#[test]
fn map_triples_to_quads() {
    let g = make_graph();
    let mut d = Vec::<([BoxTerm; 3], Option<BoxTerm>)>::new();
    g.triples()
        .map_triples(|t| -> [BoxTerm; 4] {
            [
                t.s().copied(),
                t.p().copied(),
                t.o().copied(),
                t.s().copied(),
            ]
        })
        .add_to_dataset(&mut d)
        .unwrap();
    assert_eq!(g.len(), d.len());
    for i in 0..g.len() {
        assert_eq!(g[i].s(), d[i].s());
        assert_eq!(g[i].p(), d[i].p());
        assert_eq!(g[i].o(), d[i].o());
    }
}

#[test]
fn map_triples_iter() {
    let g = make_graph();
    let v = g
        .triples()
        .map_triples(|t| t.o().value().to_string())
        .into_iter()
        .collect::<Result<Vec<String>, _>>()
        .unwrap();
    assert_eq!(
        &v[..],
        [
            "http://example.org/Person",
            "Alice",
            "http://example.org/Person",
            "Bob",
            "http://example.org/alice",
        ]
    );
}

// TODO: Requires implementation of `resolve_triples()`.
// fn make_ref_graph() -> Vec<[Term<String>; 3]> {
//     vec![
//         [&ALICE_REF, &rdf::type_, &PERSON_REF],
//         [&BOB_REF, &rdf::type_, &PERSON_REF],
//         [&BOB_REF, &KNOWS_REF, &ALICE_REF],
//     ]
//     .into_iter()
//     .map(|t| [Term::from(t.s()), Term::from(t.p()), Term::from(t.o())])
//     .collect()
// }

// #[test]
// fn resolve_triple() {
//     let g = make_graph();
//     let g_ref = make_ref_graph();
//     let base = IriParsed::new(&NS).expect("Shouldn't fail");

//     g_ref
//         .triples()
//         .resolve_triples(base)
//         .for_each_triple(|t| {
//             assert!(g.contains(t.s(), t.p(), t.o()).unwrap());
//         })
//         .unwrap();
// }

// #[test]
// fn resolve_triples_iter() {
//     let g = make_graph();
//     let g_ref = make_ref_graph();
//     let base = IriParsed::new(&NS).expect("Shouldn't fail");

//     g_ref
//         .triples()
//         .resolve_triples(base)
//         .into_iter()
//         .for_each(|t| {
//             let t = t.unwrap();
//             assert!(g.contains(t.s(), t.p(), t.o()).unwrap());
//         });
// }
