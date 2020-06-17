use super::*;
use crate::dataset::Dataset;
use crate::quad::Quad;
use crate::triple::stream::TripleSource;
use lazy_static::lazy_static;
use sophia_api::ns::{rdf, xsd};
use sophia_api::term::test::TestTerm;
use sophia_api::term::CopiableTerm;

type BoxTerm = TestTerm<Box<str>>;
type StaticTerm = TestTerm<&'static str>;

pub const NS: &'static str = "http://example.org/";
lazy_static! {
    pub static ref ALICE: StaticTerm = StaticTerm::iri2(NS, "alice");
    pub static ref BOB: StaticTerm = StaticTerm::iri2(NS, "bob");
    pub static ref CHARLIE: StaticTerm = StaticTerm::iri2(NS, "charlie");
    pub static ref KNOWS: StaticTerm = StaticTerm::iri2(NS, "knows");
    pub static ref NAME: StaticTerm = StaticTerm::iri2(NS, "name");
    pub static ref PERSON: StaticTerm = StaticTerm::iri2(NS, "Person");
    pub static ref ALICE_LIT: StaticTerm = StaticTerm::lit_dt("Alice", xsd::string);
    pub static ref BOB_LIT: StaticTerm = StaticTerm::lit_dt("Bob", xsd::string);
}

fn make_dataset() -> Vec<[StaticTerm; 4]> {
    vec![
        [*ALICE, rdf::type_.into(), *PERSON, *ALICE],
        [*ALICE, *NAME, *ALICE_LIT, *ALICE],
        [*BOB, rdf::type_.into(), *PERSON, *BOB],
        [*BOB, *NAME, *BOB_LIT, *BOB],
        [*BOB, *KNOWS, *ALICE, *ALICE],
    ]
}

fn map_term(t: &StaticTerm) -> StaticTerm {
    if t == &ALICE as &StaticTerm {
        CHARLIE.clone()
    } else {
        t.clone()
    }
}

fn make_mapped_dataset() -> Vec<[StaticTerm; 4]> {
    vec![
        [*CHARLIE, rdf::type_.into(), *PERSON, *CHARLIE],
        [*CHARLIE, *NAME, *ALICE_LIT, *CHARLIE],
        [*BOB, rdf::type_.into(), *PERSON, *BOB],
        [*BOB, *NAME, *BOB_LIT, *BOB],
        [*BOB, *KNOWS, *CHARLIE, *CHARLIE],
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
fn add_to_dataset() {
    let d = make_dataset();
    let mut e: Vec<([BoxTerm; 3], Option<BoxTerm>)> = vec![];
    d.quads().add_to_dataset(&mut e).unwrap();
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
        .filter_map_quads(|q| {
            if q.s() == &BOB as &StaticTerm {
                Some((
                    [map_term(q.s()), map_term(q.p()), map_term(q.o())],
                    q.g().map(map_term),
                ))
            } else {
                None
            }
        })
        .for_each_quad(|q| {
            c += 1;
            assert!(e.contains(q.s(), q.p(), q.o(), q.g()).unwrap());
        })
        .unwrap();
    assert_eq!(c, 3);
}

#[test]
fn filter_map_quads_to_triples() {
    let d = make_dataset();
    let mut g = Vec::<[BoxTerm; 3]>::new();
    d.quads()
        .filter_map_quads(|q| -> Option<[BoxTerm; 3]> {
            if q.s() == &BOB as &StaticTerm {
                Some([q.s().copied(), q.p().copied(), q.o().copied()])
            } else {
                None
            }
        })
        .add_to_graph(&mut g)
        .unwrap();
    let d = &d[2..];
    assert_eq!(d.len(), g.len());
    for i in 0..d.len() {
        assert_eq!(d[i].s(), g[i].s());
        assert_eq!(d[i].p(), g[i].p());
        assert_eq!(d[i].o(), g[i].o());
    }
}

#[test]
fn filter_map_quads_iter() {
    let d = make_dataset();
    let v = d
        .quads()
        .filter_map_quads(|q| {
            if q.s() == &BOB as &StaticTerm {
                Some(q.o().value().to_string())
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
fn map_quads() {
    let d = make_dataset();
    let e = make_mapped_dataset();
    let mut c = 0;
    d.quads()
        .map_quads(|q| {
            (
                [map_term(q.s()), map_term(q.p()), map_term(q.o())],
                q.g().map(map_term),
            )
        })
        .for_each_quad(|q| {
            c += 1;
            assert!(e.contains(q.s(), q.p(), q.o(), q.g()).unwrap());
        })
        .unwrap();
    assert_eq!(c, e.len());
}

#[test]
fn map_quads_to_triple() {
    let d = make_dataset();
    let mut g = Vec::<[BoxTerm; 3]>::new();
    d.quads()
        .map_quads(|q| -> [BoxTerm; 3] { [q.s().copied(), q.p().copied(), q.o().copied()] })
        .add_to_graph(&mut g)
        .unwrap();
    assert_eq!(d.len(), g.len());
    for i in 0..d.len() {
        assert_eq!(d[i].s(), g[i].s());
        assert_eq!(d[i].p(), g[i].p());
        assert_eq!(d[i].o(), g[i].o());
    }
}

#[test]
fn map_quads_iter() {
    let d = make_dataset();
    let v = d
        .quads()
        .map_quads(|q| q.o().value().to_string())
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
