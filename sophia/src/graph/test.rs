//! Contains helper functions and macros for testing Graph implementations

use std::fmt::Debug;

use crate::graph::*;
use crate::ns::*;
use crate::triple::stream::*;
use crate::triple::streaming_mode::{TripleStreamingMode, UnsafeTriple};
use crate::triple::*;
use lazy_static::lazy_static;
use sophia_term::*;

pub const NS: &str = "http://example.org/";

lazy_static! {
    pub static ref C1: StaticTerm = StaticTerm::new_iri_suffixed(NS, "C1").unwrap();
    pub static ref C2: StaticTerm = StaticTerm::new_iri_suffixed(NS, "C2").unwrap();
    pub static ref P1: StaticTerm = StaticTerm::new_iri_suffixed(NS, "p1").unwrap();
    pub static ref P2: StaticTerm = StaticTerm::new_iri_suffixed(NS, "p2").unwrap();
    pub static ref I1A: StaticTerm = StaticTerm::new_iri_suffixed(NS, "I1A").unwrap();
    pub static ref I1B: StaticTerm = StaticTerm::new_iri_suffixed(NS, "I1B").unwrap();
    pub static ref I2A: StaticTerm = StaticTerm::new_iri_suffixed(NS, "I2A").unwrap();
    pub static ref I2B: StaticTerm = StaticTerm::new_iri_suffixed(NS, "I2B").unwrap();
}

pub fn populate<G: MutableGraph>(g: &mut G) -> MGResult<G, ()> {
    g.insert(&C1, &rdf::type_, &rdfs::Class)?;

    g.insert(&C2, &rdf::type_, &rdfs::Class)?;
    g.insert(&C2, &rdf::type_, &rdfs::Resource)?;
    g.insert(&C2, &rdfs::subClassOf, &C1)?;
    g.insert(&C2, &rdfs::subClassOf, &rdfs::Resource)?;

    g.insert(&P1, &rdf::type_, &rdf::Property)?;
    g.insert(&P1, &rdfs::domain, &C1)?;
    g.insert(&P1, &rdfs::range, &C2)?;

    g.insert(&P2, &rdf::type_, &rdf::Property)?;
    g.insert(&P2, &rdfs::domain, &C2)?;
    g.insert(&P2, &rdfs::range, &C2)?;

    g.insert(&I1A, &rdf::type_, &C1)?;
    g.insert(&I1B, &rdf::type_, &C1)?;
    g.insert(&I2A, &rdf::type_, &C2)?;
    g.insert(&I2B, &rdf::type_, &C2)?;
    g.insert(&I1A, &P1, &I2A)?;
    g.insert(&I1B, &P1, &I2B)?;
    g.insert(&I2A, &P2, &I2B)?;

    assert_consistent_hint(18, g.triples().size_hint());
    Ok(())
}

pub fn populate_nodes_types<G: MutableGraph>(g: &mut G) -> MGResult<G, ()> {
    g.insert(&rdf::type_, &rdf::type_, &rdf::Property)?;
    g.insert(
        &StaticTerm::new_bnode("b1").unwrap(),
        &StaticTerm::new_bnode("b2").unwrap(),
        &StaticTerm::new_bnode("b1").unwrap(),
    )?;
    g.insert(
        &StaticTerm::from("lit2"),
        &StaticTerm::from("lit1"),
        &StaticTerm::from("lit1"),
    )?;
    g.insert(
        &StaticTerm::new_variable("v1").unwrap(),
        &StaticTerm::new_variable("v2").unwrap(),
        &StaticTerm::new_variable("v3").unwrap(),
    )?;
    g.insert(
        &StaticTerm::new_bnode("b2").unwrap(),
        &StaticTerm::new_variable("v1").unwrap(),
        &StaticTerm::new_literal_lang("lit2", "en").unwrap(),
    )?;

    assert_consistent_hint(5, g.triples().size_hint());
    Ok(())
}

pub fn as_box_t<T: Triple>(triple: T) -> [BoxTerm; 3] {
    [triple.s().into(), triple.p().into(), triple.o().into()]
}

#[allow(dead_code)]
pub fn dump_graph<G: Graph>(g: &G)
where
    <<G::Triple as TripleStreamingMode>::UnsafeTriple as UnsafeTriple>::TermData: Debug,
{
    println!("<<<<");
    for t in g.triples() {
        let t = t.unwrap();
        println!("{:?}\n{:?}\n{:?}\n\n", t.s(), t.p(), t.o());
    }
    println!(">>>>");
}

pub fn assert_consistent_hint(val: usize, hint: (usize, Option<usize>)) {
    assert!(hint.0 <= val);
    assert!(val <= hint.1.or(Some(val)).unwrap())
}

pub fn make_triple_source() -> impl TripleSource {
    vec![
        [&*C1, &rdf::type_, &rdfs::Class],
        [&*C1, &rdfs::subClassOf, &*C2],
    ]
    .into_iter()
    .as_triple_source()
}

/// Generates a test suite for [`Graph`] and [`MutableGraph`] implementations.
///
/// [`Graph`]: graph/trait.Graph.html
/// [`MutableGraph`]: graph/trait.MutableGraph.html
#[macro_export]
macro_rules! test_graph_impl {
    ($mutable_graph_impl: ident) => {
        test_graph_impl!(test, $mutable_graph_impl);
    };
    ($module_name: ident, $mutable_graph_impl: ident) => {
        test_graph_impl!($module_name, $mutable_graph_impl, true);
    };
    ($module_name: ident, $mutable_graph_impl: ident, $is_set: expr) => {
        test_graph_impl!(
            $module_name,
            $mutable_graph_impl,
            $is_set,
            $mutable_graph_impl::new
        );
    };
    ($module_name: ident, $mutable_graph_impl: ident, $is_set: expr, $mutable_graph_factory: path) => {
        #[cfg(test)]
        mod $module_name {
            use resiter::oks::*;
            use sophia_term::matcher::ANY;
            use sophia_term::*;
            use $crate::graph::test::*;
            use $crate::graph::*;
            use $crate::ns::*;

            #[allow(unused_imports)]
            use super::*;

            // test MutableGraph + SetGraph

            #[test]
            fn test_simple_mutations() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                assert_eq!(g.triples().count(), 0);
                assert!(MutableGraph::insert(
                    &mut g,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class
                )?);
                assert_eq!(g.triples().count(), 1);
                assert!(MutableGraph::insert(&mut g, &C1, &rdfs::subClassOf, &C2)?);
                assert_eq!(g.triples().count(), 2);
                assert!(MutableGraph::remove(
                    &mut g,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class
                )?);
                assert_eq!(g.triples().count(), 1);
                assert!(MutableGraph::remove(&mut g, &C1, &rdfs::subClassOf, &C2)?);
                assert_eq!(g.triples().count(), 0);
                Ok(())
            }

            #[test]
            fn test_no_duplicate() -> MGResult<$mutable_graph_impl, ()> {
                if $is_set {
                    let mut g = $mutable_graph_factory();
                    assert_eq!(g.triples().count(), 0);
                    assert!(MutableGraph::insert(
                        &mut g,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class
                    )?);
                    assert_eq!(g.triples().count(), 1);
                    assert!(!MutableGraph::insert(
                        &mut g,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class
                    )?);
                    assert_eq!(g.triples().count(), 1);
                    assert!(MutableGraph::remove(
                        &mut g,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class
                    )?);
                    assert_eq!(g.triples().count(), 0);
                    assert!(!MutableGraph::remove(
                        &mut g,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class
                    )?);
                    assert_eq!(g.triples().count(), 0);
                } else {
                    println!("effectively skipped, since is_set is false");
                }
                Ok(())
            }

            #[test]
            fn test_x_all_mutations() {
                let mut g = $mutable_graph_factory();
                assert_eq!(g.triples().count(), 0);
                assert_eq!(g.insert_all(&mut make_triple_source()).unwrap(), 2);
                assert_eq!(g.triples().count(), 2);
                if $is_set {
                    assert_eq!(g.insert_all(&mut make_triple_source()).unwrap(), 0);
                    assert_eq!(g.triples().count(), 2);
                }
                assert_eq!(g.remove_all(&mut make_triple_source()).unwrap(), 2);
                assert_eq!(g.triples().count(), 0);
                assert_eq!(g.remove_all(&mut make_triple_source()).unwrap(), 0);
                assert_eq!(g.triples().count(), 0);
            }

            #[test]
            fn test_remove_matching() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let o_matcher = [C1.clone(), C2.clone()];
                g.remove_matching(&ANY, &rdf::type_, &o_matcher[..])?;
                assert_consistent_hint(14, g.triples().size_hint());
                Ok(())
            }

            #[test]
            fn test_retain_matching() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let o_matcher = [C1.clone(), C2.clone()];
                g.retain_matching(&ANY, &rdf::type_, &o_matcher[..])?;
                assert_consistent_hint(4, g.triples().size_hint());
                Ok(())
            }

            // Test Graph

            #[test]
            fn test_triples() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples();
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &ANY, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), g.triples().count());
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &P1, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_s() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples_with_s(&C2);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&*C2, &ANY, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), 4);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_p() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples_with_p(&rdf::type_);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &rdf::type_, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), 9);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Resource)?);
                    assert!(!Graph::contains(
                        &v,
                        &C2,
                        &rdfs::subClassOf,
                        &rdfs::Resource
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_o() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples_with_o(&C2);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &ANY, &*C2)] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), 5);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &P2, &rdfs::domain, &C2)?);
                    assert!(!Graph::contains(&v, &P1, &rdfs::domain, &C1)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_sp() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples_with_sp(&C2, &rdf::type_);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&*C2, &rdf::type_, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_so() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples_with_so(&C2, &rdfs::Resource);
                let hint = triples.size_hint();

                for iter in vec![triples, g.triples_matching(&*C2, &ANY, &rdfs::Resource)] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(
                        &v,
                        &C2,
                        &rdfs::subClassOf,
                        &rdfs::Resource
                    )?);
                    assert!(!Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_po() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples_with_po(&rdf::type_, &rdfs::Class);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &rdf::type_, &rdfs::Class)] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &P2, &rdf::type_, &rdf::Property)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_spo() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let triples = g.triples_with_spo(&C2, &rdf::type_, &rdfs::Resource);
                let hint = triples.size_hint();
                for iter in vec![
                    triples,
                    g.triples_matching(&*C2, &rdf::type_, &rdfs::Resource),
                ] {
                    let v: Vec<_> = iter.oks().map(as_box_t).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Resource)?);
                    assert!(!Graph::contains(
                        &v,
                        &C1,
                        &rdfs::subClassOf,
                        &rdfs::Resource
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_contains() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;
                assert!(Graph::contains(&g, &C2, &rdfs::subClassOf, &C1)?);
                assert!(!Graph::contains(&g, &C1, &rdfs::subClassOf, &C2)?);
                Ok(())
            }

            #[test]
            fn test_triples_matching() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let p_matcher: [StaticTerm; 2] = [rdf::type_.clone(), rdfs::domain.clone()];
                let o_matcher: [StaticTerm; 2] = [C2.clone(), rdfs::Class.clone()];
                let v: Vec<_> = g
                    .triples_matching(&ANY, &p_matcher[..], &o_matcher[..])
                    .oks()
                    .map(as_box_t)
                    .collect();
                assert_eq!(v.len(), 5);
                assert!(Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                assert!(Graph::contains(&v, &P2, &rdfs::domain, &C2)?);
                assert!(Graph::contains(&v, &I2A, &rdf::type_, &C2)?);
                assert!(!Graph::contains(&v, &P1, &rdfs::domain, &C1)?);
                assert!(!Graph::contains(&v, &I1A, &rdf::type_, &C1)?);
                Ok(())
            }

            #[test]
            fn test_subjects() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let subjects = g.subjects().unwrap();
                assert_eq!(subjects.len(), 8);

                let rsubjects: std::collections::HashSet<_> =
                    subjects.iter().map(|t| RefTerm::from(t)).collect();
                assert!(rsubjects.contains(&C1));
                assert!(rsubjects.contains(&C2));
                assert!(rsubjects.contains(&P1));
                assert!(rsubjects.contains(&P2));
                assert!(rsubjects.contains(&I1A));
                assert!(rsubjects.contains(&I1B));
                assert!(rsubjects.contains(&I2A));
                assert!(rsubjects.contains(&I2B));
                Ok(())
            }

            #[test]
            fn test_predicates() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let predicates = g.predicates().unwrap();
                assert_eq!(predicates.len(), 6);

                let rpredicates: std::collections::HashSet<_> =
                    predicates.iter().map(|t| RefTerm::from(t)).collect();
                assert!(rpredicates.contains(&rdf::type_));
                assert!(rpredicates.contains(&rdfs::subClassOf));
                assert!(rpredicates.contains(&rdfs::domain));
                assert!(rpredicates.contains(&rdfs::range));
                assert!(rpredicates.contains(&P1));
                assert!(rpredicates.contains(&P2));
                Ok(())
            }

            #[test]
            fn test_objects() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate(&mut g)?;

                let objects = g.objects().unwrap();
                assert_eq!(objects.len(), 7);

                let robjects: std::collections::HashSet<_> =
                    objects.iter().map(|t| RefTerm::from(t)).collect();
                assert!(robjects.contains(&rdf::Property));
                assert!(robjects.contains(&rdfs::Class));
                assert!(robjects.contains(&rdfs::Resource));
                assert!(robjects.contains(&C1));
                assert!(robjects.contains(&C2));
                assert!(robjects.contains(&I2A));
                assert!(robjects.contains(&I2B));
                Ok(())
            }

            #[test]
            fn test_iris() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate_nodes_types(&mut g)?;

                let iris = g.iris().unwrap();
                assert_eq!(iris.len(), 2);

                let riris: std::collections::HashSet<_> =
                    iris.iter().map(|t| RefTerm::from(t)).collect();
                assert!(riris.contains(&rdf::Property));
                assert!(riris.contains(&rdf::type_));
                Ok(())
            }

            #[test]
            fn test_bnodes() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate_nodes_types(&mut g)?;

                let bnodes = g.bnodes().unwrap();
                assert_eq!(bnodes.len(), 2);

                let rbnodes: std::collections::HashSet<_> =
                    bnodes.iter().map(|t| t.value()).collect();
                assert!(rbnodes.contains("b1"));
                assert!(rbnodes.contains("b2"));
                Ok(())
            }

            #[test]
            fn test_literals() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate_nodes_types(&mut g)?;

                let literals = g.literals().unwrap();
                assert_eq!(literals.len(), 3);

                let rliterals: std::collections::HashSet<_> =
                    literals.iter().map(|t| RefTerm::from(t)).collect();
                assert!(rliterals.contains(&StaticTerm::from("lit1")));
                assert!(rliterals.contains(&StaticTerm::from("lit2")));
                assert!(rliterals.contains(&StaticTerm::new_literal_lang("lit2", "en").unwrap()));
                Ok(())
            }

            #[test]
            fn test_variables() -> MGResult<$mutable_graph_impl, ()> {
                let mut g = $mutable_graph_factory();
                populate_nodes_types(&mut g)?;

                let variables = g.variables().unwrap();
                assert_eq!(variables.len(), 3);

                let rvariables: std::collections::HashSet<_> =
                    variables.iter().map(|t| t.value()).collect();
                assert!(rvariables.contains("v1"));
                assert!(rvariables.contains("v2"));
                assert!(rvariables.contains("v3"));
                Ok(())
            }
        }
    };
}
