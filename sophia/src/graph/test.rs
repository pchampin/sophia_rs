//! Contains helper functions and macros for testing Graph implementations

use std::fmt::Debug;

use crate::graph::*;
use crate::ns::*;
use crate::triple::stream::*;
use crate::triple::streaming_mode::{TripleStreamingMode, UnsafeTriple};
use crate::triple::*;
use lazy_static::lazy_static;
pub use sophia_term; // required when test macro is used in other packages
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
    //
    pub static ref B1: StaticTerm = StaticTerm::new_bnode("1").unwrap();
    pub static ref B2: StaticTerm = StaticTerm::new_bnode("2").unwrap();
    pub static ref L1: StaticTerm = StaticTerm::from("lit1");
    pub static ref L2: StaticTerm = StaticTerm::from("lit2");
    pub static ref L2E: StaticTerm = StaticTerm::new_literal_lang("lit2", "en").unwrap();
    pub static ref V1: StaticTerm = StaticTerm::new_variable("v1").unwrap();
    pub static ref V2: StaticTerm = StaticTerm::new_variable("v2").unwrap();
    pub static ref V3: StaticTerm = StaticTerm::new_variable("v3").unwrap();
}

pub fn no_triple() -> impl TripleSource {
    let v = Vec::<[StaticTerm; 3]>::new();
    v.into_iter().as_triple_source()
}

pub fn some_triples() -> impl TripleSource {
    vec![
        [*C1, rdf::type_, rdfs::Class],
        [*C2, rdf::type_, rdfs::Class],
        [*C2, rdf::type_, rdfs::Resource],
        [*C2, rdfs::subClassOf, *C1],
        [*C2, rdfs::subClassOf, rdfs::Resource],
        //
        [*P1, rdf::type_, rdf::Property],
        [*P1, rdfs::domain, *C1],
        [*P1, rdfs::range, *C2],
        //
        [*P2, rdf::type_, rdf::Property],
        [*P2, rdfs::domain, *C2],
        [*P2, rdfs::range, *C2],
        //
        [*I1A, rdf::type_, *C1],
        [*I1B, rdf::type_, *C1],
        [*I2A, rdf::type_, *C2],
        [*I2B, rdf::type_, *C2],
        [*I1A, *P1, *I2A],
        [*I1B, *P1, *I2B],
        [*I2A, *P2, *I2B],
    ]
    .into_iter()
    .as_triple_source()
}

pub fn strict_node_types_triples() -> impl TripleSource {
    vec![
        [rdf::type_, rdf::type_, rdf::Property],
        [*B1, rdf::type_, *L1],
        [*B2, rdf::type_, *B1],
        [*B2, rdf::type_, *L2],
        [*B2, rdf::type_, *L2E],
    ]
    .into_iter()
    .as_triple_source()
}

pub fn generalized_node_types_triples() -> impl TripleSource {
    vec![
        [rdf::type_, rdf::type_, rdf::Property],
        [*B1, *B2, *B1],
        [*L2, *L1, *L1],
        [*V1, *V2, *V3],
        [*B2, *V1, *L2E],
    ]
    .into_iter()
    .as_triple_source()
}

pub fn as_box_t<T: Triple, E>(triple: Result<T, E>) -> [BoxTerm; 3]
where
    E: Debug,
{
    let triple = triple.unwrap();
    [
        triple.s().clone_into(),
        triple.p().clone_into(),
        triple.o().clone_into(),
    ]
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
    assert!(hint.0 <= val, "hint {:?} not consistent with {}", hint, val);
    assert!(
        val <= hint.1.unwrap_or(val),
        "hint {:?} not consistent with {}",
        hint,
        val
    )
}

/// Generates a test suite for implementations of
/// [`Graph`], [`CollectibleGraph`] and [`MutableGraph`].
///
/// If your type only implements [`Graph`] and [`CollectibleGraph`],
/// you should use [`test_immutable_graph_impl`] instead.
///
/// This macro is only available when the feature `test_macros` is enabled.
///
/// It accepts the following parameters:
/// * `module_name`: the name of the module to generate (defaults to `test`);
/// * `graph_impl`: the type to test, implementing [`Graph`], [`CollectibleGraph`] and [`MutableGraph`];
/// * `is_set`: a boolean, indicating if `graph_impl` implements [`SetGraph`]
///   (defaults to `true`);
/// * `is_gen`: a boolean, indicating if `graph_impl` supports the [generalized model]
///   (defaults to `true`);
/// * `graph_collector`: a function used to collect triples into an instance of `graph_impl`
///   (defaults to `graph_impl::from_triple_source`);
/// * `mt` is used internally, do not touch it...
///
/// [`Graph`]: graph/trait.Graph.html
/// [`CollectibleGraph`]: graph/trait.CollectibleGraph.html
/// [`MutableGraph`]: graph/trait.MutableGraph.html
/// [`test_immutable_graph_impl`]: ./macro.test_immutable_graph_impl
/// [`SetGraph`]: graph/trait.SetGraph.html
/// [generalized model]: ./index.html
#[macro_export]
macro_rules! test_graph_impl {
    ($graph_impl: ident) => {
        $crate::test_graph_impl!(test, $graph_impl);
    };
    ($module_name: ident, $graph_impl: ident) => {
        $crate::test_graph_impl!($module_name, $graph_impl, true);
    };
    ($module_name: ident, $graph_impl: ident, $is_set: expr) => {
        $crate::test_graph_impl!($module_name, $graph_impl, $is_set, true);
    };
    ($module_name: ident, $graph_impl: ident, $is_set: expr, $is_gen: expr) => {
        $crate::test_graph_impl!($module_name, $graph_impl, $is_set, $is_gen, $graph_impl::from_triple_source);
    };
    ($module_name: ident, $graph_impl: ident, $is_set: expr, $is_gen: expr, $graph_collector: path) => {
        $crate::test_graph_impl!($module_name, $graph_impl, $is_set, $is_gen, $graph_collector, {
            // these tests will only be performed for implementations of `MutableGraph`
            #[test]
            fn test_simple_mutations() -> MGResult<$graph_impl, ()> {
                let mut g: $graph_impl = $graph_collector(no_triple()).unwrap();
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
            fn test_no_duplicate() -> MGResult<$graph_impl, ()> {
                if $is_set {
                    let mut g: $graph_impl = $graph_collector(no_triple()).unwrap();
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
                let mut g: $graph_impl = $graph_collector(no_triple()).unwrap();
                assert_eq!(g.triples().count(), 0);
                let inserted = g.insert_all(some_triples()).unwrap();
                if $is_set {
                    assert_eq!(inserted, 18, "returned by insert_all");
                }
                assert_eq!(g.triples().count(), 18, "after insert_all");
                if $is_set {
                    let inserted = g.insert_all(some_triples()).unwrap();
                    assert_eq!(inserted, 0, "returned by insert_all again");
                    assert_eq!(g.triples().count(), 18, "after insert_all again");
                }
                let removed = g.remove_all(some_triples()).unwrap();
                if $is_set {
                    assert_eq!(removed, 18, "returned by remove_all");
                }
                assert_eq!(g.triples().count(), 0, "after remove_all");
                if $is_set {
                    let removed = g.remove_all(some_triples()).unwrap();
                    assert_eq!(removed, 0, "returned by remove_all again");
                    assert_eq!(g.triples().count(), 0, "after remove_all again");
                }
            }

            #[test]
            fn test_remove_matching() -> MGResult<$graph_impl, ()> {
                let mut g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let o_matcher = [C1.clone(), C2.clone()];
                g.remove_matching(&ANY, &rdf::type_, &o_matcher[..])?;
                assert_consistent_hint(14, g.triples().size_hint());
                Ok(())
            }

            #[test]
            fn test_retain_matching() -> MGResult<$graph_impl, ()> {
                let mut g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let o_matcher = [C1.clone(), C2.clone()];
                g.retain_matching(&ANY, &rdf::type_, &o_matcher[..])?;
                assert_consistent_hint(4, g.triples().size_hint());
                Ok(())
            }
        });
    };
    ($module_name: ident, $graph_impl: ident, $is_set: expr, $is_gen: expr, $graph_collector: path, { $($mt:tt)* }) => {
        #[cfg(test)]
        mod $module_name {
            use $crate::graph::test::*;
            use $crate::graph::*;
            use $crate::ns::*;
            use self::sophia_term::*;
            use self::sophia_term::matcher::ANY;

            #[allow(unused_imports)]
            use super::*;

            #[test]
            fn test_triples() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples();
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), g.triples().count());
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &P1, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_s() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_s(&C2);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&*C2, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 4);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_p() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_p(&rdf::type_);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &rdf::type_, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
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
            fn test_triples_with_o() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_o(&C2);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &ANY, &*C2)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 5);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &P2, &rdfs::domain, &C2)?);
                    assert!(!Graph::contains(&v, &P1, &rdfs::domain, &C1)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_sp() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_sp(&C2, &rdf::type_);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&*C2, &rdf::type_, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_so() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_so(&C2, &rdfs::Resource);
                let hint = triples.size_hint();

                for iter in vec![triples, g.triples_matching(&*C2, &ANY, &rdfs::Resource)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
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
            fn test_triples_with_po() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_po(&rdf::type_, &rdfs::Class);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &rdf::type_, &rdfs::Class)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &P2, &rdf::type_, &rdf::Property)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_spo() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_spo(&C2, &rdf::type_, &rdfs::Resource);
                let hint = triples.size_hint();
                for iter in vec![
                    triples,
                    g.triples_matching(&*C2, &rdf::type_, &rdfs::Resource),
                ] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
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
            fn test_contains() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                assert!(Graph::contains(&g, &C2, &rdfs::subClassOf, &C1)?);
                assert!(!Graph::contains(&g, &C1, &rdfs::subClassOf, &C2)?);
                Ok(())
            }

            #[test]
            fn test_triples_matching() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let p_matcher: [StaticTerm; 2] = [rdf::type_.clone(), rdfs::domain.clone()];
                let o_matcher: [StaticTerm; 2] = [C2.clone(), rdfs::Class.clone()];
                let v: Vec<_> = g
                    .triples_matching(&ANY, &p_matcher[..], &o_matcher[..])
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
            fn test_subjects() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let subjects = g.subjects().unwrap();
                assert_eq!(subjects.len(), 8);

                let rsubjects: std::collections::HashSet<_> =
                    subjects.iter().map(|t| t.as_ref_str()).collect();
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
            fn test_predicates() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let predicates = g.predicates().unwrap();
                assert_eq!(predicates.len(), 6);

                let rpredicates: std::collections::HashSet<_> =
                    predicates.iter().map(|t| t.as_ref_str()).collect();
                assert!(rpredicates.contains(&rdf::type_));
                assert!(rpredicates.contains(&rdfs::subClassOf));
                assert!(rpredicates.contains(&rdfs::domain));
                assert!(rpredicates.contains(&rdfs::range));
                assert!(rpredicates.contains(&P1));
                assert!(rpredicates.contains(&P2));
                Ok(())
            }

            #[test]
            fn test_objects() -> MGResult<$graph_impl, ()> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let objects = g.objects().unwrap();
                assert_eq!(objects.len(), 7);

                let robjects: std::collections::HashSet<_> =
                    objects.iter().map(|t| t.as_ref_str()).collect();
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
            fn test_iris() -> MGResult<$graph_impl, ()> {
                let g = if $is_gen {
                    $graph_collector(generalized_node_types_triples()).unwrap()
                } else {
                    $graph_collector(strict_node_types_triples()).unwrap()
                };

                let iris = g.iris().unwrap();
                assert_eq!(iris.len(), 2);

                let riris: std::collections::HashSet<_> =
                    iris.iter().map(|t| t.as_ref_str()).collect();
                assert!(riris.contains(&rdf::Property));
                assert!(riris.contains(&rdf::type_));
                Ok(())
            }

            #[test]
            fn test_bnodes() -> MGResult<$graph_impl, ()> {
                let g = if $is_gen {
                    $graph_collector(generalized_node_types_triples()).unwrap()
                } else {
                    $graph_collector(strict_node_types_triples()).unwrap()
                };

                let bnodes = g.bnodes().unwrap();
                assert_eq!(bnodes.len(), 2);

                let rbnodes: std::collections::HashSet<_> =
                    bnodes.iter().map(|t| t.value()).collect();
                assert!(rbnodes.contains("1"));
                assert!(rbnodes.contains("2"));
                Ok(())
            }

            #[test]
            fn test_literals() -> MGResult<$graph_impl, ()> {
                let g = if $is_gen {
                    $graph_collector(generalized_node_types_triples()).unwrap()
                } else {
                    $graph_collector(strict_node_types_triples()).unwrap()
                };

                let literals = g.literals().unwrap();
                assert_eq!(literals.len(), 3);

                let rliterals: std::collections::HashSet<_> =
                    literals.iter().map(|t| t.as_ref_str()).collect();
                assert!(rliterals.contains(&StaticTerm::from("lit1")));
                assert!(rliterals.contains(&StaticTerm::from("lit2")));
                assert!(rliterals.contains(&StaticTerm::new_literal_lang("lit2", "en").unwrap()));
                Ok(())
            }

            #[test]
            fn test_variables() -> MGResult<$graph_impl, ()> {
                if $is_gen {
                    let g: $graph_impl = $graph_collector(generalized_node_types_triples()).unwrap();

                    let variables = g.variables().unwrap();
                    assert_eq!(variables.len(), 3);

                    let rvariables: std::collections::HashSet<_> =
                        variables.iter().map(|t| t.value()).collect();
                    assert!(rvariables.contains("v1"));
                    assert!(rvariables.contains("v2"));
                    assert!(rvariables.contains("v3"));
                } else {
                    let g: $graph_impl = $graph_collector(strict_node_types_triples()).unwrap();

                    let variables = g.variables().unwrap();
                    assert_eq!(variables.len(), 0);
                }
                Ok(())
            }

            // Tests for MutableGraph only, if enabled:
            $($mt)*
        }
    };
}

/// Generates a test suite for implementations of
/// [`Graph`], [`CollectibleGraph`].
///
/// If your type also implements [`MutableGraph`],
/// you should use [`test_graph_impl`] instead.
///
/// This macro is only available when the feature `test_macros` is enabled.
///
/// It accepts the following parameters:
/// * `module_name`: the name of the module to generate (defaults to `test`);
/// * `graph_impl`: the type to test, implementing [`Graph`] and [`CollectibleGraph`];
/// * `is_set`: a boolean, indicating if `graph_impl` implements [`SetGraph`]
///   (defaults to `true`);
/// * `is_gen`: a boolean, indicating if `graph_impl` supports the [generalized model]
///   (defaults to `true`);
/// * `graph_collector`: a function used to collect triples into an instance of `graph_impl`
///   (defaults to `graph_impl::from_triple_source`);
///
/// [`Graph`]: graph/trait.Graph.html
/// [`CollectibleGraph`]: graph/trait.CollectibleGraph.html
/// [`MutableGraph`]: graph/trait.MutableGraph.html
/// [`test_graph_impl`]: ./macro.test_graph_impl
/// [`SetGraph`]: graph/trait.SetGraph.html
/// [generalized model]: ./index.html
#[macro_export]
macro_rules! test_immutable_graph_impl {
    ($graph_impl: ident) => {
        $crate::test_immutable_graph_impl!(test, $graph_impl);
    };
    ($module_name: ident, $graph_impl: ident) => {
        $crate::test_immutable_graph_impl!($module_name, $graph_impl, true);
    };
    ($module_name: ident, $graph_impl: ident, $is_set: expr) => {
        $crate::test_immutable_graph_impl!($module_name, $graph_impl, $is_set, true);
    };
    ($module_name: ident, $graph_impl: ident, $is_set: expr, $is_gen: expr) => {
        $crate::test_immutable_graph_impl!(
            $module_name,
            $graph_impl,
            $is_set,
            $is_gen,
            $graph_impl::from_triple_source
        );
    };
    ($module_name: ident, $graph_impl: ident, $is_set: expr, $is_gen: expr, $graph_collector: path) => {
        // calling test_graph_impl, but passing an empty block as mt (the mutability tests)
        $crate::test_graph_impl!(
            $module_name,
            $graph_impl,
            $is_set,
            $is_gen,
            $graph_collector,
            {}
        );
    };
}
