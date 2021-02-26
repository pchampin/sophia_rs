//! Contains helper functions and macros for testing Graph implementations

use std::fmt::Debug;

use crate::graph::*;
use crate::ns::*;
use crate::term::test::TestTerm;
use crate::term::CopiableTerm;
use crate::triple::stream::*;
use crate::triple::streaming_mode::{TripleStreamingMode, UnsafeTriple};
use crate::triple::*;
use lazy_static::lazy_static;

type StaticTerm = TestTerm<&'static str>;
type BoxTerm = TestTerm<Box<str>>;

pub const NS: &str = "http://example.org/";
lazy_static! {
    pub static ref C1: StaticTerm  = StaticTerm::iri2(NS, "C1");
    pub static ref C2: StaticTerm = StaticTerm::iri2(NS, "C2");
    pub static ref P1: StaticTerm = StaticTerm::iri2(NS, "p1");
    pub static ref P2: StaticTerm = StaticTerm::iri2(NS, "p2");
    pub static ref I1A: StaticTerm = StaticTerm::iri2(NS, "I1A");
    pub static ref I1B: StaticTerm = StaticTerm::iri2(NS, "I1B");
    pub static ref I2A: StaticTerm = StaticTerm::iri2(NS, "I2A");
    pub static ref I2B: StaticTerm = StaticTerm::iri2(NS, "I2B");
    //
    pub static ref B1: StaticTerm = StaticTerm::bnode("1");
    pub static ref B2: StaticTerm = StaticTerm::bnode("2");
    pub static ref L1: StaticTerm = StaticTerm::lit_dt("lit1", xsd::string);
    pub static ref L2: StaticTerm = StaticTerm::lit_dt("lit2", xsd::string);
    pub static ref L2E: StaticTerm = StaticTerm::lit_lang("lit2", "en");
    pub static ref V1: StaticTerm = StaticTerm::var("v1");
    pub static ref V2: StaticTerm = StaticTerm::var("v2");
    pub static ref V3: StaticTerm = StaticTerm::var("v3");
}

pub fn no_triple() -> impl TripleSource {
    let v = Vec::<[StaticTerm; 3]>::new();
    v.into_iter().into_triple_source()
}

pub fn some_triples() -> impl TripleSource {
    let v: Vec<[StaticTerm; 3]> = vec![
        [*C1, rdf::type_.into(), rdfs::Class.into()],
        [*C2, rdf::type_.into(), rdfs::Class.into()],
        [*C2, rdf::type_.into(), rdfs::Resource.into()],
        [*C2, rdfs::subClassOf.into(), *C1],
        [*C2, rdfs::subClassOf.into(), rdfs::Resource.into()],
        //
        [*P1, rdf::type_.into(), rdf::Property.into()],
        [*P1, rdfs::domain.into(), *C1],
        [*P1, rdfs::range.into(), *C2],
        //
        [*P2, rdf::type_.into(), rdf::Property.into()],
        [*P2, rdfs::domain.into(), *C2],
        [*P2, rdfs::range.into(), *C2],
        //
        [*I1A, rdf::type_.into(), *C1],
        [*I1B, rdf::type_.into(), *C1],
        [*I2A, rdf::type_.into(), *C2],
        [*I2B, rdf::type_.into(), *C2],
        [*I1A, *P1, *I2A],
        [*I1B, *P1, *I2B],
        [*I2A, *P2, *I2B],
    ];
    v.into_iter().into_triple_source()
}

pub fn strict_node_types_triples() -> impl TripleSource {
    let v: Vec<[StaticTerm; 3]> = vec![
        [rdf::type_.into(), rdf::type_.into(), rdf::Property.into()],
        [*B1, rdf::type_.into(), *L1],
        [*B2, rdf::type_.into(), *B1],
        [*B2, rdf::type_.into(), *L2],
        [*B2, rdf::type_.into(), *L2E],
    ];
    v.into_iter().into_triple_source()
}

pub fn generalized_node_types_triples() -> impl TripleSource {
    let v: Vec<[StaticTerm; 3]> = vec![
        [rdf::type_.into(), rdf::type_.into(), rdf::Property.into()],
        [*B1, *B2, *B1],
        [*L2, *L1, *L1],
        [*V1, *V2, *V3],
        [*B2, *V1, *L2E],
    ];
    v.into_iter().into_triple_source()
}

pub fn as_box_t<T: Triple, E>(triple: Result<T, E>) -> [BoxTerm; 3]
where
    E: Debug,
{
    let triple = triple.unwrap();
    [
        triple.s().copied(),
        triple.p().copied(),
        triple.o().copied(),
    ]
}

#[allow(dead_code)]
pub fn dump_graph<G: Graph>(g: &G)
where
    <<G::Triple as TripleStreamingMode>::UnsafeTriple as UnsafeTriple>::Term: Debug,
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

pub fn assert_contains<'a, I, T, U>(collection: I, item: &U)
where
    I: IntoIterator<Item = &'a T>,
    T: 'a,
    U: PartialEq<T>,
{
    assert!(collection.into_iter().any(|i| item == i))
}

/// Generate a test suite for an implementation of
/// [`Graph`], [`CollectibleGraph`] and [`MutableGraph`].
///
/// If your type only implements [`Graph`] and [`CollectibleGraph`],
/// you should use [`test_immutable_graph_impl`] instead.
///
/// This macro is only available when the feature `test_macro` is enabled.
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
/// [`test_immutable_graph_impl`]: ./macro.test_immutable_graph_impl.html
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
            fn test_simple_mutations() -> Result<(), Box<dyn std::error::Error>> {
                let mut g: $graph_impl = $graph_collector(no_triple()).unwrap();
                assert_eq!(g.triples().count(), 0);
                assert!(MutableGraph::insert(
                    &mut g,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class
                )? || !$is_set);
                assert_eq!(g.triples().count(), 1);
                assert!(MutableGraph::insert(&mut g, &*C1, &rdfs::subClassOf, &*C2)? || !$is_set);
                assert_eq!(g.triples().count(), 2);
                assert!(MutableGraph::remove(
                    &mut g,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class
                )? || !$is_set);
                assert_eq!(g.triples().count(), 1);
                assert!(MutableGraph::remove(&mut g, &*C1, &rdfs::subClassOf, &*C2)? || !$is_set);
                assert_eq!(g.triples().count(), 0);
                Ok(())
            }

            #[test]
            fn test_no_duplicate() -> Result<(), Box<dyn std::error::Error>> {
                if $is_set {
                    let mut g: $graph_impl = $graph_collector(no_triple()).unwrap();
                    assert_eq!(g.triples().count(), 0);
                    assert!(MutableGraph::insert(
                        &mut g,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class
                    )? || !$is_set);
                    assert_eq!(g.triples().count(), 1);
                    assert!(!MutableGraph::insert(
                        &mut g,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class
                    )? || !$is_set);
                    assert_eq!(g.triples().count(), 1);
                    assert!(MutableGraph::remove(
                        &mut g,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class
                    )? || !$is_set);
                    assert_eq!(g.triples().count(), 0);
                    assert!(!MutableGraph::remove(
                        &mut g,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class
                    )? || !$is_set);
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
            fn test_remove_matching() -> Result<(), Box<dyn std::error::Error>> {
                let mut g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let o_matcher = [&*C1, &*C2];
                g.remove_matching(&ANY, &rdf::type_, &o_matcher)?;
                assert_consistent_hint(14, g.triples().size_hint());
                Ok(())
            }

            #[test]
            fn test_retain_matching() -> Result<(), Box<dyn std::error::Error>> {
                let mut g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let o_matcher = [&*C1, &*C2];
                g.retain_matching(&ANY, &rdf::type_, &o_matcher)?;
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
            use $crate::term::matcher::ANY;

            #[allow(unused_imports)]
            use super::*;

            #[test]
            fn test_triples() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples();
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), g.triples().count());
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &*C1, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &*P1, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_s() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_s(&*C2);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&*C2, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 4);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &*C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &*C1, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_p() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_p(&rdf::type_);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &rdf::type_, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 9);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &*C2, &rdf::type_, &rdfs::Resource)?);
                    assert!(!Graph::contains(
                        &v,
                        &*C2,
                        &rdfs::subClassOf,
                        &rdfs::Resource
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_o() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_o(&*C2);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &ANY, &*C2)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 5);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &*P2, &rdfs::domain, &*C2)?);
                    assert!(!Graph::contains(&v, &*P1, &rdfs::domain, &*C1)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_sp() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_sp(&*C2, &rdf::type_);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&*C2, &rdf::type_, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &*C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &*C2, &rdfs::subClassOf, &*C1)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_so() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_so(&*C2, &rdfs::Resource);
                let hint = triples.size_hint();

                for iter in vec![triples, g.triples_matching(&*C2, &ANY, &rdfs::Resource)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(
                        &v,
                        &*C2,
                        &rdfs::subClassOf,
                        &rdfs::Resource
                    )?);
                    assert!(!Graph::contains(&v, &*C2, &rdf::type_, &rdfs::Class)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_po() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_po(&rdf::type_, &rdfs::Class);
                let hint = triples.size_hint();
                for iter in vec![triples, g.triples_matching(&ANY, &rdf::type_, &rdfs::Class)] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &*C2, &rdf::type_, &rdfs::Class)?);
                    assert!(!Graph::contains(&v, &*P2, &rdf::type_, &rdf::Property)?);
                }
                Ok(())
            }

            #[test]
            fn test_triples_with_spo() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let triples = g.triples_with_spo(&*C2, &rdf::type_, &rdfs::Resource);
                let hint = triples.size_hint();
                for iter in vec![
                    triples,
                    g.triples_matching(&*C2, &rdf::type_, &rdfs::Resource),
                ] {
                    let v: Vec<_> = iter.map(as_box_t).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Graph::contains(&v, &*C2, &rdf::type_, &rdfs::Resource)?);
                    assert!(!Graph::contains(
                        &v,
                        &*C1,
                        &rdfs::subClassOf,
                        &rdfs::Resource
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_contains() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                assert!(Graph::contains(&g, &*C2, &rdfs::subClassOf, &*C1)?);
                assert!(!Graph::contains(&g, &*C1, &rdfs::subClassOf, &*C2)?);
                Ok(())
            }

            #[test]
            fn test_triples_matching() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let p_matcher = [&rdf::type_, &rdfs::domain];
                let o_matcher = [&*C2, &rdfs::Class.into()];
                let v: Vec<_> = g
                    .triples_matching(&ANY, &p_matcher, &o_matcher)
                    .map(as_box_t)
                    .collect();
                assert_eq!(v.len(), 5);
                assert!(Graph::contains(&v, &*C1, &rdf::type_, &rdfs::Class)?);
                assert!(Graph::contains(&v, &*P2, &rdfs::domain, &*C2)?);
                assert!(Graph::contains(&v, &*I2A, &rdf::type_, &*C2)?);
                assert!(!Graph::contains(&v, &*P1, &rdfs::domain, &*C1)?);
                assert!(!Graph::contains(&v, &*I1A, &rdf::type_, &*C1)?);
                Ok(())
            }

            #[test]
            fn test_subjects() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let subjects = g.subjects().unwrap();
                assert_eq!(subjects.len(), 8);
                assert_contains(&subjects, &*C1);
                assert_contains(&subjects, &*C2);
                assert_contains(&subjects, &*P1);
                assert_contains(&subjects, &*P2);
                assert_contains(&subjects, &*I1A);
                assert_contains(&subjects, &*I1B);
                assert_contains(&subjects, &*I2A);
                assert_contains(&subjects, &*I2B);
                Ok(())
            }

            #[test]
            fn test_predicates() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let predicates = g.predicates().unwrap();
                assert_eq!(predicates.len(), 6);
                assert_contains(&predicates, &rdf::type_);
                assert_contains(&predicates, &rdfs::subClassOf);
                assert_contains(&predicates, &rdfs::domain);
                assert_contains(&predicates, &rdfs::range);
                assert_contains(&predicates, &*P1);
                assert_contains(&predicates, &*P2);
                Ok(())
            }

            #[test]
            fn test_objects() -> Result<(), Box<dyn std::error::Error>> {
                let g: $graph_impl = $graph_collector(some_triples()).unwrap();

                let objects = g.objects().unwrap();
                assert_eq!(objects.len(), 7);
                assert_contains(&objects, &rdf::Property);
                assert_contains(&objects, &rdfs::Class);
                assert_contains(&objects, &rdfs::Resource);
                assert_contains(&objects, &*C1);
                assert_contains(&objects, &*C2);
                assert_contains(&objects, &*I2A);
                assert_contains(&objects, &*I2B);
                Ok(())
            }

            #[test]
            fn test_iris() -> Result<(), Box<dyn std::error::Error>> {
                let g = if $is_gen {
                    $graph_collector(generalized_node_types_triples()).unwrap()
                } else {
                    $graph_collector(strict_node_types_triples()).unwrap()
                };

                let iris = g.iris().unwrap();
                assert_eq!(iris.len(), 2);
                assert_contains(&iris, &rdf::Property);
                assert_contains(&iris, &rdf::type_);
                Ok(())
            }

            #[test]
            fn test_bnodes() -> Result<(), Box<dyn std::error::Error>> {
                let g = if $is_gen {
                    $graph_collector(generalized_node_types_triples()).unwrap()
                } else {
                    $graph_collector(strict_node_types_triples()).unwrap()
                };

                let bnodes = g.bnodes().unwrap();
                assert_eq!(bnodes.len(), 2);
                assert_contains(&bnodes, &*B1);
                assert_contains(&bnodes, &*B2);
                Ok(())
            }

            #[test]
            fn test_literals() -> Result<(), Box<dyn std::error::Error>> {
                let g = if $is_gen {
                    $graph_collector(generalized_node_types_triples()).unwrap()
                } else {
                    $graph_collector(strict_node_types_triples()).unwrap()
                };

                let literals = g.literals().unwrap();
                assert_eq!(literals.len(), 3);
                assert_contains(&literals, &*L1);
                assert_contains(&literals, &*L2);
                assert_contains(&literals, &*L2E);
                Ok(())
            }

            #[test]
            fn test_variables() -> Result<(), Box<dyn std::error::Error>> {
                if $is_gen {
                    let g: $graph_impl = $graph_collector(generalized_node_types_triples()).unwrap();

                    let variables = g.variables().unwrap();
                    assert_eq!(variables.len(), 3);
                    assert_contains(&variables, &*V1);
                    assert_contains(&variables, &*V2);
                    assert_contains(&variables, &*V3);
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

/// Generate a test suite for an implementation of
/// [`Graph`] and [`CollectibleGraph`].
///
/// If your type also implements [`MutableGraph`],
/// you should use [`test_graph_impl`] instead.
///
/// This macro is only available when the feature `test_macro` is enabled.
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
/// [`test_graph_impl`]: ./macro.test_graph_impl.html
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
