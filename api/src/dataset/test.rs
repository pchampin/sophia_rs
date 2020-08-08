//! Contains helper functions and macros for testing Dataset implementations

use std::fmt::Debug;

use crate::dataset::*;
use crate::graph::test::*;
use crate::ns::*;
use crate::quad::stream::*;
use crate::quad::streaming_mode::{QuadStreamingMode, UnsafeQuad};
use crate::quad::*;
use crate::term::test::TestTerm;
use crate::term::{CopiableTerm, CopyTerm};
use lazy_static::lazy_static;

type StaticTerm = TestTerm<&'static str>;
type BoxTerm = TestTerm<Box<str>>;

lazy_static! {
    pub static ref G1: StaticTerm = StaticTerm::iri2(NS, "G1");
    pub static ref G2: StaticTerm = StaticTerm::iri2(NS, "G2");
    //
    pub static ref DG: Option<StaticTerm> = None;
    pub static ref GN1: Option<StaticTerm> = Some(*G1);
    pub static ref GN2: Option<StaticTerm> = Some(*G2);
}

pub fn no_quad() -> impl QuadSource {
    let v = Vec::<([StaticTerm; 3], Option<StaticTerm>)>::new();
    v.into_iter().as_quad_source()
}

pub fn some_quads() -> impl QuadSource {
    let v: Vec<([StaticTerm; 3], Option<StaticTerm>)> = vec![
        ([*C1, rdf::type_.into(), rdfs::Class.into()], *DG),
        ([*C1, rdf::type_.into(), rdfs::Class.into()], *GN1),
        ([*C2, rdf::type_.into(), rdfs::Class.into()], *DG),
        ([*C2, rdfs::subClassOf.into(), *C1], *GN1),
        ([*C2, rdfs::subClassOf.into(), rdfs::Resource.into()], *GN1),
        //
        ([*P1, rdf::type_.into(), rdf::Property.into()], *DG),
        ([*P1, rdfs::domain.into(), *C1], *GN1),
        ([*P1, rdfs::range.into(), *C2], *GN1),
        //
        ([*P2, rdf::type_.into(), rdf::Property.into()], *DG),
        ([*P2, rdfs::domain.into(), *C2], *GN1),
        ([*P2, rdfs::range.into(), *C2], *GN1),
        //
        ([*I1A, rdf::type_.into(), *C1], *GN2),
        ([*I1B, rdf::type_.into(), *C1], *GN2),
        ([*I2A, rdf::type_.into(), *C2], *GN2),
        ([*I2B, rdf::type_.into(), *C2], *GN2),
        ([*I1A, *P1, *I2A], *GN2),
        ([*I1B, *P1, *I2B], *GN2),
        ([*I2A, *P2, *I2B], *GN2),
    ];
    v.into_iter().as_quad_source()
}

pub fn strict_node_types_quads() -> impl QuadSource {
    let v: Vec<([StaticTerm; 3], Option<StaticTerm>)> = vec![
        (
            [rdf::type_.into(), rdf::type_.into(), rdf::Property.into()],
            Some(rdf::type_.into()),
        ),
        ([*B1, rdf::type_.into(), *L1], Some(*B2)),
        ([*B2, rdf::type_.into(), *B1], None),
        ([*B2, rdf::type_.into(), *L2], None),
        ([*B2, rdf::type_.into(), *L2E], None),
    ];
    v.into_iter().as_quad_source()
}

pub fn generalized_node_types_quads() -> impl QuadSource {
    let v: Vec<([StaticTerm; 3], Option<StaticTerm>)> = vec![
        (
            [rdf::type_.into(), rdf::type_.into(), rdf::Property.into()],
            Some(rdf::type_.into()),
        ),
        ([*B1, *B2, *B1], Some(*B2)),
        ([*L2, *L1, *L1], Some(*L2)),
        ([*V1, *V2, *V3], Some(*V3)),
        ([*B2, *V1, *L2E], None),
    ];
    v.into_iter().as_quad_source()
}

pub fn as_box_q<Q: Quad, E>(quad: Result<Q, E>) -> ([BoxTerm; 3], Option<BoxTerm>)
where
    E: Debug,
{
    let quad = quad.unwrap();
    (
        [quad.s().copied(), quad.p().copied(), quad.o().copied()],
        quad.g().map(CopyTerm::copy),
    )
}

#[allow(dead_code)]
pub fn dump_dataset<D: Dataset>(d: &D)
where
    <<D::Quad as QuadStreamingMode>::UnsafeQuad as UnsafeQuad>::Term: Debug,
{
    println!("<<<<");
    for q in d.quads() {
        let q = q.unwrap();
        println!("{:?}\n{:?}\n{:?}\n{:?}\n\n", q.s(), q.p(), q.o(), q.g());
    }
    println!(">>>>");
}

/// Generate a test suite for an implementation of
/// [`Dataset`], [`CollectibleDataset`] and [`MutableDataset`].
///
/// If your type only implements [`Dataset`] and [`CollectibleDataset`],
/// you should use [`test_immutable_dataset_impl`] instead.
///
/// This macro is only available when the feature `test_macro` is enabled.
///
/// It accepts the following parameters:
/// * `module_name`: the name of the module to generate (defaults to `test`);
/// * `dataset_impl`: the type to test, implementing [`Dataset`], [`CollectibleDataset`] and [`MutableDataset`];
/// * `is_set`: a Boolean, indicating if `dataset_impl` implements [`SetDataset`]
///   (defaults to `true`);
/// * `is_gen`: a Boolean, indicating if `dataset_impl` supports the [generalized model]
///   (defaults to `true`).
/// * `dataset_collector`: a function used to create an empty instance of `dataset_impl`
///   (defaults to `dataset_impl::from_quad_source`);
/// * `mt` is used internally, do not touch it...
///
/// [`Dataset`]: dataset/trait.Dataset.html
/// [`CollectibleDataset`]: dataset/trait.CollectibleDataset.html
/// [`MutableDataset`]: dataset/trait.MutableDataset.html
/// [`test_immutable_dataset_impl`]: ./macro.test_immutable_dataset_impl.html
/// [`SetDataset`]: dataset/trait.SetDataset.html
/// [generalized model]: ./index.html
#[macro_export]
macro_rules! test_dataset_impl {
    ($dataset_impl: ident) => {
        $crate::test_dataset_impl!(test, $dataset_impl);
    };
    ($module_name: ident, $dataset_impl: ident) => {
        $crate::test_dataset_impl!($module_name, $dataset_impl, true);
    };
    ($module_name: ident, $dataset_impl: ident, $is_set: expr) => {
        $crate::test_dataset_impl!($module_name, $dataset_impl, $is_set, true);
    };
    ($module_name: ident, $dataset_impl: ident, $is_set: expr, $is_gen: expr) => {
        $crate::test_dataset_impl!($module_name, $dataset_impl, $is_set, $is_gen, $dataset_impl::from_quad_source);
    };
    ($module_name: ident, $dataset_impl: ident, $is_set: expr, $is_gen: expr, $dataset_collector: path) => {
        $crate::test_dataset_impl!($module_name, $dataset_impl, $is_set, $is_gen, $dataset_collector, {
            // these tests will only be performed for implementations of `MutableDataset`
            #[test]
            fn test_simple_mutations() -> MDResult<$dataset_impl, ()> {
                let mut d: $dataset_impl = $dataset_collector(no_quad()).unwrap();
                assert_eq!(d.quads().count(), 0);
                assert!(MutableDataset::insert(
                    &mut d,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class,
                    DG.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::insert(
                    &mut d,
                    &*C1,
                    &rdfs::subClassOf,
                    &*C2,
                    GN1.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 2);
                assert!(MutableDataset::remove(
                    &mut d,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class,
                    DG.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::remove(
                    &mut d,
                    &*C1,
                    &rdfs::subClassOf,
                    &*C2,
                    GN1.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 0);
                Ok(())
            }

            #[test]
            fn test_no_duplicate() -> MDResult<$dataset_impl, ()> {
                if $is_set {
                    let mut d: $dataset_impl = $dataset_collector(no_quad()).unwrap();
                    assert_eq!(d.quads().count(), 0);
                    assert!(MutableDataset::insert(
                        &mut d,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        DG.as_ref(),
                    )?);
                    assert_eq!(d.quads().count(), 1);
                    assert!(!MutableDataset::insert(
                        &mut d,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        DG.as_ref(),
                    )?);
                    assert_eq!(d.quads().count(), 1);
                    assert!(MutableDataset::remove(
                        &mut d,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        DG.as_ref(),
                    )?);
                    assert_eq!(d.quads().count(), 0);
                    assert!(!MutableDataset::remove(
                        &mut d,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        DG.as_ref(),
                    )?);
                    assert_eq!(d.quads().count(), 0);
                } else {
                    println!("effectively skipped, since is_set is false");
                }
                Ok(())
            }

            #[test]
            fn test_different_graphs_do_not_count_as_duplicate() -> MDResult<$dataset_impl, ()> {
                let mut d: $dataset_impl = $dataset_collector(no_quad()).unwrap();
                assert_eq!(d.quads().count(), 0);
                assert!(MutableDataset::insert(
                    &mut d,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class,
                    DG.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::insert(
                    &mut d,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class,
                    GN1.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 2);
                assert!(MutableDataset::remove(
                    &mut d,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class,
                    DG.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::remove(
                    &mut d,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class,
                    GN1.as_ref(),
                )?);
                assert_eq!(d.quads().count(), 0);
                Ok(())
            }

            #[test]
            fn test_x_all_mutations() {
                let mut d: $dataset_impl = $dataset_collector(no_quad()).unwrap();
                assert_eq!(d.quads().count(), 0);
                let inserted = d.insert_all(some_quads()).unwrap();
                if $is_set {
                    assert_eq!(inserted, 18, "returned by insert_all");
                }
                assert_eq!(d.quads().count(), 18, "after insert_all");
                if $is_set {
                    let inserted = d.insert_all(some_quads()).unwrap();
                    assert_eq!(inserted, 0, "returned by insert_all again");
                    assert_eq!(d.quads().count(), 18, "after insert_all again");
                }
                let removed = d.remove_all(some_quads()).unwrap();
                if $is_set {
                    assert_eq!(removed, 18, "returned by remove_all");
                }
                assert_eq!(d.quads().count(), 0, "after remove_all");
                if $is_set {
                    let removed = d.remove_all(some_quads()).unwrap();
                    assert_eq!(removed, 0, "returned by remove_all again");
                    assert_eq!(d.quads().count(), 0, "after remove_all again");
                }
            }

            #[test]
            fn test_remove_matching() -> MDResult<$dataset_impl, ()> {
                let mut d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let o_matcher = [&*C1, &*C2];
                d.remove_matching(&ANY, &rdf::type_, &o_matcher, &ANY)?;
                assert_consistent_hint(14, d.quads().size_hint());
                Ok(())
            }

            #[test]
            fn test_retain_matching() -> MDResult<$dataset_impl, ()> {
                let mut d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let o_matcher = [&*C1, &*C2];
                d.retain_matching(&ANY, &rdf::type_, &o_matcher, &ANY)?;
                print!("{:?}", d.quads().size_hint());
                assert_consistent_hint(4, d.quads().size_hint());
                Ok(())
            }
        });
    };
    ($module_name: ident, $dataset_impl: ident, $is_set: expr, $is_gen: expr, $dataset_collector: path, { $($mt:tt)* }) => {
        #[cfg(test)]
        mod $module_name {
            use $crate::dataset::test::*;
            use $crate::dataset::*;
            use $crate::graph::test::*;
            use $crate::ns::*;
            use $crate::term::TTerm;
            use $crate::term::matcher::ANY;

            #[allow(unused_imports)]
            use super::*;

            #[test]
            fn test_quads() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads();
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), d.quads().count());
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*P1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_s() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_s(&*C2);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &ANY, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 3);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C2,
                        &rdf::type_,
                        &rdfs::Class,
                        GN1.as_ref(),
                    )?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C2,
                        &rdf::type_,
                        &rdf::Property,
                        DG.as_ref(),
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_p() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_p(&rdfs::subClassOf);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &rdfs::subClassOf, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C2,
                        &rdfs::subClassOf,
                        &rdfs::Class,
                        DG.as_ref(),
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_o() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_o(&*I2B);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &*I2B, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*I1B, &*P1, &*I2B, GN2.as_ref())?);
                    assert!(!Dataset::contains(&v, &*I1B, &*P1, &*I2B, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*I2A, &*P1, &*I2B, GN2.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_g() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_g(GN1.as_ref());
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &ANY, &GN1.as_ref())] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 7);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_sp() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_sp(&*C2, &rdf::type_);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &rdf::type_, &ANY, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C2,
                        &rdf::type_,
                        &rdfs::Class,
                        GN1.as_ref(),
                    )?);
                    assert!(!Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_so() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_so(&*C2, &*C1);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &ANY, &*C1, &ANY)] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, DG.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_po() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_po(&rdf::type_, &rdfs::Class);
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&ANY, &rdf::type_, &rdfs::Class, &ANY),
                ] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 3);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        GN2.as_ref(),
                    )?);
                    assert!(!Dataset::contains(
                        &v,
                        &*P1,
                        &rdf::type_,
                        &rdf::Property,
                        DG.as_ref(),
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_sg() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_sg(&*C2, GN1.as_ref());
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &ANY, &ANY, &GN1.as_ref())] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, DG.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_pg() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_pg(&rdf::type_, GN1.as_ref());
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &rdf::type_, &ANY, &GN1.as_ref())] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, GN1.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_og() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_og(&*C1, GN1.as_ref());
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &*C1, &GN1.as_ref())] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, DG.as_ref())?);
                    assert!(!Dataset::contains(&v, &*I1A, &rdf::type_, &*C1, GN2.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_spo() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_spo(&*C1, &rdf::type_, &rdfs::Class);
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&*C1, &rdf::type_, &rdfs::Class, &ANY),
                ] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, GN1.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_spg() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_spg(&*C1, &rdf::type_, DG.as_ref());
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C1, &rdf::type_, &ANY, &DG.as_ref())] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        GN1.as_ref(),
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_sog() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_sog(&*C1, &rdfs::Class, DG.as_ref());
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C1, &ANY, &rdfs::Class, &DG.as_ref())] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        GN1.as_ref(),
                    )?);
                    assert!(!Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_pog() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_pog(&rdf::type_, &rdfs::Class, DG.as_ref());
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&ANY, &rdf::type_, &rdfs::Class,  &DG.as_ref()),
                ] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        GN1.as_ref(),
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_spog() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let quads = d.quads_with_spog(&*C1, &rdf::type_, &rdfs::Class, DG.as_ref());
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&*C1, &rdf::type_, &rdfs::Class, &DG.as_ref()),
                ] {
                    let v: Vec<_> = iter.map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &*C1, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                    assert!(!Dataset::contains(
                        &v,
                        &*C1,
                        &rdf::type_,
                        &rdfs::Class,
                        GN1.as_ref(),
                    )?);
                    assert!(!Dataset::contains(&v, &*C2, &rdf::type_, &rdfs::Class, DG.as_ref())?);
                }
                Ok(())
            }

            #[test]
            fn test_contains() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();
                assert!(Dataset::contains(&d, &*C2, &rdfs::subClassOf, &*C1, GN1.as_ref())?);
                assert!(!Dataset::contains(&d, &*C1, &rdfs::subClassOf, &*C2, GN1.as_ref())?);
                Ok(())
            }

            #[test]
            fn test_quads_matching() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let p_matcher = [&rdf::type_, &rdfs::domain];
                let o_matcher = [&*C1, &*C2];
                let g_matcher = [|g: Option<&dyn TTerm>| g.is_some()];
                let v: Vec<_> = d
                    .quads_matching(&ANY, &p_matcher, &o_matcher, &g_matcher)
                    .map(as_box_q)
                    .collect();
                assert_eq!(v.len(), 6);
                assert!(Dataset::contains(&v, &*P1, &rdfs::domain, &*C1, GN1.as_ref())?);
                assert!(Dataset::contains(&v, &*P2, &rdfs::domain, &*C2, GN1.as_ref())?);
                assert!(Dataset::contains(&v, &*I1A, &rdf::type_, &*C1, GN2.as_ref())?);
                assert!(Dataset::contains(&v, &*I2A, &rdf::type_, &*C2, GN2.as_ref())?);
                assert!(!Dataset::contains(&v, &*C2, &rdfs::subClassOf, &*C1, GN1.as_ref())?);
                assert!(!Dataset::contains(
                    &v,
                    &*C1,
                    &rdf::type_,
                    &rdfs::Class,
                    GN1.as_ref()
                )?);
                Ok(())
            }

            #[test]
            fn test_subjects() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let subjects = d.subjects().unwrap();
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
            fn test_predicates() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let predicates = d.predicates().unwrap();
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
            fn test_objects() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let objects = d.objects().unwrap();
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
            fn test_graph_names() -> MDResult<$dataset_impl, ()> {
                let d: $dataset_impl = $dataset_collector(some_quads()).unwrap();

                let graph_names = d.graph_names().unwrap();
                assert_eq!(graph_names.len(), 2);
                assert_contains(&graph_names, &*G1);
                assert_contains(&graph_names, &*G2);
                Ok(())
            }

            #[test]
            fn test_iris() -> MDResult<$dataset_impl, ()> {
                let d = if $is_gen {
                    $dataset_collector(generalized_node_types_quads()).unwrap()
                } else {
                    $dataset_collector(strict_node_types_quads()).unwrap()
                };

                let iris = d.iris().unwrap();
                assert_eq!(iris.len(), 2);
                assert_contains(&iris, &rdf::Property);
                assert_contains(&iris, &rdf::type_);
                Ok(())
            }

            #[test]
            fn test_bnodes() -> MDResult<$dataset_impl, ()> {
                let d = if $is_gen {
                    $dataset_collector(generalized_node_types_quads()).unwrap()
                } else {
                    $dataset_collector(strict_node_types_quads()).unwrap()
                };

                let bnodes = d.bnodes().unwrap();
                assert_eq!(bnodes.len(), 2);
                assert_contains(&bnodes, &*B1);
                assert_contains(&bnodes, &*B2);
                Ok(())
            }

            #[test]
            fn test_literals() -> MDResult<$dataset_impl, ()> {
                let d = if $is_gen {
                    $dataset_collector(generalized_node_types_quads()).unwrap()
                } else {
                    $dataset_collector(strict_node_types_quads()).unwrap()
                };

                let literals = d.literals().unwrap();
                assert_eq!(literals.len(), 3);
                assert_contains(&literals, &*L1);
                assert_contains(&literals, &*L2);
                assert_contains(&literals, &*L2E);
                Ok(())
            }

            #[test]
            fn test_variables() -> MDResult<$dataset_impl, ()> {
                if $is_gen {
                    let d: $dataset_impl = $dataset_collector(generalized_node_types_quads()).unwrap();

                    let variables = d.variables().unwrap();
                    assert_eq!(variables.len(), 3);
                    assert_contains(&variables, &*V1);
                    assert_contains(&variables, &*V2);
                    assert_contains(&variables, &*V3);
                } else {
                    let d: $dataset_impl = $dataset_collector(strict_node_types_quads()).unwrap();

                    let variables = d.variables().unwrap();
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
/// [`Dataset`] and [`CollectibleDataset`].
///
/// If your type also implements [`MutableDataset`],
/// you should use [`test_dataset_impl`] instead.
///
/// This macro is only available when the feature `test_macro` is enabled.
///
/// It accepts the following parameters:
/// * `module_name`: the name of the module to generate (defaults to `test`);
/// * `dataset_impl`: the type to test, implementing [`Dataset`] and [`CollectibleDataset`];
/// * `is_set`: a Boolean, indicating if `dataset_impl` implements [`SetDataset`]
///   (defaults to `true`);
/// * `is_gen`: a Boolean, indicating if `dataset_impl` supports the [generalized model]
///   (defaults to `true`);
/// * `dataset_collector`: a function used to collect quads into an instance of `dataset_impl`
///   (defaults to `dataset_impl::from_quad_source`);
///
/// [`Dataset`]: dataset/trait.Dataset.html
/// [`CollectibleDataset`]: dataset/trait.CollectibleDataset.html
/// [`MutableDataset`]: dataset/trait.MutableDataset.html
/// [`test_dataset_impl`]: ./macro.test_dataset_impl.html
/// [`SetDataset`]: dataset/trait.SetDataset.html
/// [generalized model]: ./index.html
#[macro_export]
macro_rules! test_immutable_dataset_impl {
    ($dataset_impl: ident) => {
        $crate::test_immutable_dataset_impl!(test, $dataset_impl);
    };
    ($module_name: ident, $dataset_impl: ident) => {
        $crate::test_immutable_dataset_impl!($module_name, $dataset_impl, true);
    };
    ($module_name: ident, $dataset_impl: ident, $is_set: expr) => {
        $crate::test_immutable_dataset_impl!($module_name, $dataset_impl, $is_set, true);
    };
    ($module_name: ident, $dataset_impl: ident, $is_set: expr, $is_gen: expr) => {
        $crate::test_immutable_dataset_impl!(
            $module_name,
            $dataset_impl,
            $is_set,
            $is_gen,
            $dataset_impl::from_quad_source
        );
    };
    ($module_name: ident, $dataset_impl: ident, $is_set: expr, $is_gen: expr, $dataset_collector: path) => {
        // calling test_dataset_impl, but passing an empty block as mt (the mutability tests)
        $crate::test_dataset_impl!(
            $module_name,
            $dataset_impl,
            $is_set,
            $is_gen,
            $dataset_collector,
            {}
        );
    };
}
