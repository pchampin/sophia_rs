//! Contains helper functions and marocs for testing Dataset implementations

use std::fmt::Debug;

use crate::dataset::*;
use crate::ns::*;
use crate::quad::stream::*;
use crate::quad::*;
use crate::term::*;

pub const NS: &str = "http://example.org/";

lazy_static! {
    pub static ref G1: StaticTerm = StaticTerm::new_iri2(NS, "G1").unwrap();
    pub static ref G2: StaticTerm = StaticTerm::new_iri2(NS, "G2").unwrap();
    pub static ref C1: StaticTerm = StaticTerm::new_iri2(NS, "C1").unwrap();
    pub static ref C2: StaticTerm = StaticTerm::new_iri2(NS, "C2").unwrap();
    pub static ref P1: StaticTerm = StaticTerm::new_iri2(NS, "p1").unwrap();
    pub static ref P2: StaticTerm = StaticTerm::new_iri2(NS, "p2").unwrap();
    pub static ref I1A: StaticTerm = StaticTerm::new_iri2(NS, "I1A").unwrap();
    pub static ref I1B: StaticTerm = StaticTerm::new_iri2(NS, "I1B").unwrap();
    pub static ref I2A: StaticTerm = StaticTerm::new_iri2(NS, "I2A").unwrap();
    pub static ref I2B: StaticTerm = StaticTerm::new_iri2(NS, "I2B").unwrap();
    pub static ref DG: GraphName<&'static str> = None;
    pub static ref GN1: GraphName<&'static str> = Some(G1.clone());
    pub static ref GN2: GraphName<&'static str> = Some(G2.clone());
}

pub fn populate<D: MutableDataset>(d: &mut D) -> MDResult<D, ()> {
    d.insert(&C1, &rdf::type_, &rdfs::Class, &DG)?;
    d.insert(&C1, &rdf::type_, &rdfs::Class, &GN1)?;

    d.insert(&C2, &rdf::type_, &rdfs::Class, &DG)?;
    d.insert(&C2, &rdfs::subClassOf, &C1, &GN1)?;

    d.insert(&P1, &rdf::type_, &rdf::Property, &DG)?;
    d.insert(&P1, &rdfs::domain, &C1, &GN1)?;
    d.insert(&P1, &rdfs::range, &C2, &GN1)?;

    d.insert(&P2, &rdf::type_, &rdf::Property, &DG)?;
    d.insert(&P2, &rdfs::domain, &C2, &GN1)?;
    d.insert(&P2, &rdfs::range, &C2, &GN1)?;

    d.insert(&I1A, &rdf::type_, &C1, &GN2)?;
    d.insert(&I1B, &rdf::type_, &C1, &GN2)?;
    d.insert(&I2A, &rdf::type_, &C2, &GN2)?;
    d.insert(&I2B, &rdf::type_, &C2, &GN2)?;
    d.insert(&I1A, &P1, &I2A, &GN2)?;
    d.insert(&I1B, &P1, &I2B, &GN2)?;
    d.insert(&I2A, &P2, &I2B, &GN2)?;

    assert_consistent_hint(17, d.quads().size_hint());
    Ok(())
}

pub fn populate_nodes_types<D: MutableDataset>(d: &mut D) -> MDResult<D, ()> {
    let gn = Some(StaticTerm::from(&rdf::type_));
    d.insert(&rdf::type_, &rdf::type_, &rdf::Property, &gn)?;
    let gn = Some(StaticTerm::new_bnode("b2").unwrap());
    d.insert(
        &StaticTerm::new_bnode("b1").unwrap(),
        &StaticTerm::new_bnode("b2").unwrap(),
        &StaticTerm::new_bnode("b1").unwrap(),
        &gn,
    )?;
    let gn = Some(StaticTerm::from("lit2"));
    d.insert(
        &StaticTerm::from("lit2"),
        &StaticTerm::from("lit1"),
        &StaticTerm::from("lit1"),
        &gn,
    )?;
    let gn = Some(StaticTerm::new_variable("v3").unwrap());
    d.insert(
        &StaticTerm::new_variable("v1").unwrap(),
        &StaticTerm::new_variable("v2").unwrap(),
        &StaticTerm::new_variable("v3").unwrap(),
        &gn,
    )?;
    d.insert(
        &StaticTerm::new_bnode("b2").unwrap(),
        &StaticTerm::new_variable("v1").unwrap(),
        &StaticTerm::new_literal_lang("lit2", "en").unwrap(),
        &DG,
    )?;

    assert_consistent_hint(5, d.quads().size_hint());
    Ok(())
}

pub fn as_box_q<'a, Q: Quad<'a> + 'a>(quad: Q) -> ([BoxTerm; 3], GraphName<Box<str>>) {
    (
        [quad.s().into(), quad.p().into(), quad.o().into()],
        quad.g().convert_graph_name(),
    )
}

#[allow(dead_code)]
pub fn dump_graph<'a, D: Dataset<'a>>(d: &'a D)
where
    <D::Quad as Quad<'a>>::TermData: Debug,
{
    println!("<<<<");
    for q in d.quads() {
        let q = q.unwrap();
        println!("{:?}\n{:?}\n{:?}\n{:?}\n\n", q.s(), q.p(), q.o(), q.g());
    }
    println!(">>>>");
}

pub fn assert_consistent_hint(val: usize, hint: (usize, Option<usize>)) {
    assert!(hint.0 <= val);
    assert!(val <= hint.1.or(Some(val)).unwrap())
}

pub fn make_quad_source() -> impl QuadSource<'static> {
    vec![
        [&*C1, &rdf::type_, &rdfs::Class, &G1],
        [&*C1, &rdfs::subClassOf, &*C2, &G1],
    ]
    .into_iter()
    .as_quad_source()
}

/// Generates a test suite for [`Dataset`] and [`MutableDataset`] implementations.
///
/// [`Dataset`]: dataset/trait.Dataset.html
/// [`MutableDataset`]: dataset/trait.MutableDataset.html
#[macro_export]
macro_rules! test_dataset_impl {
    ($mutable_dataset_impl:ident) => {
        test_dataset_impl!(test, $mutable_dataset_impl);
    };
    ($module_name: ident, $mutable_dataset_impl: ident) => {
        test_dataset_impl!($module_name, $mutable_dataset_impl, true);
    };
    ($module_name: ident, $mutable_dataset_impl: ident, $is_set: expr) => {
        test_dataset_impl!(
            $module_name,
            $mutable_dataset_impl,
            $is_set,
            $mutable_dataset_impl::new
        );
    };
    ($module_name: ident, $mutable_dataset_impl: ident, $is_set: expr, $mutable_dataset_factory: path) => {
        #[cfg(test)]
        mod $module_name {
            use resiter::oks::*;
            use $crate::dataset::test::*;
            use $crate::dataset::*;
            use $crate::ns::*;
            use $crate::quad::stream::*;
            use $crate::term::{GraphName, matcher::ANY, *};

            #[allow(unused_imports)]
            use super::*;

            // test MutableDataset + SetGraph

            #[test]
            fn test_simple_mutations() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                assert_eq!(d.quads().count(), 0);
                assert!(MutableDataset::insert(
                    &mut d,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class,
                    &DG
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::insert(
                    &mut d,
                    &C1,
                    &rdfs::subClassOf,
                    &C2,
                    &GN1
                )?);
                assert_eq!(d.quads().count(), 2);
                assert!(MutableDataset::remove(
                    &mut d,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class,
                    &DG
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::remove(
                    &mut d,
                    &C1,
                    &rdfs::subClassOf,
                    &C2,
                    &GN1
                )?);
                assert_eq!(d.quads().count(), 0);
                Ok(())
            }

            #[test]
            fn test_no_duplicate() -> MDResult<$mutable_dataset_impl, ()> {
                if $is_set {
                    let mut d = $mutable_dataset_factory();
                    assert_eq!(d.quads().count(), 0);
                    assert!(MutableDataset::insert(
                        &mut d,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &DG
                    )?);
                    assert_eq!(d.quads().count(), 1);
                    assert!(!MutableDataset::insert(
                        &mut d,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &DG
                    )?);
                    assert_eq!(d.quads().count(), 1);
                    assert!(MutableDataset::remove(
                        &mut d,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &DG
                    )?);
                    assert_eq!(d.quads().count(), 0);
                    assert!(!MutableDataset::remove(
                        &mut d,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &DG
                    )?);
                    assert_eq!(d.quads().count(), 0);
                } else {
                    println!("effectively skipped, since is_set is false");
                }
                Ok(())
            }

            #[test]
            fn test_different_graphs_do_not_count_as_duplicate(
            ) -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                assert_eq!(d.quads().count(), 0);
                assert!(MutableDataset::insert(
                    &mut d,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class,
                    &DG
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::insert(
                    &mut d,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class,
                    &GN1
                )?);
                assert_eq!(d.quads().count(), 2);
                assert!(MutableDataset::remove(
                    &mut d,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class,
                    &DG
                )?);
                assert_eq!(d.quads().count(), 1);
                assert!(MutableDataset::remove(
                    &mut d,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class,
                    &GN1
                )?);
                assert_eq!(d.quads().count(), 0);
                Ok(())
            }

            #[test]
            fn test_sink_mutations() {
                let mut d = $mutable_dataset_factory();
                assert_eq!(d.quads().count(), 0);
                assert_eq!(make_quad_source().in_sink(&mut d.inserter()).unwrap(), 2);
                assert_eq!(d.quads().count(), 2);
                if $is_set {
                    assert_eq!(make_quad_source().in_sink(&mut d.inserter()).unwrap(), 0);
                    assert_eq!(d.quads().count(), 2);
                }
                assert_eq!(make_quad_source().in_sink(&mut d.remover()).unwrap(), 2);
                assert_eq!(d.quads().count(), 0);
                assert_eq!(make_quad_source().in_sink(&mut d.remover()).unwrap(), 0);
                assert_eq!(d.quads().count(), 0);
            }

            #[test]
            fn test_x_all_mutations() {
                let mut d = $mutable_dataset_factory();
                assert_eq!(d.quads().count(), 0);
                assert_eq!(d.insert_all(&mut make_quad_source()).unwrap(), 2);
                assert_eq!(d.quads().count(), 2);
                if $is_set {
                    assert_eq!(d.insert_all(&mut make_quad_source()).unwrap(), 0);
                    assert_eq!(d.quads().count(), 2);
                }
                assert_eq!(d.remove_all(&mut make_quad_source()).unwrap(), 2);
                assert_eq!(d.quads().count(), 0);
                assert_eq!(d.remove_all(&mut make_quad_source()).unwrap(), 0);
                assert_eq!(d.quads().count(), 0);
            }

            #[test]
            fn test_remove_matching() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let o_matcher = [C1.clone(), C2.clone()];
                d.remove_matching(&ANY, &rdf::type_, &o_matcher[..], &ANY)?;
                assert_consistent_hint(13, d.quads().size_hint());
                Ok(())
            }

            #[test]
            fn test_retain() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let o_matcher = [C1.clone(), C2.clone()];
                MutableDataset::retain(&mut d, &ANY, &rdf::type_, &o_matcher[..], &ANY)?;
                print!("{:?}", d.quads().size_hint());
                assert_consistent_hint(4, d.quads().size_hint());
                Ok(())
            }

            // Test Dataset

            #[test]
            fn test_quads() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads();
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &ANY, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), d.quads().count());
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &GN1)?);
                    assert!(!Dataset::contains(&v, &P1, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_s() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_s(&C2);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &ANY, &ANY, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C2,
                        &rdf::type_,
                        &rdfs::Class,
                        &GN1
                    )?);
                    assert!(!Dataset::contains(
                        &v,
                        &C2,
                        &rdf::type_,
                        &rdf::Property,
                        &DG
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_p() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_p(&rdfs::subClassOf);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &rdfs::subClassOf, &ANY, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &GN1)?);
                    assert!(!Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C2,
                        &rdfs::subClassOf,
                        &rdfs::Class,
                        &DG
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_o() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_o(&I2B);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &*I2B, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &I1B, &P1, &I2B, &GN2)?);
                    assert!(!Dataset::contains(&v, &I1B, &P1, &I2B, &GN1)?);
                    assert!(!Dataset::contains(&v, &I2A, &P1, &I2B, &GN2)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_g() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_g(&GN1);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &ANY, &*GN1)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 6);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &GN1)?);
                    assert!(!Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_sp() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_sp(&C2, &rdf::type_);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &rdf::type_, &ANY, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C2,
                        &rdf::type_,
                        &rdfs::Class,
                        &GN1
                    )?);
                    assert!(!Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_so() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_so(&C2, &C1);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &ANY, &*C1, &ANY)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &GN1)?);
                    assert!(!Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &DG)?);
                    assert!(!Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_po() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_po(&rdf::type_, &rdfs::Class);
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&ANY, &rdf::type_, &rdfs::Class, &ANY),
                ] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 3);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &GN2
                    )?);
                    assert!(!Dataset::contains(
                        &v,
                        &P1,
                        &rdf::type_,
                        &rdf::Property,
                        &DG
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_sg() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_sg(&C2, &GN1);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C2, &ANY, &ANY, &*GN1)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &GN1)?);
                    assert!(!Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &DG)?);
                    assert!(!Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_pg() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_pg(&rdf::type_, &GN1);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &rdf::type_, &ANY, &*GN1)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &GN1)?);
                    assert!(!Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &GN1)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_og() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_og(&C1, &GN1);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&ANY, &ANY, &*C1, &*GN1)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &GN1)?);
                    assert!(!Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &DG)?);
                    assert!(!Dataset::contains(&v, &I1A, &rdf::type_, &C1, &GN2)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_spo() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_spo(&C1, &rdf::type_, &rdfs::Class);
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&*C1, &rdf::type_, &rdfs::Class, &ANY),
                ] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &GN1)?);
                    assert!(!Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_spg() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_spg(&C1, &rdf::type_, &DG);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C1, &rdf::type_, &ANY, &*DG)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &GN1
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_sog() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_sog(&C1, &rdfs::Class, &DG);
                let hint = quads.size_hint();
                for iter in vec![quads, d.quads_matching(&*C1, &ANY, &rdfs::Class, &*DG)] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &GN1
                    )?);
                    assert!(!Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_pog() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_pog(&rdf::type_, &rdfs::Class, &DG);
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&ANY, &rdf::type_, &rdfs::Class, &*DG),
                ] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 2);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &GN1
                    )?);
                }
                Ok(())
            }

            #[test]
            fn test_quads_with_spog() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let quads = d.quads_with_spog(&C1, &rdf::type_, &rdfs::Class, &DG);
                let hint = quads.size_hint();
                for iter in vec![
                    quads,
                    d.quads_matching(&*C1, &rdf::type_, &rdfs::Class, &*DG),
                ] {
                    let v: Vec<_> = iter.oks().map(as_box_q).collect();
                    assert_eq!(v.len(), 1);
                    assert_consistent_hint(v.len(), hint);
                    assert!(Dataset::contains(&v, &C1, &rdf::type_, &rdfs::Class, &DG)?);
                    assert!(!Dataset::contains(
                        &v,
                        &C1,
                        &rdf::type_,
                        &rdfs::Class,
                        &GN1
                    )?);
                    assert!(!Dataset::contains(&v, &C2, &rdf::type_, &rdfs::Class, &DG)?);
                }
                Ok(())
            }

            #[test]
            fn test_contains() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;
                assert!(Dataset::contains(&d, &C2, &rdfs::subClassOf, &C1, &GN1)?);
                assert!(!Dataset::contains(&d, &C1, &rdfs::subClassOf, &C2, &GN1)?);
                Ok(())
            }

            #[test]
            fn test_quads_matching() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let p_matcher: [StaticTerm; 2] = [rdf::type_.clone(), rdfs::domain.clone()];
                let o_matcher: [StaticTerm; 2] = [C1.clone(), C2.clone()];
                let g_matcher = |g: &GraphName<&str>| g.is_some();
                let v: Vec<_> = d
                    .quads_matching(&ANY, &p_matcher[..], &o_matcher[..], &g_matcher)
                    .oks()
                    .map(as_box_q)
                    .collect();
                assert_eq!(v.len(), 6);
                assert!(Dataset::contains(&v, &P1, &rdfs::domain, &C1, &GN1)?);
                assert!(Dataset::contains(&v, &P2, &rdfs::domain, &C2, &GN1)?);
                assert!(Dataset::contains(&v, &I1A, &rdf::type_, &C1, &GN2)?);
                assert!(Dataset::contains(&v, &I2A, &rdf::type_, &C2, &GN2)?);
                assert!(!Dataset::contains(&v, &C2, &rdfs::subClassOf, &C1, &GN1)?);
                assert!(!Dataset::contains(
                    &v,
                    &C1,
                    &rdf::type_,
                    &rdfs::Class,
                    &GN1
                )?);
                Ok(())
            }

            #[test]
            fn test_subjects() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let subjects = d.subjects().unwrap();
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
            fn test_predicates() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let predicates = d.predicates().unwrap();
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
            fn test_objects() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let objects = d.objects().unwrap();
                assert_eq!(objects.len(), 6);

                let robjects: std::collections::HashSet<_> =
                    objects.iter().map(|t| RefTerm::from(t)).collect();
                assert!(robjects.contains(&rdf::Property));
                assert!(robjects.contains(&rdfs::Class));
                assert!(robjects.contains(&C1));
                assert!(robjects.contains(&C2));
                assert!(robjects.contains(&I2A));
                assert!(robjects.contains(&I2B));
                Ok(())
            }

            #[test]
            fn test_graph_names() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate(&mut d)?;

                let graph_names = d.graph_names().unwrap();
                assert_eq!(graph_names.len(), 2);

                let rgraph_names: std::collections::HashSet<_> =
                    graph_names.iter().map(|t| RefTerm::from(t)).collect();
                assert!(rgraph_names.contains(&G1));
                assert!(rgraph_names.contains(&G2));
                Ok(())
            }

            #[test]
            fn test_iris() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate_nodes_types(&mut d)?;

                let iris = d.iris().unwrap();
                assert_eq!(iris.len(), 2);

                let riris: std::collections::HashSet<_> =
                    iris.iter().map(|t| RefTerm::from(t)).collect();
                assert!(riris.contains(&rdf::Property));
                assert!(riris.contains(&rdf::type_));
                Ok(())
            }

            #[test]
            fn test_bnodes() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate_nodes_types(&mut d)?;

                let bnodes = d.bnodes().unwrap();
                assert_eq!(bnodes.len(), 2);

                let rbnodes: std::collections::HashSet<_> =
                    bnodes.iter().map(|t| t.value()).collect();
                assert!(rbnodes.contains("b1"));
                assert!(rbnodes.contains("b2"));
                Ok(())
            }

            #[test]
            fn test_literals() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate_nodes_types(&mut d)?;

                let literals = d.literals().unwrap();
                assert_eq!(literals.len(), 3);

                let rliterals: std::collections::HashSet<_> =
                    literals.iter().map(|t| RefTerm::from(t)).collect();
                assert!(rliterals.contains(&StaticTerm::from("lit1")));
                assert!(rliterals.contains(&StaticTerm::from("lit2")));
                assert!(rliterals.contains(&StaticTerm::new_literal_lang("lit2", "en").unwrap()));
                Ok(())
            }

            #[test]
            fn test_variables() -> MDResult<$mutable_dataset_impl, ()> {
                let mut d = $mutable_dataset_factory();
                populate_nodes_types(&mut d)?;

                let variables = d.variables().unwrap();
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
