#[macro_export]
macro_rules! test_graph_impl {
    ($mutable_graph_impl:ident) => {
        test_graph_impl!(test, $mutable_graph_impl);
    };
    ($module_name: ident, $mutable_graph_impl: ident) => {
        #[cfg(test)]
        mod $module_name {
            use crate::graph::*;
            use crate::ns::*;
            use crate::streams::*;
            use crate::term::*;
            use crate::term::matcher::ANY;
            use crate::triple::*;
            use std::fmt::Debug;
            use resiter::oks::*;

            #[allow(unused_imports)]
            use super::*;

            const NS: &str = "http://example.org/";

            lazy_static!{
                static ref C1: StaticTerm = StaticTerm::new_iri2(NS, "C1").unwrap();
                static ref C2: StaticTerm = StaticTerm::new_iri2(NS, "C2").unwrap();
                static ref P1: StaticTerm = StaticTerm::new_iri2(NS, "p1").unwrap();
                static ref P2: StaticTerm = StaticTerm::new_iri2(NS, "p2").unwrap();
                static ref I1A: StaticTerm = StaticTerm::new_iri2(NS, "I1A").unwrap();
                static ref I1B: StaticTerm = StaticTerm::new_iri2(NS, "I1B").unwrap();
                static ref I2A: StaticTerm = StaticTerm::new_iri2(NS, "I2A").unwrap();
                static ref I2B: StaticTerm = StaticTerm::new_iri2(NS, "I2B").unwrap();
            }

            // test MutableGraph + SetGraph

            #[test]
            fn test_simple_mutations() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.triples().count(), 0);
                assert!   (MutableGraph::insert(&mut g, &C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.triples().count(), 1);
                assert!   (MutableGraph::insert(&mut g, &C1, &rdfs::subClassOf, &C2)?);
                assert_eq!(g.triples().count(), 2);
                assert!   (MutableGraph::remove(&mut g, &C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.triples().count(), 1);
                assert!   (MutableGraph::remove(&mut g, &C1, &rdfs::subClassOf, &C2)?);
                assert_eq!(g.triples().count(), 0);
                Ok(())
            }

            #[test]
            fn test_no_duplicate() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.triples().count(), 0);
                assert!   (MutableGraph::insert(&mut g, &C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.triples().count(), 1);
                assert!  (!MutableGraph::insert(&mut g, &C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.triples().count(), 1);
                assert!   (MutableGraph::remove(&mut g, &C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.triples().count(), 0);
                assert!  (!MutableGraph::remove(&mut g, &C1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_sink_mutations() {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.triples().count(), 0);
                assert_eq!(make_triple_source().in_sink(&mut g.inserter()).unwrap(), 2);
                assert_eq!(g.triples().count(), 2);
                assert_eq!(make_triple_source().in_sink(&mut g.inserter()).unwrap(), 0);
                assert_eq!(g.triples().count(), 2);
                assert_eq!(make_triple_source().in_sink(&mut g.remover()).unwrap(), 2);
                assert_eq!(g.triples().count(), 0);
                assert_eq!(make_triple_source().in_sink(&mut g.remover()).unwrap(), 0);
                assert_eq!(g.triples().count(), 0);
            }

            #[test]
            fn test_x_all_mutations() {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.triples().count(), 0);
                assert_eq!(g.insert_all(&mut make_triple_source()).unwrap(), 2);
                assert_eq!(g.triples().count(), 2);
                assert_eq!(g.insert_all(&mut make_triple_source()).unwrap(), 0);
                assert_eq!(g.triples().count(), 2);
                assert_eq!(g.remove_all(&mut make_triple_source()).unwrap(), 2);
                assert_eq!(g.triples().count(), 0);
                assert_eq!(g.remove_all(&mut make_triple_source()).unwrap(), 0);
                assert_eq!(g.triples().count(), 0);
            }

            #[test]
            fn test_remove_matching() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let o_matcher = [C1.clone(), C2.clone()];
                g.remove_matching(&ANY, &rdf::type_, &o_matcher[..])?;
                assert_consistent_hint(12, g.triples().size_hint());
                Ok(())
            }

            #[test]
            fn test_retain() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let o_matcher = [C1.clone(), C2.clone()];
                MutableGraph::retain(&mut g, &ANY, &rdf::type_, &o_matcher[..])?;
                print!("{:?}", g.triples().size_hint());
                assert_consistent_hint(4, g.triples().size_hint());
                Ok(())
            }

            // Test Graph

            #[test]
            fn test_iter_and_hint() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples().oks().map(as_box_t).collect();
                assert_eq!(v.len(), g.triples().count());
                assert_consistent_hint(v.len(), g.triples().size_hint());
                assert!(Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &P1, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.triples_matching(&ANY, &ANY, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), g.triples().count());
                assert_consistent_hint(v.len(), g.triples().size_hint());
                assert!(Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &P1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_s() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples_with_s(&C2).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.triples_with_s(&C2).size_hint());
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.triples_matching(&*C2, &ANY, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.triples_with_s(&C2).size_hint());
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_p() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples_with_p(&rdfs::subClassOf).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.triples_with_p(&rdfs::subClassOf).size_hint());
                assert!(Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);
                assert!(!Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.triples_matching(&ANY, &rdfs::subClassOf, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.triples_with_p(&rdfs::subClassOf).size_hint());
                assert!(Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);
                assert!(!Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_o() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples_with_o(&I2B).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.triples_with_o(&I2B).size_hint());
                assert!(Graph::contains(&v, &I1B, &P1, &I2B)?);
                assert!(!Graph::contains(&v, &I2B, &rdf::type_, &C2)?);

                let v: Vec<_> = g.triples_matching(&ANY, &ANY, &*I2B).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.triples_with_o(&I2B).size_hint());
                assert!(Graph::contains(&v, &I1B, &P1, &I2B)?);
                assert!(!Graph::contains(&v, &I2B, &rdf::type_, &C2)?);
                Ok(())
            }

            #[test]
            fn test_x_for_sp() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples_with_sp(&C2, &rdf::type_).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.triples_with_sp(&C2, &rdf::type_).size_hint());
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);

                let v: Vec<_> = g.triples_matching(&*C2, &rdf::type_, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.triples_with_sp(&C2, &rdf::type_).size_hint());
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);
                Ok(())
            }

            #[test]
            fn test_x_for_so() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples_with_so(&C2, &C1).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.triples_with_so(&C2, &C1).size_hint());
                assert!(Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);
                assert!(!Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.triples_matching(&*C2, &ANY, &*C1).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.triples_with_so(&C2, &C1).size_hint());
                assert!(Graph::contains(&v, &C2, &rdfs::subClassOf, &C1)?);
                assert!(!Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_po() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples_with_po(&rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.triples_with_po(&rdf::type_, &rdfs::Class).size_hint());
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &P2, &rdf::type_, &rdf::Property)?);

                let v: Vec<_> = g.triples_matching(&ANY, &rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.triples_with_po(&rdf::type_, &rdfs::Class).size_hint());
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &P2, &rdf::type_, &rdf::Property)?);
                Ok(())
            }

            #[test]
            fn test_x_for_spo() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.triples_with_spo(&C2, &rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.triples_matching(&*C2, &rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert!(Graph::contains(&v, &C2, &rdf::type_, &rdfs::Class)?);
                assert!(!Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_contains() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;
                assert!(Graph::contains(&g, &C2, &rdfs::subClassOf, &C1)?);
                assert!(!Graph::contains(&g, &C1, &rdfs::subClassOf, &C2)?);
                Ok(())
            }

            #[test]
            fn test_triples_matching() -> MGResult<$mutable_graph_impl, ()>
            {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let p_matcher: [StaticTerm;2] = [rdf::type_.clone(), rdfs::domain.clone()];
                let o_matcher: [StaticTerm;2] = [C2.clone(), rdfs::Class.clone()];
                let v: Vec<_> = g.triples_matching(&ANY, &p_matcher[..], &o_matcher[..]).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 5);
                assert!(Graph::contains(&v, &C1, &rdf::type_, &rdfs::Class)?);
                assert!(Graph::contains(&v, &P2, &rdfs::domain, &C2)?);
                assert!(Graph::contains(&v, &I2A, &rdf::type_, &C2)?);
                assert!(!Graph::contains(&v, &P1, &rdfs::domain, &C1)?);
                assert!(!Graph::contains(&v, &I1A, &rdf::type_, &C1)?);
                Ok(())
            }

            // helper functions

            fn populate<G: MutableGraph> (g: &mut G) -> MGResult<G, ()>
            {
                g.insert(&C1, &rdf::type_, &rdfs::Class)?;

                g.insert(&C2, &rdf::type_, &rdfs::Class)?;
                g.insert(&C2, &rdfs::subClassOf, &C1)?;

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

                assert_consistent_hint(16, g.triples().size_hint());
                Ok(())
            }

            fn as_box_t<'a, T: Triple<'a>+'a> (triple: T) -> [BoxTerm;3] {
                [triple.s().into(), triple.p().into(), triple.o().into()]
            }

            #[allow(dead_code)]
            fn dump_graph<'a, G: Graph<'a>> (g: &'a G) where
                <G::Triple as Triple<'a>>::Holder: Debug,
            {
                println!("<<<<");
                for t in g.triples() {
                    let t = t.unwrap();
                    println!("{:?}\n{:?}\n{:?}\n\n", t.s(), t.p(), t.o());
                }
                println!(">>>>");
            }

            fn assert_consistent_hint(val: usize, hint: (usize, Option<usize>)) {
                assert!(hint.0 <= val);
                assert!(val <= hint.1.or(Some(val)).unwrap())
            }

            fn make_triple_source() -> impl TripleSource<'static> {
                vec![
                    [&*C1, &rdf::type_, &rdfs::Class],
                    [&*C1, &rdfs::subClassOf, &*C2],
                ].into_iter()
                .as_triple_source()
            }


        }
    }
}
