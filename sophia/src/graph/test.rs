#[macro_export]
macro_rules! test_graph_impl {
    ($mutable_graph_impl:ident) => {
        test_graph_impl!(test, $mutable_graph_impl);
    };
    ($module_name: ident, $mutable_graph_impl: ident) => {
        #[cfg(test)]
        mod $module_name {
            use ::graph::*;
            use ::ns::*;
            use ::streams::*;
            use ::term::*;
            use ::term::matcher::ANY;
            use ::triple::*;
            use std::fmt::Debug;
            use resiter::oks::*;

            #[allow(unused_imports)]
            use super::*;

            const NS: &str = "http://example.org/";

            type TestResult = Result<(), <$mutable_graph_impl as MutableGraph>::MutationError>;

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
            fn test_simple_mutations() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.iter().count(), 0);
                assert!   (g.insert(&C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.iter().count(), 1);
                assert!   (g.insert(&C1, &rdfs::subClassOf, &C2)?);
                assert_eq!(g.iter().count(), 2);
                assert!   (g.remove(&C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.iter().count(), 1);
                assert!   (g.remove(&C1, &rdfs::subClassOf, &C2)?);
                assert_eq!(g.iter().count(), 0);
                Ok(())
            }

            #[test]
            fn test_no_duplicate() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.iter().count(), 0);
                assert!   (g.insert(&C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.iter().count(), 1);
                assert!  (!g.insert(&C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.iter().count(), 1);
                assert!   (g.remove(&C1, &rdf::type_, &rdfs::Class)?);
                assert_eq!(g.iter().count(), 0);
                assert!  (!g.remove(&C1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_sink_mutations() {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.iter().count(), 0);
                assert_eq!(make_triple_source().into_sink(&mut g.inserter()).unwrap(), 2);
                assert_eq!(g.iter().count(), 2);
                assert_eq!(make_triple_source().into_sink(&mut g.inserter()).unwrap(), 0);
                assert_eq!(g.iter().count(), 2);
                assert_eq!(make_triple_source().into_sink(&mut g.remover()).unwrap(), 2);
                assert_eq!(g.iter().count(), 0);
                assert_eq!(make_triple_source().into_sink(&mut g.remover()).unwrap(), 0);
                assert_eq!(g.iter().count(), 0);
            }

            #[test]
            fn test_x_all_mutations() {
                let mut g = $mutable_graph_impl::new();
                assert_eq!(g.iter().count(), 0);
                assert_eq!(g.insert_all(make_triple_source()).unwrap(), 2);
                assert_eq!(g.iter().count(), 2);
                assert_eq!(g.insert_all(make_triple_source()).unwrap(), 0);
                assert_eq!(g.iter().count(), 2);
                assert_eq!(g.remove_all(make_triple_source()).unwrap(), 2);
                assert_eq!(g.iter().count(), 0);
                assert_eq!(g.remove_all(make_triple_source()).unwrap(), 0);
                assert_eq!(g.iter().count(), 0);
            }

            #[test]
            fn test_remove_matching() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let o_matcher = [C1.clone(), C2.clone()];
                g.remove_matching(&ANY, &rdf::type_, &o_matcher[..])?;
                assert_consistent_hint(12, g.iter().size_hint());
                Ok(())
            }

            #[test]
            fn test_retain() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let o_matcher = [C1.clone(), C2.clone()];
                g.retain(&ANY, &rdf::type_, &o_matcher[..])?;
                print!("{:?}", g.iter().size_hint());
                assert_consistent_hint(4, g.iter().size_hint());
                Ok(())
            }

            // Test Graph

            #[test]
            fn test_iter_and_hint() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter().oks().map(as_box_t).collect();
                assert_eq!(v.len(), g.iter().count());
                assert_consistent_hint(v.len(), g.iter().size_hint());
                assert!(v.contains(&C1, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&P1, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.iter_matching(&ANY, &ANY, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), g.iter().count());
                assert_consistent_hint(v.len(), g.iter().size_hint());
                assert!(v.contains(&C1, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&P1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_s() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter_for_s(&C2).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.iter_for_s(&C2).size_hint());
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&C1, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.iter_matching(&*C2, &ANY, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.iter_for_s(&C2).size_hint());
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&C1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_p() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter_for_p(&rdfs::subClassOf).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.iter_for_p(&rdfs::subClassOf).size_hint());
                assert!(v.contains(&C2, &rdfs::subClassOf, &C1)?);
                assert!(!v.contains(&C2, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.iter_matching(&ANY, &rdfs::subClassOf, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.iter_for_p(&rdfs::subClassOf).size_hint());
                assert!(v.contains(&C2, &rdfs::subClassOf, &C1)?);
                assert!(!v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_o() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter_for_o(&I2B).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.iter_for_o(&I2B).size_hint());
                assert!(v.contains(&I1B, &P1, &I2B)?);
                assert!(!v.contains(&I2B, &rdf::type_, &C2)?);

                let v: Vec<_> = g.iter_matching(&ANY, &ANY, &*I2B).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.iter_for_o(&I2B).size_hint());
                assert!(v.contains(&I1B, &P1, &I2B)?);
                assert!(!v.contains(&I2B, &rdf::type_, &C2)?);
                Ok(())
            }

            #[test]
            fn test_x_for_sp() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter_for_sp(&C2, &rdf::type_).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.iter_for_sp(&C2, &rdf::type_).size_hint());
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&C2, &rdfs::subClassOf, &C1)?);

                let v: Vec<_> = g.iter_matching(&*C2, &rdf::type_, &ANY).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.iter_for_sp(&C2, &rdf::type_).size_hint());
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&C2, &rdfs::subClassOf, &C1)?);
                Ok(())
            }

            #[test]
            fn test_x_for_so() -> TestResult  {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter_for_so(&C2, &C1).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.iter_for_so(&C2, &C1).size_hint());
                assert!(v.contains(&C2, &rdfs::subClassOf, &C1)?);
                assert!(!v.contains(&C2, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.iter_matching(&*C2, &ANY, &*C1).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert_consistent_hint(v.len(), g.iter_for_so(&C2, &C1).size_hint());
                assert!(v.contains(&C2, &rdfs::subClassOf, &C1)?);
                assert!(!v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_x_for_po() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter_for_po(&rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.iter_for_po(&rdf::type_, &rdfs::Class).size_hint());
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&P2, &rdf::type_, &rdf::Property)?);

                let v: Vec<_> = g.iter_matching(&ANY, &rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 2);
                assert_consistent_hint(v.len(), g.iter_for_po(&rdf::type_, &rdfs::Class).size_hint());
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&P2, &rdf::type_, &rdf::Property)?);
                Ok(())
            }

            #[test]
            fn test_x_for_spo() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let v: Vec<_> = g.iter_for_spo(&C2, &rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&C1, &rdf::type_, &rdfs::Class)?);

                let v: Vec<_> = g.iter_matching(&*C2, &rdf::type_, &rdfs::Class).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 1);
                assert!(v.contains(&C2, &rdf::type_, &rdfs::Class)?);
                assert!(!v.contains(&C1, &rdf::type_, &rdfs::Class)?);
                Ok(())
            }

            #[test]
            fn test_contains() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;
                assert!(g.contains(&C2, &rdfs::subClassOf, &C1)?);
                assert!(!g.contains(&C1, &rdfs::subClassOf, &C2)?);
                Ok(())
            }

            #[test]
            fn test_iter_matching() -> TestResult {
                let mut g = $mutable_graph_impl::new();
                populate(&mut g)?;

                let p_matcher: [StaticTerm;2] = [rdf::type_.clone(), rdfs::domain.clone()];
                let o_matcher: [StaticTerm;2] = [C2.clone(), rdfs::Class.clone()];
                let v: Vec<_> = g.iter_matching(&ANY, &p_matcher[..], &o_matcher[..]).oks().map(as_box_t).collect();
                assert_eq!(v.len(), 5);
                assert!(v.contains(&C1, &rdf::type_, &rdfs::Class)?);
                assert!(v.contains(&P2, &rdfs::domain, &C2)?);
                assert!(v.contains(&I2A, &rdf::type_, &C2)?);
                assert!(!v.contains(&P1, &rdfs::domain, &C1)?);
                assert!(!v.contains(&I1A, &rdf::type_, &C1)?);
                Ok(())
            }

            // helper functions

            fn populate<G: MutableGraph> (g: &mut G) -> Result<(), G::MutationError> {
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

                assert_consistent_hint(16, g.iter().size_hint());
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
                for t in g.iter() {
                    let t = t.unwrap();
                    println!("{:?}\n{:?}\n{:?}\n\n", t.s(), t.p(), t.o());
                }
                println!(">>>>");
            }

            fn assert_consistent_hint(val: usize, hint: (usize, Option<usize>)) {
                assert!(hint.0 <= val);
                assert!(val <= hint.1.or(Some(val)).unwrap())
            }

            fn make_triple_source() -> impl TripleSource {
                vec![
                    [&*C1, &rdf::type_, &rdfs::Class],
                    [&*C1, &rdfs::subClassOf, &*C2],
                ].into_iter()
                .wrap_as_oks()
            }


        }
    }
}
