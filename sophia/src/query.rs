//! Early attempt at coding a query processor

use std::collections::HashMap;
use std::iter::once;

use resiter::map::*;

use crate::graph::*;
use crate::term::*;
use crate::triple::*;

pub type Binding = HashMap<String, RcTerm>;

pub enum Query {
    Triples(Vec<[RcTerm; 3]>),
}

impl Query {
    fn prepare<'a, G: Graph<'a>>(&mut self, graph: &'a G, initial_binding: &Binding) {
        match self {
            Query::Triples(triples) => {
                // sorts triple from q according to how many results they may give
                let mut hints: Vec<_> = triples
                    .iter()
                    .map(|t| {
                        let tm = vec![
                            matcher(t.s(), &initial_binding),
                            matcher(t.p(), &initial_binding),
                            matcher(t.o(), &initial_binding),
                        ];
                        // NB: the unsafe code below is used to cheat about tm's lifetime.
                        // Because G is bound to 'a, triples_matching() requires tm to live as long as 'a.
                        // But in fact, that is not necessary, because we are consuming the iterator immediately.
                        let tm_ref = unsafe { &*(&tm[..] as *const [Option<RcTerm>]) };
                        let hint = triples_matching(graph, tm_ref).size_hint();
                        (hint.1.unwrap_or(std::usize::MAX), hint.0)
                    })
                    .collect();
                for i in 1..hints.len() {
                    let mut j = i;
                    while j > 0 && hints[j - 1] > hints[j] {
                        hints.swap(j - 1, j);
                        triples.swap(j - 1, j);
                        j -= 1;
                    }
                }
            }
        }
    }

    /// Process this query against the given graph, and return an fallible iterator of Bindings.
    ///
    /// The iterator may fail (i.e. yield `Err`) if an operation on the graph fails.
    pub fn process<'a, G: Graph<'a>>(
        &'a mut self,
        graph: &'a G,
    ) -> Box<dyn Iterator<Item = GResult<'a, G, Binding>> + 'a> {
        self.process_with(graph, Binding::new())
    }

    /// Process this query against the given graph, and return an fallible iterator of Bindings,
    /// starting with the given binding.
    ///
    /// The iterator may fail (i.e. yield `Err`) if an operation on the graph fails.
    pub fn process_with<'a, G: Graph<'a>>(
        &'a mut self,
        graph: &'a G,
        initial_binding: Binding,
    ) -> Box<dyn Iterator<Item = GResult<'a, G, Binding>> + 'a> {
        self.prepare(graph, &initial_binding);
        match self {
            Query::Triples(triples) => bindings_for_triples(graph, triples, initial_binding),
        }
    }
}

/// Iter over the bindings of all triples in `q` for graph `g`, given the binding `b`.
fn bindings_for_triples<'a, G>(
    g: &'a G,
    q: &'a [[RcTerm; 3]],
    b: Binding,
) -> Box<dyn Iterator<Item = GResult<'a, G, Binding>> + 'a>
where
    G: Graph<'a>,
{
    if q.is_empty() {
        Box::new(once(Ok(b)))
    } else {
        Box::new(
            bindings_for_triple(g, &q[0], b).flat_map(move |res| match res {
                Err(err) => Box::new(once(Err(err))),
                Ok(b2) => bindings_for_triples(g, &q[1..], b2),
            }),
        )
    }
}

/// Iter over the bindings of triple `tq` for graph `g`, given the binding `b`.
fn bindings_for_triple<'a, G>(
    g: &'a G,
    tq: &'a [RcTerm; 3],
    b: Binding,
) -> impl Iterator<Item = GResult<'a, G, Binding>> + 'a
where
    G: Graph<'a>,
{
    let tm = vec![
        matcher(tq.s(), &b),
        matcher(tq.p(), &b),
        matcher(tq.o(), &b),
    ];
    // NB: the unsafe code below is used to convince the compiler that &tm has lifetime 'a .
    // We can guarantee that because the closure below takes ownership of tm,
    // and it will live as long as the returned iterator.
    triples_matching(g, unsafe { &*(&tm[..] as *const [Option<RcTerm>]) }).map_ok(move |tr| {
        let mut b2 = b.clone();
        if tm[0].is_none() {
            b2.insert(tq.s().value(), tr.s().into());
        }
        if tm[1].is_none() {
            b2.insert(tq.p().value(), tr.p().into());
        }
        if tm[2].is_none() {
            b2.insert(tq.o().value(), tr.o().into());
        }
        b2
    })
}

/// Make a matcher corresponding to term `t`, given binding `b`.
fn matcher(t: &RcTerm, b: &Binding) -> Option<RcTerm> {
    if let Variable(vname) = t {
        let vname: &str = &vname;
        b.get(vname).cloned()
    } else {
        Some(t.clone())
    }
}

/// A wrapper around Graph::triples_matchings, with more convenient parameters.
fn triples_matching<'a, G>(g: &'a G, tm: &'a [Option<RcTerm>]) -> GTripleSource<'a, G>
where
    G: Graph<'a>,
{
    debug_assert_eq!(tm.len(), 3);
    let s = &tm[0];
    let p = &tm[1];
    let o = &tm[2];
    g.triples_matching(s, p, o)
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::graph::inmem::FastGraph;
    use crate::ns::{rdf, Namespace};
    use crate::term::RcTerm;

    #[test]
    fn test_bindings_for_triple_0var_0() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_event = schema.get("Event").unwrap();
        let x_alice = RcTerm::new_iri("http://example.org/alice").unwrap();

        let tq: [RcTerm; 3] = [
            x_alice.clone(),
            RcTerm::from(&rdf::type_),
            RcTerm::from(&s_event),
        ];

        let results: Result<Vec<_>, _> = bindings_for_triple(&g, &tq, Binding::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_bindings_for_triple_0var_1() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_person = schema.get("Person").unwrap();
        let x_alice = RcTerm::new_iri("http://example.org/alice").unwrap();

        let tq: [RcTerm; 3] = [
            x_alice.clone(),
            RcTerm::from(&rdf::type_),
            RcTerm::from(&s_person),
        ];

        let results: Result<Vec<Binding>, _> =
            bindings_for_triple(&g, &tq, Binding::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 1);
        for r in results.iter() {
            assert_eq!(r.len(), 0);
        }
    }

    #[test]
    fn test_bindings_for_triple_1var_0() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_event = schema.get("Event").unwrap();

        let v1 = RcTerm::new_variable("v1").unwrap();

        let tq: [RcTerm; 3] = [
            v1.clone(),
            RcTerm::from(&rdf::type_),
            RcTerm::from(&s_event),
        ];

        let results: Result<Vec<Binding>, _> =
            bindings_for_triple(&g, &tq, Binding::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_bindings_for_triple_1var() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_person = schema.get("Person").unwrap();

        let v1 = RcTerm::new_variable("v1").unwrap();

        let tq: [RcTerm; 3] = [
            v1.clone(),
            RcTerm::from(&rdf::type_),
            RcTerm::from(&s_person),
        ];

        let results: Result<Vec<Binding>, _> =
            bindings_for_triple(&g, &tq, Binding::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 3);
        for r in results.iter() {
            assert_eq!(r.len(), 1);
            assert!(r.contains_key("v1"));
        }
        let mut results: Vec<_> = results
            .into_iter()
            .map(|b| b.get("v1").unwrap().value())
            .collect();
        results.sort();
        assert_eq!(results[0], "http://example.org/alice");
        assert_eq!(results[1], "http://example.org/bob");
        assert_eq!(results[2], "http://example.org/charlie");
    }

    #[test]
    fn test_bindings_for_triple_2var() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_name = schema.get("name").unwrap();

        let v1 = RcTerm::new_variable("v1").unwrap();
        let v2 = RcTerm::new_variable("v2").unwrap();

        let tq: [RcTerm; 3] = [v1.clone(), RcTerm::from(&s_name), v2.clone()];

        let results: Result<Vec<Binding>, _> =
            bindings_for_triple(&g, &tq, Binding::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 5);
        for r in results.iter() {
            assert_eq!(r.len(), 2);
            assert!(r.contains_key("v1"));
            assert!(r.contains_key("v2"));
        }
        let mut results: Vec<_> = results
            .into_iter()
            .map(|b| {
                format!(
                    "{} {}",
                    b.get("v1").unwrap().value(),
                    b.get("v2").unwrap().value(),
                )
            })
            .collect();
        results.sort();
        assert_eq!(results[0], "http://example.org/alice Alice");
        assert_eq!(results[1], "http://example.org/alice_n_bob Alice & Bob");
        assert_eq!(results[2], "http://example.org/bob Bob");
        assert_eq!(results[3], "http://example.org/charlie Charlie");
        assert_eq!(results[4], "http://example.org/dan Dan");
    }

    #[test]
    fn test_query_triples() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_person = schema.get("Person").unwrap();
        let s_name = schema.get("name").unwrap();

        let v1 = RcTerm::new_variable("v1").unwrap();
        let v2 = RcTerm::new_variable("v2").unwrap();

        let mut q = Query::Triples(vec![
            [v1.clone(), RcTerm::from(&s_name), v2.clone()],
            [
                v1.clone(),
                RcTerm::from(&rdf::type_),
                RcTerm::from(&s_person),
            ],
        ]);

        let results: Result<Vec<Binding>, _> = q.process(&g).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 3);
        for r in results.iter() {
            assert_eq!(r.len(), 2);
            assert!(r.contains_key("v1"));
            assert!(r.contains_key("v2"));
        }
        let mut results: Vec<_> = results
            .into_iter()
            .map(|b| {
                format!(
                    "{} {}",
                    b.get("v1").unwrap().value(),
                    b.get("v2").unwrap().value(),
                )
            })
            .collect();
        results.sort();
        assert_eq!(results[0], "http://example.org/alice Alice");
        assert_eq!(results[1], "http://example.org/bob Bob");
        assert_eq!(results[2], "http://example.org/charlie Charlie");
    }

    fn data() -> FastGraph {
        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_person = schema.get("Person").unwrap();
        let s_organization = schema.get("Organization").unwrap();
        let s_name = schema.get("name").unwrap();
        let s_member = schema.get("member").unwrap();

        let example = Namespace::new("http://example.org/").unwrap();
        let x_alice = example.get("alice").unwrap();
        let x_bob = example.get("bob").unwrap();
        let x_charlie = example.get("charlie").unwrap();
        let x_dan = example.get("dan").unwrap();
        let x_alice_n_bob = example.get("alice_n_bob").unwrap();

        let mut g = FastGraph::new();
        g.insert(&x_alice, &rdf::type_, &s_person).unwrap();
        g.insert(&x_alice, &s_name, &"Alice".into()).unwrap();
        g.insert(&x_bob, &rdf::type_, &s_person).unwrap();
        g.insert(&x_bob, &s_name, &"Bob".into()).unwrap();
        g.insert(&x_charlie, &rdf::type_, &s_person).unwrap();
        g.insert(&x_charlie, &s_name, &"Charlie".into()).unwrap();
        g.insert(&x_dan, &s_name, &"Dan".into()).unwrap();
        g.insert(&x_alice_n_bob, &rdf::type_, &s_organization)
            .unwrap();
        g.insert(&x_alice_n_bob, &s_name, &"Alice & Bob".into())
            .unwrap();
        g.insert(&x_alice_n_bob, &s_member, &x_alice).unwrap();
        g.insert(&x_alice_n_bob, &s_member, &x_bob).unwrap();

        g
    }
}
