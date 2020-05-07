//! Query processing over RDF graphs and datasets.
//!
//! **Important**: this is a preliminary and incomplete implementation.
//! The API of this module is likely to change heavily in the future.

use std::collections::HashMap;
use std::iter::once;

use resiter::map::*;
use sophia_term::matcher::AnyOrExactly;
use sophia_term::*;

use crate::graph::*;
use crate::triple::*;

/// A map associating variable names to [`term`](../term/enum.Term.html)s.
pub type BindingMap = HashMap<String, RcTerm>;

/// A query can be processed against a graph, producing a sequence of binding maps.
pub enum Query {
    /// [Basic graph pattern](https://www.w3.org/TR/sparql11-query/#BasicGraphPatterns)
    Triples(Vec<[RcTerm; 3]>),
}

impl Query {
    fn prepare<G: Graph>(&mut self, graph: &G, initial_bindings: &BindingMap) {
        match self {
            Query::Triples(triples) => {
                // sorts triple from q according to how many results they may give
                let mut hints: Vec<_> = triples
                    .iter()
                    .map(|t| {
                        let tm = vec![
                            matcher(t.s(), &initial_bindings),
                            matcher(t.p(), &initial_bindings),
                            matcher(t.o(), &initial_bindings),
                        ];
                        let hint = triples_matching(graph, &tm).size_hint();
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

    /// Process this query against the given graph, and return an fallible iterator of BindingMaps.
    ///
    /// The iterator may fail (i.e. yield `Err`) if an operation on the graph fails.
    pub fn process<'s, G: Graph>(
        &'s mut self,
        graph: &'s G,
    ) -> Box<dyn Iterator<Item = GResult<G, BindingMap>> + 's> {
        self.process_with(graph, BindingMap::new())
    }

    /// Process this query against the given graph, and return an fallible iterator of BindingMaps,
    /// starting with the given bindings.
    ///
    /// The iterator may fail (i.e. yield `Err`) if an operation on the graph fails.
    pub fn process_with<'s, G: Graph>(
        &'s mut self,
        graph: &'s G,
        initial_bindings: BindingMap,
    ) -> Box<dyn Iterator<Item = GResult<G, BindingMap>> + 's> {
        self.prepare(graph, &initial_bindings);
        match self {
            Query::Triples(triples) => bindings_for_triples(graph, triples, initial_bindings),
        }
    }
}

/// Iter over the bindings of all triples in `q` for graph `g`, given the binding `b`.
fn bindings_for_triples<'a, G>(
    g: &'a G,
    q: &'a [[RcTerm; 3]],
    b: BindingMap,
) -> Box<dyn Iterator<Item = GResult<G, BindingMap>> + 'a>
where
    G: Graph,
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
    b: BindingMap,
) -> impl Iterator<Item = GResult<G, BindingMap>> + 'a
where
    G: Graph,
{
    let tm = vec![
        matcher(tq.s(), &b),
        matcher(tq.p(), &b),
        matcher(tq.o(), &b),
    ];
    // NB: the unsafe code below is used to convince the compiler that &tm has lifetime 'a .
    // We can guarantee that because the closure below takes ownership of tm,
    // and it will live as long as the returned iterator.
    triples_matching(g, unsafe { &*(&tm[..] as *const [Binding]) }).map_ok(move |tr| {
        let mut b2 = b.clone();
        if tm[0].is_free() {
            b2.insert(tq.s().value().to_string(), RcTerm::copy(tr.s()));
        }
        if tm[1].is_free() {
            b2.insert(tq.p().value().to_string(), RcTerm::copy(tr.p()));
        }
        if tm[2].is_free() {
            b2.insert(tq.o().value().to_string(), RcTerm::copy(tr.o()));
        }
        b2
    })
}

/// Make a matcher corresponding to term `t`, given binding `b`.
fn matcher(t: &RcTerm, b: &BindingMap) -> Binding {
    if let Term::Variable(var) = t {
        let vname: &str = var.as_str();
        b.get(vname).cloned().into()
    } else {
        Binding::Exactly(t.clone())
    }
}

/// A wrapper around Graph::triples_matchings, with more convenient parameters.
fn triples_matching<'a, G>(g: &'a G, tm: &'a [Binding]) -> GTripleSource<'a, G>
where
    G: Graph,
{
    debug_assert_eq!(tm.len(), 3, "tm.len() = {}", tm.len());
    let s = &tm[0];
    let p = &tm[1];
    let o = &tm[2];
    g.triples_matching(s, p, o)
}

type Binding = AnyOrExactly<RcTerm>;

trait BindingExt {
    fn is_free(&self) -> bool;
}

impl BindingExt for Binding {
    fn is_free(&self) -> bool {
        matches!(self, AnyOrExactly::Any)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::graph::inmem::FastGraph;
    use crate::ns::{rdf, Namespace};
    use sophia_term::literal::AsLiteral;
    use sophia_term::RcTerm;

    #[test]
    fn test_bindings_for_triple_0var_0() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_event = schema.get("Event").unwrap();
        let x_alice = RcTerm::new_iri("http://example.org/alice").unwrap();

        let tq: [RcTerm; 3] = [x_alice.clone(), rdf::type_.map_into(), s_event.map_into()];

        let results: Result<Vec<_>, _> = bindings_for_triple(&g, &tq, BindingMap::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_bindings_for_triple_0var_1() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_person = schema.get("Person").unwrap();
        let x_alice = RcTerm::new_iri("http://example.org/alice").unwrap();

        let tq: [RcTerm; 3] = [x_alice.clone(), rdf::type_.map_into(), s_person.map_into()];

        let results: Result<Vec<BindingMap>, _> =
            bindings_for_triple(&g, &tq, BindingMap::new()).collect();
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

        let tq: [RcTerm; 3] = [v1.clone(), rdf::type_.map_into(), s_event.map_into()];

        let results: Result<Vec<BindingMap>, _> =
            bindings_for_triple(&g, &tq, BindingMap::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_bindings_for_triple_1var() {
        let g = data();

        let schema = Namespace::new("http://schema.org/").unwrap();
        let s_person = schema.get("Person").unwrap();

        let v1 = RcTerm::new_variable("v1").unwrap();

        let tq: [RcTerm; 3] = [v1.clone(), rdf::type_.map_into(), s_person.map_into()];

        let results: Result<Vec<BindingMap>, _> =
            bindings_for_triple(&g, &tq, BindingMap::new()).collect();
        let results = results.unwrap();
        assert_eq!(results.len(), 3);
        for r in results.iter() {
            assert_eq!(r.len(), 1);
            assert!(r.contains_key("v1"));
        }
        let mut results: Vec<_> = results
            .into_iter()
            .map(|b| b.get("v1").unwrap().value().to_string())
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

        let tq: [RcTerm; 3] = [v1.clone(), s_name.map_into(), v2.clone()];

        let results: Result<Vec<BindingMap>, _> =
            bindings_for_triple(&g, &tq, BindingMap::new()).collect();
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
            [v1.clone(), s_name.map_into(), v2.clone()],
            [v1.clone(), rdf::type_.map_into(), s_person.map_into()],
        ]);

        let results: Result<Vec<BindingMap>, _> = q.process(&g).collect();
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
        let lit = |txt| AsLiteral::<&str>::as_literal(&txt);
        g.insert(&x_alice, &rdf::type_, &s_person).unwrap();
        g.insert(&x_alice, &s_name, &lit("Alice")).unwrap();
        g.insert(&x_bob, &rdf::type_, &s_person).unwrap();
        g.insert(&x_bob, &s_name, &lit("Bob")).unwrap();
        g.insert(&x_charlie, &rdf::type_, &s_person).unwrap();
        g.insert(&x_charlie, &s_name, &lit("Charlie")).unwrap();
        g.insert(&x_dan, &s_name, &lit("Dan")).unwrap();
        g.insert(&x_alice_n_bob, &rdf::type_, &s_organization)
            .unwrap();
        g.insert(&x_alice_n_bob, &s_name, &lit("Alice & Bob"))
            .unwrap();
        g.insert(&x_alice_n_bob, &s_member, &x_alice).unwrap();
        g.insert(&x_alice_n_bob, &s_member, &x_bob).unwrap();

        g
    }
}
