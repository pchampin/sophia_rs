# RDF Graphs

The [`Graph`] and [`MutableGraph`] traits define how you interact with [RDF graphs] in Sophia.

## Using graphs

RDF graphs are sets of triples,
so the most common thing you need to do with a graph is to iterate over its triples.
This is achieved with the [`Graph::triples`] method:

```rust,noplayground
# use sophia::api::prelude::*;
# use sophia::inmem::graph::LightGraph;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let g = LightGraph::new();
for result in g.triples() {
	let triple = result?;
	// do something with t;
}
# Ok(()) }
```
Notice that [`Graph::triples`] yields [`Result`]s,
as some implementations of [`Graph`] may fail at any point of the iteration.

When only a subset of the triples in the graph are of interest,
you will want to use the [`Graph::triples_matching`] method:

```rust,noplayground
# use sophia::api::{ns::rdf, prelude::*, term::SimpleTerm};
# use sophia::inmem::graph::LightGraph;

# let graph = LightGraph::new();
// Utility closure to recognize IRIs in the schema.org namespace
let in_schema_org = |t: SimpleTerm| -> bool {
	t.iri()
	 .map(|iri| iri.as_str().starts_with(("http://schema.org/")))
	 .unwrap_or(false)
};
// Iter over all instances of schema.org types
graph
	.triples_matching(Any, [rdf::type_], in_schema_org)
	.map(|res| { let [s, _, o] = res.unwrap().to_spo(); (s, o)})
	.for_each(|(instance, typ)| {
		// do something 
	})
```

[`Graph::triples_matching`] accepts a large variety of parameters,
which will be described in more detail [in the next chapter](./ch05_term_matchers.md).

[`Graph`] also provide methods to iterate over all unique [subjects](https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.subjects),
[predicate](https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.predicates)
and [object](https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.objects)
in the graph,
as well as over all unique terms of a certain kind
([`Graph::iris`], [`Graph::blank_nodes`], [`Graph::literals`], etc.).

Finally, it is possible to check whether a graph contains a specific triple with the method [`Graph::contains`].


## Mutating graphs

Any implementation of [`Graph`] that can be mutated should also implement [`MutableGraph`],
which comes with additional methods for modifying the graph.
Individual triples can be added to the graph (resp. removed from the graph)
with [`MutableGraph::insert`] (resp. [`MutableGraph::remove`]).
Inserting (resp. removing) a triple that is already (resp. not) present in the graph will be essentially a noop.

```rust,noplayground
# use sophia::{api::{ns::rdf, prelude::*}, iri::*};
/// Example: increment the rdf:value of a given subject
# fn f<G: MutableGraph>(mut g: G) -> Result<(), Box<dyn std::error::Error>> {
# let s = Iri::new_unchecked("https://example.org/foo");
let old_value: i32 = g.triples_matching([s], [rdf::value], Any)
	.next()
	.unwrap()?
	.o()
	.try_into_term()?;
g.remove(s, rdf::value, old_value)?;
g.insert(s, rdf::value, old_value + 1)?;
# Ok(()) }
```

Batch modifications can also be performed on mutable graphs:

* [`MutableGraph::insert_all`](https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html#method.insert_all)
  inserts all the triples from a triple source[^triple_source];
* [`MutableGraph::remove_all`](https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html#method.remove_all)
  removes all the triples from a triple source[^triple_source];
* [`MutableGraph::remove_matching`](https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html#method.remove_matching)
  removes all the triples matching the parameters;
* [`MutableGraph::retain_matching`](https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraphremove.html#method.retain_matching)
  removes all the triples *except* those matching the parameters.
	
The parameters of `remove_matching` and `retain_matching` are similar to those of [`Graph::triples_matching`]
and are described in more detail in the [next chapter](./ch05_term_matchers.md).


## Useful types implementing [`Graph`]

* slices of [triples] implement [`Graph`];
* standard collections ([`Vec`], [`HashSet`] and [`BTreeSet`]) of [triples] implement [`Graph`] and [`MutableGraph`];
* [`sophia::inmem::LightGraph`] provides a [`Graph`] and [`MutableGraph`] implementation with a low memory footprint;
* [`sophia::inmem::FastGraph`] provides a [`Graph`] and [`MutableGraph`] implementation design for fast retrieval of any given triple.

## Recipies for constructing graphs

### Constructing and populating an empty graph

```rust,noplayground
# use sophia::{api::{ns::{Namespace, rdf}, prelude::*}, inmem::graph::FastGraph};
let mut g = FastGraph::new();
let ex = Namespace::new_unchecked("https://example.org/ns#");
let alice = ex.get_unchecked("alice");
let s = Namespace::new_unchecked("http://schema.org/");
g.insert(
	&alice,
	rdf::type_,
	s.get_unchecked("Person")
).unwrap();
g.insert(
	&alice,
	s.get_unchecked("name"),
  "Alice"
).unwrap();
```

### Constructing a graph from a triple source[^triple_source]

```rust,noplayground
# use sophia::{api::prelude::*, inmem::graph::FastGraph, iri::Iri};
# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let big_graph = FastGraph::new();
// Extract all triples about 'alice' from big_graph in a new graph
let alice = Iri::new_unchecked("https://example.org/ns#alice");
let graph: FastGraph = big_graph
	.triples_matching([alice], Any, Any)
	.collect_triples()?;
# Ok(()) }
```

NB: Only types implementing [`CollectibleGraph`]
can actually be constructed with the `collected_triples` method as above.
However, most types implementing [`Graph`] should implement [`CollectibleGraph`].

### Constructing a graph from a file

```rust,noplayground
# use sophia::{api::prelude::*, inmem::graph::FastGraph, iri::Iri};
# use std::{io::BufReader, fs::File};
use sophia::turtle::parser::turtle;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
dbg!(std::env::current_dir());
let f = BufReader::new(File::open("../sophia_doap.ttl")?);
let graph: FastGraph = turtle::parse_bufread(f)
	.collect_triples()?;
# Ok(()) }
```

For more about parsing (and serializing), see [the corresponding chapter](./ch07_parsing_and_serializing.md).

----

[^triple_source]: a [`TripleSource`] is a fallible stream of triples,
such as those returned by [`Graph::triples`] or [`Graph::triples_matching`],
or those returned by [parsers].
In particular, any iterator of `Result<T, E>` where `T: `[`Triple`] is a [`TripleSource`].


[`Graph`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html
[`MutableGraph`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html
[RDF graphs]: https://www.w3.org/TR/rdf-concepts/#dfn-rdf-graph
[`Graph::triples`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#tymethod.triples
[`Result`]: https://doc.rust-lang.org/std/result/enum.Result.html
[`Graph::triples_matching`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.triples_matching
[`Graph::iris`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.iris
[`Graph::literals`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.literals
[`Graph::blank_nodes`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.blank_nodes
[`Graph::contains`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.Graph.html#method.contains
[`MutableGraph::insert`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html#tymethod.insert
[`MutableGraph::remove`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html#tymethod.remove
[`MutableGraph::insert_triple`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html#method.insert_triple
[`MutableGraph::remove_triple`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.MutableGraph.html#method.remove_triple
[`Triple`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/triple/trait.Triple.html
[triple source]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/source/trait.TripleSource.html
[triples]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/triple/trait.Triple.html
[`Vec`]: https://doc.rust-lang.org/std/vec/struct.Vec.html
[`HashSet`]: https://doc.rust-lang.org/std/collections/struct.HashSet.html
[`BTreeSet`]: https://doc.rust-lang.org/std/collections/struct.BTreeSet.html
[`sophia::inmem::LightGraph`]: https://docs.rs/sophia_inmem/0.8.0-alpha.3/sophia_inmem/graph/type.LightGraph.html
[`sophia::inmem::FastGraph`]: https://docs.rs/sophia_inmem/0.8.0-alpha.3/sophia_inmem/graph/type.FastGraph.html
[`TripleSource`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/source/trait.TripleSource.html
[parsers]: ./ch07_parsing_and_serializing.md
[`CollectibeGraph`]: https://docs.rs/sophia_api/0.8.0-alpha.3/sophia_api/graph/trait.CollectibleGraph.html
