# Changes since version 0.7

Sophia has been heavily refactored between version 0.7 and 0.8. This refactoring was triggered by the use of [Generic Ascciated Types](https://blog.rust-lang.org/2022/10/28/gats-stabilization.html) (GATs), that have finally landed in stable Rust. But this was also an opportunity to make a number of other changes.

## The benefit of GATs

The main benefit of GATs is to get rid of [odd patterns](https://docs.rs/sophia/0.7.2/sophia/triple/streaming_mode/index.html) that were introduced in Sophia in order to keep it generic enough to support multiple implementation choices. The drawback of this approach was that implementing Sophia's traits (especially `Graph` and `Dataset`) could be cumbersome.

As an example, the [`Graph`](https://docs.rs/sophia/0.7.2/sophia/graph/trait.Graph.html) trait used to be
```rust,noplayground,ignore
pub trait Graph {
    type Triple: TripleStreamingMode;
    // ...
}
```

Given a type `MyGraph` implementing that trait, the actual type of triples yielded by [`MyGraph::triples`](https://docs.rs/sophia/0.7.2/sophia/graph/trait.Graph.html#tymethod.triples) could not be immediately determined, and was [quite intricate](https://docs.rs/sophia/latest/sophia/graph/type.GTriple.html). This could be inconvenient for some users of `MyGraph`, and was usually cumbersome for the implementer.

Compare to the new definition of the `Graph` trait:
```rust,noplayground,ignore
pub trait Graph {
    type Triple<'x>: Triple where Self: 'x;
    // ...
}
```
where `Graph::triples` now yield triples whose type is exactly `Graph::Triple<'_>`. Much easier.

The same pattern existed for [`Dataset`](https://docs.rs/sophia/0.7.2/sophia/dataset/trait.Dataset.html),
[`TripleSource`](https://docs.rs/sophia/0.7.2/sophia/triple/stream/trait.TripleSource.html), and
[`QuadSource`](https://docs.rs/sophia/0.7.2/sophia/quad/stream/trait.QuadSource.html),
where GATs have now also replaced it.

## The new `Term` trait

The old [`TTerm`](https://docs.rs/sophia/latest/sophia/term/trait.TTerm.html)
trait has been replaced by a new [`Term`](https://github.com/pchampin/sophia_rs/blob/a925e6177cfdd7e90dafd4b917ae0790c40a0165/api/src/term.rs#L87)
trait, with a significantly different API, that serves several purposes:

- it now supports [RDF-star](https://www.w3.org/2021/12/rdf-star.html)
- it now allows atomic types (such as `&str` or `i32`) to be used directly as terms
  (they are interpreted as `xsd:string` and `xsd:integer` literals, respectively).

Any code that handles terms will need some significant rewriting.
Eventually, a chapter of this book will extensively document the new `Term` trait.

### The end of the "IRI zoo"

Historically, a number of different types have been created in Sophia for representing IRIs,
which was [causing some confusion](https://github.com/pchampin/sophia_rs/discussions/112).
Most of them have now disappeared, in favor of the types defined in [`sophia_iri`](https://docs.rs/sophia_iri/latest/sophia_iri/).

### The end of the [`sophia_term`](https://docs.rs/sophia_term/0.7.2/sophia_term/) crate

The [`sophia_term`](https://docs.rs/sophia_term/0.7.2/sophia_term/)
crate no longer exists in version 0.8.0.
The most general types that it provided ([`BoxTerm`](https://docs.rs/sophia_term/0.7.2/sophia_term/type.BoxTerm.html), [`RefTerm`](https://docs.rs/sophia_term/0.7.2/sophia_term/type.RefTerm.html))
are now subsumed by [`SimpleTerm`](https://github.com/pchampin/sophia_rs/blob/a925e6177cfdd7e90dafd4b917ae0790c40a0165/api/src/term/_simple.rs#L8),
a straightforward implementation of the `Term` trait, provided by `sophia_api`.
More specific types (such as [`RcTerm`](https://docs.rs/sophia_term/0.7.2/sophia_term/type.RcTerm.html) or [`ArcTerm`](https://docs.rs/sophia_term/0.7.2/sophia_term/type.ArcTerm.html))
have been deemed non-essential, and are no longer provided at the moment.

## Simplification of the `Graph` and `Dataset` traits

In version 0.7, the `Graph` trait had a number of specialized methods for retrieving selected triples,
such as [`triples_with_s`](https://docs.rs/sophia_api/0.7.2/sophia_api/graph/trait.Graph.html#method.triples_with_s)
or [`triples_with_po`](https://docs.rs/sophia_api/0.7.2/sophia_api/graph/trait.Graph.html#method.triples_with_po)
(and similarly for `Dataset`: `quads_with_s`, etc.).

All these methods have disappeared in favor of `triples_matching`,
so that instead of:
```rust,noplayground,ignore
for t in g.triples_with_s(mys) {
    // ...
}
```
one should now write
```rust,noplayground
# extern crate sophia;
# use sophia::api::graph::Graph;
# use sophia::api::term::matcher::Any;
# let g: Vec<[i32; 3]> = vec![]; // dummy graph type
# let mys = 42;
for t in g.triples_matching([mys], Any, Any) {
    // ...
}
```
and the performances will be the same
(depending, of course, of how carefully the `Graph`/`Dataset` was implemented,
but that was already the case with the previous API).

## The `sophia` crate

As before, Sophia is still made of several specialized crates
(`sophia_api`, `sophia_iri`, `sophia_turtle`...)
that are all packaged in a one-stop-shop crate named `sophia`.
Note however that the structure of that crate as changed significantly.
In version 0.7, it re-exported symbols from the smaller crates in its *own* module hierarchy, mostly for historical reason.
In version 0.8, it simply exposes the smaller crates into a corresponding module,
e.g. `sophia::api` re-exports the root module of `sophia_api`, and so on.

## Requesting help

As migration from version 0.7 to version 0.8 can be challenging,
a [dedicated tag](https://github.com/pchampin/sophia_rs/labels/v0.8_migration)
has been added on the github repository of Sophia to mark migration issues and request assistance.
