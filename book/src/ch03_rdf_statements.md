# RDF Statements

The [`Triple`] and [`Quad`] traits define how you interact with [RDF statements] in Sophia.

Note that in Sophia's [generalized RDF] model, terms of any kind can occur in any position in a statement.
This contrasts to strict RDF where only IRIs can occur in predicate position,
and where literals can only occur in the object position.


## Using triples

Triples in RDF are made of a subject, a predicate and an object.
They can be obtained respectively via the methods [`Triple::s`], [`Triple::p`] and [`Triple::o`],
or all at once (as an array of three terms) via the method [`Triple::spo`].
These methods also have a `to_X` version that destructs the original triple instead of borrowing it.

```rust,noplayground
# use sophia::api::{ns::rdf, prelude::*};
// Example: yield all the terms being used as types in the given triples
fn all_types<IT, T>(triples: IT) -> impl Iterator<Item=T::Term>
where
  IT: IntoIterator<Item=T>,
  T: Triple,
{
  triples
    .into_iter()
    .filter(|t| rdf::type_ == t.p())
    .map(|t| t.to_o())
}
```

## Using quads

Quads are used to represent triples in the context of an optional [named graph].
Like triples, they have methods [`Quad::s`], [`Quad::p`] and [`Quad::o`],
but also [`Quad::g`] to access the optional graph name,
and [`Quad::spog`] to obtain all four components all at once.
These methods also have a `to_X` version that destructs the original quad instead of borrowing it.

```rust,noplayground
# use sophia::api::{ns::rdf, prelude::*};
// Example: yield all the triples in the default graph, from a list of quads
fn all_types<IQ, Q>(quads: IQ) -> impl Iterator<Item=[Q::Term; 3]>
where
  IQ: IntoIterator<Item=Q>,
  Q: Quad,
{
  quads
    .into_iter()
    .filter(|q| q.g().is_none())
    .map(|q| { let (spo, _g) = q.to_spog(); spo })
}
```

## Comparing triples or quads

To check whether two values implementing [`Triple`] (resp. [`Quad`])
represent the same RDF statements, the method [`Triple::eq`] (resp. [`Quad::eq`])
must be used.
It will compare each component of the statements using the [`Term::eq`] method.
Note that the `==` operator may give a different result than [`Triple::eq`] or [`Quad::eq`]
on some types implementing the [`Triple`] or the [`Quad`] trait.


## Useful types implementing [`Triple`]

While the [`Triple`] and [`Quad`] traits can be implemented by multiple types,
in most situations the following types will be used:

* `[T; 3]` where `T: `[`Term`] implements [`Triple`]
* `([T; 3], Option<T>)` where `T: `[`Term`] implements [`Quad`]



[RDF statements]: https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-statement
[generalized RDF]: ch00_introduction.html#generalized
[`Triple`]: https://docs.rs/sophia_api/0.8.0/sophia_api/triple/trait.Triple.html
[`Quad`]: https://docs.rs/sophia_api/0.8.0/sophia_api/quad/trait.Quad.html
[`Triple::s`]: https://docs.rs/sophia_api/0.8.0/sophia_api/triple/trait.Triple.html#tymethod.s
[`Triple::p`]: https://docs.rs/sophia_api/0.8.0/sophia_api/triple/trait.Triple.html#tymethod.p
[`Triple::o`]: https://docs.rs/sophia_api/0.8.0/sophia_api/triple/trait.Triple.html#tymethod.o
[`Triple::spo`]: https://docs.rs/sophia_api/0.8.0/sophia_api/triple/trait.Triple.html#method.spo
[named graph]: https://www.w3.org/TR/rdf12-concepts/#dfn-named-graph
[`Quad::s`]: https://docs.rs/sophia_api/0.8.0/sophia_api/quad/trait.Quad.html#tymethod.s
[`Quad::p`]: https://docs.rs/sophia_api/0.8.0/sophia_api/quad/trait.Quad.html#tymethod.p
[`Quad::o`]: https://docs.rs/sophia_api/0.8.0/sophia_api/quad/trait.Quad.html#tymethod.o
[`Quad::g`]: https://docs.rs/sophia_api/0.8.0/sophia_api/quad/trait.Quad.html#tymethod.g
[`Quad::spog`]: https://docs.rs/sophia_api/0.8.0/sophia_api/quad/trait.Quad.html#method.spog
[`Term::eq`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html#method.eq
[`Triple::eq`]: https://docs.rs/sophia_api/0.8.0/sophia_api/triple/trait.Triple.html#method.eq
[`Quad::eq`]: https://docs.rs/sophia_api/0.8.0/sophia_api/quad/trait.Quad.html#method.eq
[`Term`]: https://docs.rs/sophia_api/0.8.0/sophia_api/term/trait.Term.html
