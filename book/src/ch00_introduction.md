# Introduction

The [sophia crate](https://crates.io/crates/sophia) aims at providing a comprehensive toolkit for working with RDF and Linked Data in Rust.

[RDF] is a data model designed to exchange knowledge on the Web in an interoperable way. Each piece of knowledge in RDF (a [statement]) is represented by a [triple], made of three [terms]. A set of triples forms an RDF [graph]. Finally, several graphs can be grouped in a collection called a [dataset], where each graph is identified by a unique name.

In Sophia, each of these core concepts is modeled by a trait, which can be implemented in multiple ways (see for example the [`Graph`] trait and [some of the types implementing it](https://docs.rs/sophia_api/latest/sophia_api/graph/trait.Graph.html#foreign-impls)). Sophia is therefore not meant to provide the "ultimate" implementation of RDF in Rust, but a generic framework to help various implementations to interoperate with each other (in the spirit of [Apache Commons RDF] for Java or [RDFJS] for Javascript/Typescript).

## Generalized vs. Strict RDF model {#generialized}
The data model supported by this Sophia is in fact
a superset of the RDF data model as defined by the W3C.
When the distinction matters,
they will be called, respectively,
the *generalized* RDF model, and the *strict* RDF model.
The generalized RDF model extends RDF as follows:
* In addition to standard RDF terms (IRIs, blank nodes, literals, and triple terms),
  Sophia supports variables (a concept borrowed from [SPARQL] or [Notation3])
* Sophia allows any kind of term in any position (subject, predicate, object, graph name).
* Sophia allow IRIs to be relative IRI references
  (while in strict RDF, [IRIs must be absolute](https://www.w3.org/TR/rdf12-concepts/#section-IRIs)).

[RDF]: https://www.w3.org/TR/rdf12-concepts/
[statement]: https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-statement
[triple]: https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-triple
[terms]: https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-term
[graph]: https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-graph
[dataset]: https://www.w3.org/TR/rdf12-concepts/#dfn-rdf-dataset

[`Graph`]: https://docs.rs/sophia_api/latest/sophia_api/graph/trait.Graph.html
[Apache Commons RDF]: https://github.com/apache/commons-rdf/
[RDF/JS]: https://rdf.js.org/

