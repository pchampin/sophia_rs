//! This crate aims to provide a comprehensive toolkit
//! for working with [RDF] and [Linked Data] in Rust.
//!
//! RDF is a data model
//! designed to exchange knowledge on the Web
//! in an interoperable way.
//! Each piece of knowledge in RDF (a *statement*)
//! is represented by a [triple], made of three [term]s.
//! A set of [triple]s forms an RDF [graph].
//! Finally, several [graph]s can be grouped in a collection
//! called a [dataset], where each [graph] is identified by a unique name.
//!
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/
//! [triple]: triple/index.html
//! [term]: term/index.html
//! [graph]: graph/index.html
//! [dataset]: dataset/index.html
//!
//! # Generalized vs. Strict RDF model
//!
//! The data model supported by this crate is in fact
//! a superset of the RDF data model as defined by the W3C.
//! When the distinction matters,
//! they will be called, respectively,
//! the *generalized* RDF model, and the *strict* RDF model.
//!
//! # Getting Started
//!
//! Following a short example how to build a graph, mutate it and serialize it
//! back.
//!
//! ```
//! use sophia::graph::{*, inmem::LightGraph};
//! use sophia::parser;
//! use sophia::serializer::*;
//! use sophia::serializer::nt::NtSerializer;
//! use sophia::triple::stream::TripleSource;
//! use sophia_term::{Term, iri::Iri};
//!
//! let example = r#"
//!     @prefix : <http://example.org/>.
//!     @prefix foaf: <http://xmlns.com/foaf/0.1/>.
//!
//!     :alice foaf:name "Alice";
//!            foaf:mbox <mailto:alice@work.example> .
//!
//!     :bob foaf:name "Bob".
//! "#;
//! let mut graph = LightGraph::new();
//! parser::turtle::parse_str(example).in_graph(&mut graph);
//!
//! let ex = Iri::<&str>::new("http://example.org/").unwrap();
//! let foaf = Iri::<&str>::new("http://xmlns.com/foaf/0.1/").unwrap();
//! let bob = Term::from(ex.with_suffix("bob").unwrap());
//! let knows = Term::from(foaf.with_suffix("known").unwrap());
//! let alice = Term::from(ex.with_suffix("alice").unwrap());
//! graph.insert(&bob, &knows, &alice).unwrap();
//!
//! let mut nt_stringifier = NtSerializer::new_stringifier();
//! let example2 = nt_stringifier.serialize_graph(&mut graph).unwrap().as_str();
//! println!("The resulting graph\n{}", example2);
//! ```

pub mod dataset;
pub mod graph;
pub mod parser;
pub mod quad;
pub mod query;
pub mod serializer;
pub mod triple;

/// See [`sophia_term::ns`](https://docs.rs/sophia_term/latest/sophia_term/ns/index.html)
pub mod ns {
    pub use sophia_term::ns::*;
}
/// See [`sophia_term`](https://docs.rs/sophia_term/latest/sophia_term/)
pub mod term {
    pub use sophia_term::*;
}
