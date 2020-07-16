//! This crate provides a foundation,
//! as a set of traits and core types,
//! for building interoperable implementations of [RDF] and [Linked Data] in Rust.
//!
//! For an all-included crate
//! (providing actual implementations of the traits defined here),
//! see [`sophia`](https://docs.rs/sophia/latest/sophia/).
//!
//! # RDF
//!
//! [RDF] is a data model
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

pub mod dataset;
pub mod graph;
pub mod ns;
pub mod parser;
pub mod quad;
pub mod serializer;
pub mod term;
pub mod triple;
