//! This crate provides a foundation,
//! as a set of traits and core types,
//! for building interoperable implementations of [RDF] and [Linked Data] in Rust.
//!
//! For an all-inclusive crate
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
//!
//! # Generalized vs. Strict RDF model
//!
//! The data model supported by this crate is in fact
//! a superset of the RDF data model as defined by the W3C.
//! When the distinction matters,
//! they will be called, respectively,
//! the *generalized* RDF model, and the *strict* RDF model.
//!
//! The generalized RDF model extends RDF as follows:
//!
//! * In addition to standard RDF terms (IRIs, blank nodes and literals),
//!   Sophia supports
//!
//!   - RDF-star [quoted triples](https://www.w3.org/2021/12/rdf-star.html#dfn-quoted)
//!   - Variables (a concept borrowed from [SPARQL] or [Notation3])
//!
//! * Sophia allows any kind of term in any position (subject, predicate, object, graph name).
//!
//! * Sophia allow IRIs to be relative IRI references
//!   (while in strict RDF, [IRIs must be absolute](https://www.w3.org/TR/rdf11-concepts/#h3_section-IRIs)).
//!
//! # Feature gates
//!
//! - **test_macros**: with this feature enabled,
//!   this crate exposes macros that can help implementors of the API to test their implementation.
//!
//! [SPARQL]: https://www.w3.org/TR/sparql11-query/
//! [Notation3]: https://www.w3.org/TeamSubmission/n3/

pub mod dataset;
pub mod graph;
pub mod ns;
pub mod parser;
pub mod prefix;
pub mod prelude;
pub mod quad;
pub mod serializer;
pub mod source;
pub mod sparql;
pub mod term;
pub mod triple;

/// Re-export MownStr to avoid dependency version mismatch.
pub use mownstr::MownStr;
