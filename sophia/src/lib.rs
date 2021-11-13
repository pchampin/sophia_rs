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
//! extern crate sophia;
//! 
//! use sophia::graph::{*, inmem::FastGraph};
//! use sophia::ns::Namespace;
//! use sophia::parser::turtle;
//! use sophia::serializer::*;
//! use sophia::serializer::nt::NtSerializer;
//! use sophia::triple::stream::TripleSource;
//! 
//! fn main() -> Result<(), Box<dyn std::error::Error>> {
//! 
//!     let example = r#"
//!         @prefix : <http://example.org/>.
//!         @prefix foaf: <http://xmlns.com/foaf/0.1/>.
//! 
//!         :alice foaf:name "Alice";
//!             foaf:mbox <mailto:alice@work.example> .
//! 
//!         :bob foaf:name "Bob".
//!     "#;
//!     let mut graph: FastGraph = turtle::parse_str(example).collect_triples()?;
//! 
//!     let ex = Namespace::new("http://example.org/")?;
//!     let foaf = Namespace::new("http://xmlns.com/foaf/0.1/")?;
//!     graph.insert(
//!         &ex.get("bob")?,
//!         &foaf.get("knows")?,
//!         &ex.get("alice")?,
//!     )?;
//! 
//!     let mut nt_stringifier = NtSerializer::new_stringifier();
//!     let example2 = nt_stringifier.serialize_graph(&mut graph)?.as_str();
//!     println!("The resulting graph\n{}", example2);
//! 
//!     Ok::<(), Box<dyn std::error::Error>>(())
//! }
//! ```

#![deny(missing_docs)]

pub mod query;

/// This module re-exports symbols from
/// [`sophia_api::dataset`], [`sophia_indexed::dataset`] and [`sophia_inmem::dataset`].
pub mod dataset {
    pub use sophia_api::dataset::*;
    pub use sophia_indexed::dataset as indexed;
    pub use sophia_inmem::dataset as inmem;
}
/// This module re-exports symbols from
/// [`sophia_iri`].
pub mod iri {
    pub use sophia_iri::*;
}
/// This module re-exports symbols from
/// [`sophia_api::graph`], [`sophia_indexed::graph`] and [`sophia_inmem::graph`].
pub mod graph {
    pub use sophia_api::graph::*;
    pub use sophia_indexed::graph as indexed;
    pub use sophia_inmem::graph as inmem;
}
/// This module re-exports symbols from
/// [`sophia_api::ns`].
pub mod ns {
    pub use sophia_api::ns::*;
}
/// This module re-exports symbols from
/// [`sophia_api::parser`] and [`sophia_turtle::parser`].
/// If the `xml` feature is enabled, it also re-exports
/// [`sophia_xml::parser`] as [`xml`](crate::parser::xml).
pub mod parser {
    pub use sophia_api::parser::*;
    pub use sophia_turtle::parser::gtrig;
    pub use sophia_turtle::parser::nq;
    pub use sophia_turtle::parser::nt;
    pub use sophia_turtle::parser::trig;
    pub use sophia_turtle::parser::turtle;
    #[cfg(feature = "xml")]
    pub use sophia_xml::parser as xml;
    #[deprecated(since = "0.7.0", note = "please use `sophia_xml` instead")]
    #[cfg(feature = "xml")]
    pub mod xml_legacy;
}
/// This module re-exports symbols from
/// [`sophia_api::prefix`].
pub mod prefix {
    pub use sophia_api::prefix::*;
}
/// This module re-exports symbols from
/// [`sophia_api::quad`].
pub mod quad {
    pub use sophia_api::quad::*;
}
/// This module re-exports symbols from
/// [`sophia_api::serializer`] and [`sophia_turtle::serializer`].
/// If the `xml` feature is enabled, it also re-exports
/// [`sophia_xml::serializer`] as [`xml`](crate::serializer::xml).
pub mod serializer {
    pub use sophia_api::serializer::*;
    pub use sophia_turtle::serializer::nq;
    pub use sophia_turtle::serializer::nt;
    pub use sophia_turtle::serializer::trig;
    pub use sophia_turtle::serializer::turtle;
    #[cfg(feature = "xml")]
    pub use sophia_xml::serializer as xml;
}
/// This module re-exports symbols from
/// [`sophia_api::sparql`].
pub mod sparql {
    pub use sophia_api::sparql::*;
}
/// This module re-exports symbols from
/// This module re-exports symbols from
/// [`sophia_api::term`]
/// and
/// [`sophia_term`].
pub mod term {
    pub use sophia_api::term::*;
    pub use sophia_term::*;
}
/// This module re-exports symbols from
/// [`sophia_api::triple`].
pub mod triple {
    pub use sophia_api::triple::*;
}
