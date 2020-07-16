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
//! use sophia::graph::{*, inmem::FastGraph};
//! use sophia::ns::Namespace;
//! use sophia::parser::turtle;
//! use sophia::serializer::*;
//! use sophia::serializer::nt::NtSerializer;
//! use sophia::triple::stream::TripleSource;
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
//! let mut graph: FastGraph = turtle::parse_str(example).collect_triples()?;
//!
//! let ex = Namespace::new("http://example.org/")?;
//! let foaf = Namespace::new("http://xmlns.com/foaf/0.1/")?;
//! graph.insert(
//!     &ex.get("bob")?,
//!     &foaf.get("knows")?,
//!     &ex.get("alice")?,
//! )?;
//!
//! let mut nt_stringifier = NtSerializer::new_stringifier();
//! let example2 = nt_stringifier.serialize_graph(&mut graph)?.as_str();
//! println!("The resulting graph\n{}", example2);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

pub mod query;
pub mod serializer;

pub mod dataset {
    pub use sophia_api::dataset::*;
    pub mod indexed;
    pub mod inmem;
}
pub mod graph {
    pub use sophia_api::graph::*;
    pub mod indexed;
    pub mod inmem;
}
pub mod ns {
    pub use sophia_api::ns::*;
}
pub mod parser {
    pub use sophia_api::parser::*;
    pub mod gtrig;
    pub mod nq;
    pub mod nt;
    pub mod rio_common;
    pub mod trig;
    pub mod turtle;
    #[cfg(feature = "xml")]
    pub mod xml;
}
pub mod quad {
    pub use sophia_api::quad::*;
}
pub mod term {
    pub use sophia_term::*;
}
pub mod triple {
    pub use sophia_api::triple::*;
}
