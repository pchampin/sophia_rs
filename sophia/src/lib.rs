//! This meta-crate aims to provide a comprehensive toolkit
//! for working with [RDF] and [Linked Data] in Rust.
//!
//! It provides a unified access to a number of smaller crates,
//! that make the Sophia toolkit:
//!
//! * [`api`]
//! * [`inmem`]
//! * [`iri`]
//! * [`isomorphism`]
//! * [`turtle`]
//! * [`xml`] (with the `xml` feature enabled)
//!
//! # Getting Started
//!
//! Following a short example how to build a graph, mutate it and serialize it
//! back.
//!
//! ```
//! use sophia::api::prelude::*;
//! use sophia::api::ns::Namespace;
//! use sophia::inmem::graph::LightGraph;
//! use sophia::turtle::parser::turtle;
//! use sophia::turtle::serializer::nt::NtSerializer;
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
//! let mut graph: LightGraph = turtle::parse_str(example).collect_triples()?;
//!
//! let ex = Namespace::new("http://example.org/")?;
//! let foaf = Namespace::new("http://xmlns.com/foaf/0.1/")?;
//! graph.insert(
//!     ex.get("bob")?,
//!     foaf.get("knows")?,
//!     ex.get("alice")?,
//! )?;
//!
//! let mut nt_stringifier = NtSerializer::new_stringifier();
//! let example2 = nt_stringifier.serialize_graph(&graph)?.as_str();
//! println!("The resulting graph:\n{}", example2);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

pub use sophia_api as api;
pub use sophia_c14n as c14n;
pub use sophia_inmem as inmem;
pub use sophia_iri as iri;
pub use sophia_isomorphism as isomorphism;
pub use sophia_turtle as turtle;
#[cfg(feature = "xml")]
pub use sophia_xml as xml;
