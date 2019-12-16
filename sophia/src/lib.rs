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
//! use sophia::ns::Namespace;
//! use sophia::parser;
//! use sophia::serializer;
//! use sophia::serializer::TripleStringifier;
//! use sophia::triple::stream::{TripleSink, TripleSource};
//!
//! let example = r#"
//!     <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
//!     <http://example.org/alice> <http://xmlns.com/foaf/0.1/mbox> <mailto:alice@work.example> .
//!
//!     <http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
//! "#;
//! let mut graph = LightGraph::new();
//! parser::nt::parse_str(example).in_graph(&mut graph);
//!
//! let ex = Namespace::new("http://example.org/").unwrap();
//! let foaf = Namespace::new("http://xmlns.com/foaf/0.1/").unwrap();
//! let bob = ex.get("bob").unwrap();
//! let knows = foaf.get("known").unwrap();
//! let alice = ex.get("alice").unwrap();
//! graph.insert(&bob, &knows, &alice).unwrap();
//!
//! let mut nt_stringifier = serializer::nt::stringifier();
//! let example2 = nt_stringifier.stringify_graph(&mut graph).unwrap();
//! println!("The resulting graph\n{}", example2);
//! ```

// error_chain is recursing a lot
#![recursion_limit = "256"]

#[macro_use]
extern crate error_chain;
extern crate language_tag;
#[macro_use]
extern crate lazy_static;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate regex;
#[macro_use]
extern crate rental;
#[cfg(feature = "xml")]
extern crate quick_xml;
extern crate resiter;
#[cfg(feature = "rio")]
extern crate rio_api;
#[cfg(feature = "rio")]
extern crate rio_turtle;
extern crate url;
extern crate weak_table;

pub mod dataset;
pub mod error;
pub mod graph;
pub mod ns;
pub mod parser;
pub mod quad;
pub mod query;
pub mod serializer;
pub mod term;
pub mod triple;
