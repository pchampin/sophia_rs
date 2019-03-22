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
//! # Examples
//!
// TODO: flesh out one or several example(s) of code

// error_chain is recursing a lot
#![recursion_limit = "256"]

#[macro_use]
extern crate coercible_errors;
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
extern crate resiter;
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
