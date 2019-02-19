//! This crate aims to provide a comprehensive toolkit
//! for working with [RDF](https://www.w3.org/TR/rdf-primer/)
//! and [Linked Data](http://linkeddata.org/) in Rust.
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

#[macro_use] extern crate coercible_errors;
#[macro_use] extern crate error_chain;
extern crate language_tag;
#[macro_use] extern crate lazy_static;
extern crate pest;
#[macro_use] extern crate pest_derive;
extern crate regex;
#[macro_use] extern crate rental;
extern crate resiter;
extern crate url;
extern crate weak_table;

pub mod error;
pub mod graph;
pub mod ns;
pub mod parsers;
pub mod query;
pub mod serializers;
pub mod streams;
pub mod term;
pub mod triple;
