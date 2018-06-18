//! This crate aims to provide a comprehensive toolkit
//! for working with [RDF](https://www.w3.org/TR/rdf-primer/)
//! and [Linked Data](http://linkeddata.org/) in Rust.
//!
// TODO: flesh out one or several example(s) of code

#![feature(map_get_key_value)]

extern crate language_tag;
#[macro_use] extern crate lazy_static;
extern crate pest;
#[macro_use] extern crate pest_derive;
extern crate regex;
extern crate url;
extern crate weak_table;

pub mod graph;
pub mod ns;
pub mod term;