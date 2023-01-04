//! RDF-related languages (e.g. Turtle, SPARQL) often use prefixes to shorten IRIs.
//! This crate provides generic traits and types to handle prefix maps.

mod _error;
pub use _error::*;
mod _prefix_map;
pub use _prefix_map::*;
mod _regex;
pub use _regex::*;
mod _trait;
pub use _trait::*;
mod _wrapper;
pub use _wrapper::*;
