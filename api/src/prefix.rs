//! RDF-related languages (e.g. Turtle, SPARQL) often use prefixes to shorten IRIs.
//! This crate provides generic traits and types to handle prefix maps.

mod _prefix;
pub use _prefix::*;
mod _prefix_map;
pub use _prefix_map::*;
