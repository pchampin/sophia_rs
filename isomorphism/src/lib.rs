//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! This crate provides function to check if two graphs (resp. two datasets)
//! are [isomorphic]
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf12-primer/
//! [Linked Data]: http://linkeddata.org/
//! [isomorphic]: https://www.w3.org/TR/rdf12-concepts/#graph-isomorphism
#![deny(missing_docs)]

mod dataset;
mod graph;
mod iso_term;

pub use dataset::isomorphic_datasets;
pub use graph::isomorphic_graphs;

#[cfg(test)]
mod test;
