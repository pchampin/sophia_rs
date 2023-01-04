//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! This crate provides function to check if two graphs (resp. two datasets)
//! are [isomorphic]
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/
//! [isomorphic]: https://www.w3.org/TR/rdf11-concepts/#h3_graph-isomorphism
//!
//! # Accuracy
//!
//! If `g1` and `g2` are isomorphic, the function will always return `true`.
//!
//! If they are not isomorphic, the function will generally return `false`,
//! but a few pathological cases may be falses positives
//! (*i.e.* recognized as isomorphic while they are not).
//!
//! For example, the graph:
//!
//! ```turtle
//!     _:a :rel _:b.
//!     _:b :rel _:a.
//!     _:c :rel _:c.
//! ```
//!
//! and the graph:
//!
//! ```turtle
//!     _:a :rel _:b.
//!     _:b :rel _:c.
//!     _:c :rel _:a.
//! ```
//!
//! are considered isomorphic by the algorithm of this crate,
//! because they have the same number of blank nodes and arcs,
//! and all of their blank nodes are locally indistinguisable
//! (same number of incoming and outgoinc arcs,
//! linking them to undistinguishable blank nodes).
//!
//! Correctly answering in this kind of pathological case requires a combinatorial exploration
//! of all possible bnode-pairings, which would make the algorithm very slow in the worst case.
//!
//! The choice has been made to accept this flaw,
//! as such undistinguishable blank nodes are very rare in real data,
//! and not particularly useful.
mod dataset;
mod graph;
mod hash;
mod iso_term;

pub use dataset::isomorphic_datasets;
pub use graph::isomorphic_graphs;

#[cfg(test)]
mod test;
