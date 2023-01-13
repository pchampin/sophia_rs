//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! This crate provides function to canonicalize graphs and datasets.
//!
//! It currently implements the [`urdna2015`] algorithm.
//!
//! TODO list:
//! - [ ] check that UTF-8 byte-by-byte ordering is indeed equivalent to code point ordering.
//!       If not, fix all the places with a 'FIX? code point ordering' comment
//! - [ ] use c14n in sophia_isomorphism, replacing the current incomplete algorithm

#![deny(missing_docs)]

mod _c14n_term;
mod _cnq;
mod _permutations;

pub mod urdna2015;

use thiserror::Error;

/// Canonicalization error.
#[derive(Debug, Error)]
pub enum C14nError<E> {
    /// The dataset raised an error during canonicalization
    Dataset(#[from] E),
    /// An IO error occurrend while writing the normalized form
    Io(std::io::Error),
    /// The graph was deemed too complex by the configured safeguards of the algorithm
    ToxicGraph(String),
    /// The c14n algorithm does not support this dataset
    Unsupported(String),
}
