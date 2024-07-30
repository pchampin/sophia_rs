//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! This crate provides function to canonicalize graphs and datasets.
//!
//! It currently implements the [RDFC-1.0](rdfc10) algorithm.
//!
//! TODO list:
//! - [x] check that UTF-8 byte-by-byte ordering is indeed equivalent to code point ordering.
//! - [ ] use c14n in sophia_isomorphism, replacing the current incomplete algorithm
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/

#![deny(missing_docs)]

mod _c14n_term;
mod _cnq;
mod _permutations;

pub mod hash;
pub mod rdfc10;

use thiserror::Error;

/// Canonicalization error.
#[derive(Debug, Error)]
pub enum C14nError<E: std::error::Error + Send + Sync + 'static> {
    /// The dataset raised an error during canonicalization
    #[error("Error from dataset: {0}")]
    Dataset(#[from] E),
    /// An IO error occurrend while writing the normalized form
    #[error("IO error: {0}")]
    Io(std::io::Error),
    /// The graph was deemed too complex by the configured safeguards of the algorithm
    #[error("Toxic graph detected: {0}")]
    ToxicGraph(String),
    /// The c14n algorithm does not support this dataset
    #[error("Unsupported feature: {0}")]
    Unsupported(String),
}

#[cfg(test)]
fn test_setup() {
    TEST_SETUP.call_once(|| {
        env_logger::init();
    });
}

#[cfg(test)]
static TEST_SETUP: std::sync::Once = std::sync::Once::new();
