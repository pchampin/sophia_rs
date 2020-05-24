//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! It provides functions for validating IRIs and IRI references,
//! as well as for resolving IRI references agains a given base IRI.
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/

#![deny(missing_docs)]

mod _regex;
pub use self::_regex::*;
pub mod error;
pub mod resolve;

#[cfg(any(test, feature = "test_data"))]
pub mod test;
