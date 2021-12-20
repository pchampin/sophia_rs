//! This crate provides functions for validating IRIs and IRI references,
//! as well as for resolving IRI references agains a given base IRI.
//!
//! It is developed as a part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust,
//! but can be used independantly.
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/

#![deny(missing_docs)]

mod _regex;
pub use self::_regex::*;
mod _trait;
pub use self::_trait::*;
mod _wrapper;
pub use _wrapper::*;
pub mod error;
pub mod resolve;

#[cfg(any(test, feature = "test_data"))]
pub mod test;
