//! This crate provides functions for validating IRIs and IRI references,
//! as well as for resolving IRI references against a given base IRI.
//!
//! It is developed as a part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust,
//! but can be used independently.
//!
//! # Feature gates
//!
//! - **test_data** exposes the [`test`](`mod@test`) module,
//!   which contains arrays of good and bad IRIs,
//!   useful for testing purposes, possibly in other crates.
//!
//! - **examples** exposes the [`wrap_macro_examples`] module,
//!   whose role is to exemplify the effect of the [`wrap`] macro.
//!
//! [Sophia]: https://docs.rs/sophia/latest/sophia/
//! [RDF]: https://www.w3.org/TR/rdf-primer/
//! [Linked Data]: http://linkeddata.org/

#![deny(missing_docs)]

mod _wrap_macro;

mod _error;
pub use _error::*;
mod _regex;
pub use self::_regex::*;
mod _trait;
pub use self::_trait::*;
mod _wrapper;
pub use _wrapper::*;
#[cfg(feature = "serde")]
mod _serde;

pub mod resolve;

#[cfg(feature = "examples")]
pub mod wrap_macro_examples;

#[cfg(any(test, feature = "test_data"))]
pub mod test;

#[cfg(not(feature = "threadsafe_err"))]
pub use std::error::Error;
#[cfg(feature = "threadsafe_err")]
pub use ThreadSafeError as Error;

///! An error trait meant to enable sending errors safely across threads.
pub trait ThreadSafeError: std::error::Error + Send + Sync + 'static {}
impl<E> ThreadSafeError for E where E: std::error::Error + Send + Sync + 'static {}
