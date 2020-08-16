//! Serializer and parser for the [JSON-LD] concrete syntax of RDF.
//!
//! NB: this implementation only supports the [expanded document form] of [JSON-LD].
//!
//! NB2: currently, only the serializer part is implemented.
//!
//! [JSON-LD]: https://www.w3.org/TR/json-ld11/
//! [expanded document form]: https://www.w3.org/TR/json-ld11/#expanded-document-form

#![deny(missing_docs)]

pub mod config;
pub use config::*;
pub mod error;
pub use error::*;
pub mod serializer;
pub use serializer::*;
mod util_traits;

#[cfg(any(test, feature = "test_util"))]
pub mod test_util;
