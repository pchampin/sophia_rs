//! An RDF graph, the central notion of the RDF data model,
//! is a collection of triples.
//!
//! This module provides [reusable abstractions](#traits)
//! for different kinds of graph,
//! as well as a few implementations for them.

#[cfg(any(test, feature = "test_macro"))]
#[macro_use]
pub mod test;

mod _ext_impl;
pub use self::_ext_impl::*;
mod _traits;
pub use self::_traits::*;
mod _isomorphism;
pub use self::_isomorphism::*;
