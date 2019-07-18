//! An RDF graph, the central notion of the RDF data model,
//! is a collection of triples.
//!
//! This module provides [reusable abstractions](#traits)
//! for different kinds of graph,
//! as well as a few implementations for them.

#[cfg(test)]
#[macro_use]
pub mod test;

pub mod adapter;
#[macro_use]
pub mod indexed;
pub mod inmem;

mod _ext_impl;
pub use self::_ext_impl::*;
mod _sinks;
pub use self::_sinks::*;
mod _traits;
pub use self::_traits::*;
