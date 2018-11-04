//! An RDF graph, the central notion of the RDF data model,
//! is a collection of triples.
//!
//! This module provides [reusable abstractions](#traits)
//! for different kinds of graph,
//! as well as a few implementations for them.

#[cfg(test)]
#[macro_use]
mod test;

#[macro_use]
pub mod index;
pub mod inmem;

mod ext_impl; pub use self::ext_impl::*;
mod sinks; pub use self::sinks::*;
mod traits; pub use self::traits::*;
