//! An RDF graph, the central notion of the RDF data model,
//! is a set of triples.

pub mod index;
pub mod inmem;

mod traits; pub use self::traits::*;
mod ext_impl; pub use self::ext_impl::*;

#[cfg(test)] mod test;

pub use self::inmem::SimpleGraph;