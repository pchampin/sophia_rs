//! An RDF graph, the central notion of the RDF data model,
//! is a set of triples.

#[cfg(test)]
#[macro_use]
mod test;

#[macro_use]
pub mod index;
pub mod inmem;

mod traits; pub use self::traits::*;
mod ext_impl; pub use self::ext_impl::*;
