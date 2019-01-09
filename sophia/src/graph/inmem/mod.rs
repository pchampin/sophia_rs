//! In-memory implementations of RDF graphs.
//! 
//! This module provides [building blocks](#structs)
//! for defining implementations of [`Graph`] and [`MutableGraph`],
//! with fine-tuned trade-offs between memory footprint and performance.
//! 
//! It also provides two pre-defined trade-offs:
//! [`FastGraph`] and [`SmallGraph`],
//! provided in different flavours
//! ([default](#types), [`small`](small/index.html), [`sync`](sync/index.html)).
//! 
//! # Customized trade-off
//! 
//! By combining a given core implementation with various wrappers,
//! you can easily make a Graph type with the exact trade-offs that you need.
//! 
//! For example, if one needs a small graph (less than 2^16 terms),
//! that can be exchanged across threads,
//! and whose arcs will mostly be traversed backward (from object to subject),
//! an appropriate type definition would be:
//! 
//! ```
//! use sophia::term::factory::ArcTermFactory;
//! use sophia::graph::inmem::*;
//! 
//! type MyGraph = OpsWrapper<GenericGraph<u16, ArcTermFactory>>;
//! let g = MyGraph::new();
//! ```
//! 
//! [`Graph`]: ../trait.Graph.html
//! [`MutableGraph`]: ../trait.MutableGraph.html
//! [`FastGraph`]: type.FastGraph.html
//! [`SmallGraph`]: type.SmallGraph.html

use std::borrow::Borrow;

use ::term::*;
use ::term::factory::*;
use super::index::*;
use super::traits::*;

#[macro_use]
mod wrapper; pub use self::wrapper::*;
mod hash_graph; pub use self::hash_graph::*;
mod spo_wrapper; pub use self::spo_wrapper::*;
mod ops_wrapper; pub use self::ops_wrapper::*;
mod term_index_map_u; pub use self::term_index_map_u::*;

/// A generic in-memory graph.
/// 
/// `I` must be a type for which [`TermIndexMapU`](struct.TermIndexMapU.html)
/// implements [`TermIndexMap`](../index/trait.TermIndexMap.html),
/// typically `u16` or `u32`.
/// 
/// `F` must implement [`TermFactory`](../../term/factory/trait.TermFactory.html).
/// 
pub type GenericGraph<I, F> = HashGraph<TermIndexMapU<I, F>>;

/// A heavily indexed graph.
/// Fast to query but slow to load, with a relatively high memory footprint.
pub type FastGraph = OpsWrapper<SpoWrapper<GenericGraph<u32, RcTermFactory>>>;

/// A graph with no triple index.
/// Fast to load but slow to query, with a relatively low memory footprint.
pub type LightGraph = GenericGraph<u32, RcTermFactory>;

#[cfg(test)] test_graph_impl!(test_fastg, FastGraph);
#[cfg(test)] test_graph_impl!(test_lightg, LightGraph);

/// Flavours of Graph implementations with a smaller memory-footprint.
/// 
/// The trade-off is that these implementations can only contain a small number (2^16) of terms.
/// 
pub mod small {
    use super::*;

    /// A heavily indexed graph.
    /// Fast to query but slow to load, with a relatively high memory footprint.
    pub type FastGraph = OpsWrapper<SpoWrapper<GenericGraph<u16, RcTermFactory>>>;
    /// A graph with no triple index.
    /// Fast to load but slow to query, with a relatively low memory footprint.
    pub type LightGraph = GenericGraph<u16, RcTermFactory>;

    #[cfg(test)] test_graph_impl!(test_fastg, FastGraph);
    #[cfg(test)] test_graph_impl!(test_lightg, LightGraph);
}

/// Flavours of Graph implementations which are safe to share across threads.
pub mod sync {
    use super::*;

    /// A heavily indexed graph.
    /// Fast to query but slow to load, with a relatively high memory footprint.
    pub type FastGraph = OpsWrapper<SpoWrapper<GenericGraph<u32, ArcTermFactory>>>;
    /// A graph with no triple index.
    /// Fast to load but slow to query, with a relatively low memory footprint.
    pub type LightGraph = GenericGraph<u32, ArcTermFactory>;

    #[cfg(test)] test_graph_impl!(test_fastg, FastGraph);
    #[cfg(test)] test_graph_impl!(test_lightg, LightGraph);
}

