//! In-memory implementations of RDF datasets.
//!
//! This module provides [building blocks](#structs)
//! for defining implementations of [`Dataset`] and [`MutableDataset`],
//! with fine-tuned trade-offs between memory footprint and performance.
//!
//! It also provides two pre-defined trade-offs:
//! [`FastDataset`] and [`LightDataset`],
//! provided in different flavours
//! ([default](#types), [`small`](small/index.html), [`sync`](sync/index.html)).
//!
//! This module is the twin of the [`graph.inmem`](../../graph/inmem/index.html) module.
//! See its documentation for more details.
//!
//! [`Dataset`]: ../trait.Dataset.html
//! [`MutableDataset`]: ../trait.MutableDataset.html
//! [`FastDataset`]: type.FastDataset.html
//! [`LightDataset`]: type.LightDataset.html

use super::_traits::*;
use super::indexed::*;
use crate::graph::inmem::TermIndexMapU;
use crate::term::{factory::*, *};

#[macro_use]
mod _wrapper;
pub use self::_wrapper::*;
mod _hash_dataset;
pub use self::_hash_dataset::*;
mod _gspo_wrapper;
pub use self::_gspo_wrapper::*;
mod _ogps_wrapper;
pub use self::_ogps_wrapper::*;

/// A generic in-memory dataset.
///
/// `I` must be a type for which [`TermIndexMapU`](../../graph/inmem/struct.TermIndexMapU.html)
/// implements [`TermIndexMap`](../../graph/index/trait.TermIndexMap.html),
/// typically `u16` or `u32`.
///
/// `F` must implement [`TermFactory`](../../term/factory/trait.TermFactory.html).
///
pub type GenericDataset<I, F> = HashDataset<TermIndexMapU<I, F>>;

type FastWrapper<T> = OgpsWrapper<GspoWrapper<T>>;

/// A heavily indexed dataset.
/// Fast to query but slow to load, with a relatively high memory footprint.
pub type FastDataset = FastWrapper<GenericDataset<u32, RcTermFactory>>;

/// A dataset with no triple index.
/// Fast to load but slow to query, with a relatively low memory footprint.
pub type LightDataset = GenericDataset<u32, RcTermFactory>;

#[cfg(test)]
test_dataset_impl!(test_fastd, FastDataset);

#[cfg(test)]
test_dataset_impl!(test_lightd, LightDataset);

/// Flavours of Dataset implementations with a smaller memory-footprint.
///
/// The trade-off is that these implementations can only contain a small number (2^16) of terms.
///
pub mod small {
    use super::*;

    /// A heavily indexed dataset.
    /// Fast to query but slow to load, with a relatively high memory footprint.
    pub type FastDataset = FastWrapper<GenericDataset<u16, RcTermFactory>>;
    /// A dataset with no triple index.
    /// Fast to load but slow to query, with a relatively low memory footprint.
    pub type LightDataset = GenericDataset<u16, RcTermFactory>;

    #[cfg(test)]
    test_dataset_impl!(test_fastd, FastDataset);
    #[cfg(test)]
    test_dataset_impl!(test_lightd, LightDataset);
}

/// Flavours of Dataset implementations which are safe to share across threads.
pub mod sync {
    use super::*;

    /// A heavily indexed dataset.
    /// Fast to query but slow to load, with a relatively high memory footprint.
    pub type FastDataset = FastWrapper<GenericDataset<u32, ArcTermFactory>>;
    /// A dataset with no triple index.
    /// Fast to load but slow to query, with a relatively low memory footprint.
    pub type LightDataset = GenericDataset<u32, ArcTermFactory>;

    #[cfg(test)]
    test_dataset_impl!(test_fastd, FastDataset);
    #[cfg(test)]
    test_dataset_impl!(test_lightd, LightDataset);
}
