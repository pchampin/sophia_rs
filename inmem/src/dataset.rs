//! In-memory implementations of RDF datasets.
//!
//! This module provides [building blocks](#structs)
//! for defining implementations of [`Dataset`] and [`MutableDataset`],
//! with fine-tuned trade-offs between memory footprint and performance.
//!
//! It also provides two pre-defined trade-offs:
//! [`FastDataset`] and [`LightDataset`],
//! provided in different flavours
//! ([default](#types), [`small`], [`sync`]).
//!
//! This module is the twin of the [`graph`](super::graph) module.
//! See its documentation for more details.

use crate::graph::TermIndexMapU;
use sophia_api::dataset::{CollectibleDataset, Dataset, MutableDataset, SetDataset};
use sophia_indexed::dataset::*;
use sophia_term::factory::*;
use sophia_term::*;

// Symbols from other crates, re-exported for the sake of macros
pub use sophia_api::dataset::{DQuadSource, DResult, DResultTermSet, DTerm};

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
/// `I` must be a type for which [`TermIndexMapU`]
/// implements [`TermIndexMap`](sophia_term::index_map::TermIndexMap),
/// typically `u16` or `u32`.
///
/// `F` must implement [`TermFactory`].
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
sophia_api::test_dataset_impl!(test_fastd, FastDataset);

#[cfg(all(test, feature = "all_tests"))]
sophia_api::test_dataset_impl!(test_lightd, LightDataset);

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

    #[cfg(all(test, feature = "all_tests"))]
    sophia_api::test_dataset_impl!(test_fastd, FastDataset);
    #[cfg(all(test, feature = "all_tests"))]
    sophia_api::test_dataset_impl!(test_lightd, LightDataset);
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

    #[cfg(all(test, feature = "all_tests"))]
    sophia_api::test_dataset_impl!(test_fastd, FastDataset);
    #[cfg(all(test, feature = "all_tests"))]
    sophia_api::test_dataset_impl!(test_lightd, LightDataset);
}
