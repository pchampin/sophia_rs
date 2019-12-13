//! An RDF dataset is composed of a default graph,
//! and zero or more named graphs, each associated with a graph name.
//!
//! Another way to look at it is as a collection of [quads](../quad/index.html).
//!
//! This module provides [reusable abstractions](#traits)
//! for different kinds of datasets,
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

mod _error;
pub use self::_error::*;
