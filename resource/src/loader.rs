//! This create defies the [`Loader`] trait and some implementations.
mod _error;
pub use _error::*;
mod _local;
pub use _local::*;
mod _no;
pub use _no::*;
mod _trait;
pub use _trait::*;

mod util;

#[cfg(test)]
mod test;
