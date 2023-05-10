//! This crate defines the type [`Resource`] and other related types.

mod _iter;
pub use _iter::*;
mod _typed;
pub use _typed::*;
mod _error;
pub use _error::*;
mod _struct;
pub use _struct::*;

#[cfg(test)]
mod test;
