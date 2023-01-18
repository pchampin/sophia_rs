//! A source yields items, and may also fail in the process.
//! This module provides two specialized kinds of source:
//! [`TripleSource`] and [`QuadSource`].
//!
//! These traits provides an API similar to (a subset of) the [`Iterator`] API,
//! with methods such as [`for_each_triple`] and [`try_for_each_triple`]
//! (resp. '[`for_each_quad`] and [`try_for_each_quad`]).
//!
//! # Rationale (or Why not simply use `Iterator`?)
//!
//! The [`Iterator`] trait is designed in such a way that items must live at least as long as the iterator itself.
//! This assumption may be too strong in some situations.
//!
//! For example,
//! consider a parser using 3 buffers to store the subject, predicate,
//! and object of the triples it parses.
//! Each time it extracts a triple from the data,
//! it yields it (as 3 references to its internal buffers)
//! to the closure passed to `for_each_triple`.
//! Then, it **reuses** the buffers to store the data for the next triple,
//! and yields the new triple, as 3 references to the *same* buffers.
//!
//! Such a parser can not implement [`Iterator`],
//! because, once yielded by the iterator's `next` method,
//! an item is free to live during further iterations.
//! In particular, it can be stored in a collection,
//! and still be alive when the `next` method is called again
//! (consider for example the [`Iterator::collect`] method).
//!
//! Because many parsers (as well as other triple/quad sources)
//! will be implemented in a manner similar to that described above,
//! we have to provide a trait with *weaker assumptions*
//! on the lifetime of the yielded triples.
//!
//! The alternative would be to wrap such parsers with a layer that would *copy*
//! the data from internal buffers to fresh buffers for each triples,
//! but we do not want to impose that cost on all implementations
//! — especially when many consumers will be happy with short-lived references.
//!
//! # Why not implement a single trait `Source`?
//!
//! A cleaner design would have been to have a single trait `Source`,
//! with generic method names such as `for_each_item`.
//!
//! This proved hard to use in practice,
//! because restricting the type of items requires [higher-rank trait bounds],
//! and these bounds would have to be repeated anywhere the specialized trait is required.
//!
//! The choice was therefore to favor ease of use over purity and maintainability.
//!
//! [`for_each_triple`]: TripleSource::for_each_triple
//! [`try_for_each_triple`]: TripleSource::try_for_each_triple
//! [`for_each_quad`]: QuadSource::for_each_quad
//! [`try_for_each_quad`]: QuadSource::try_for_each_quad
//! [higher-rank trait bounds]: https://doc.rust-lang.org/nomicon/hrtb.html

use std::error::Error;

pub mod convert;
pub mod filter;
pub mod filter_map;
pub mod map;

mod _quad;
pub use _quad::*;
mod _stream_error;
pub use _stream_error::*;
mod _triple;
pub use _triple::*;

use super::*;
use std::convert::Infallible;
