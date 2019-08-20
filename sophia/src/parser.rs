//! Parsers for standard RDF syntaxes,
//! and tools for building new parsers.
//!
//! # Uniform interface
//!
//! Each parser module defines a `Config` type, that
//! - implements [`Default`],
//! - has three methods `parse_bufread`, `parse_read` and `parse_str`,
//!   accepting [`io::BufRead`], [`io::Read`] and [`&str`] respectively,
//!   and all returning a [`TripleSource`] or [`QuadSource`] iterator.
//!
//! Each parser module also has three functions
//! `parse_bufread`, `parse_read` and `parse_str`,
//! calling the corresponding methods from the default `Config`.
//!
//! [`Default`]: https://doc.rust-lang.org/std/default/trait.Default.html
//! [`io::BufRead`]: https://doc.rust-lang.org/std/io/trait.BufRead.html
//! [`io::Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
//! [`&str`]: https://doc.rust-lang.org/std/primitive.str.html
//! [`TripleSource`]: ../triple/stream/trait.TripleSource.html
//! [`QuadSource`]: ../quad/stream/trait.QuadSource.html

#[macro_use]
pub mod common;
pub mod nq;
pub mod nt;
#[cfg(feature = "rio")]
pub mod rio_common;
#[cfg(feature = "rio")]
pub mod rio_trig;
#[cfg(feature = "rio")]
pub mod rio_turtle;
#[cfg(feature = "xml")]
pub mod xml;
