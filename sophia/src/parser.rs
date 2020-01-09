//! Parsers for standard RDF syntaxes,
//! and tools for building new parsers.
//!
//! # Uniform interface
//!
//! Each parser module defines a `Config` type, that
//! - implements [`Default`],
//! - has three methods `parse_bufread`, `parse_read` and `parse_str`,
//!   accepting [`io::BufRead`], [`io::Read`] and [`&str`] respectively,
//!   and all returning a [`TripleSource`] or [`QuadSource`].
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

/// This macro provides a straightforward implementation of the default functions
/// of a parser module producing triples.
#[macro_export]
macro_rules! def_default_triple_parser_api {
    () => {
        def_default_parser_api!($crate::triple::stream::TripleSource);
    };
}

/// This macro provides a straightforward implementation of the default functions
/// of a parser module producing quads.
#[macro_export]
macro_rules! def_default_quad_parser_api {
    () => {
        def_default_parser_api!($crate::quad::stream::QuadSource);
    };
}

macro_rules! def_default_parser_api {
    ($item: path) => {
        /// Shortcut for `Config::default().parse_bufread(bufread)`
        #[inline]
        pub fn parse_bufread<'a, B: ::std::io::BufRead + 'a>(bufread: B) -> impl $item + 'a {
            Config::default().parse_bufread(bufread)
        }
        /// Shortcut for `Config::default().parse_read(read)`
        #[inline]
        pub fn parse_read<'a, R: ::std::io::Read + 'a>(read: R) -> impl $item + 'a {
            Config::default().parse_read(read)
        }
        /// Shortcut for `Config::default().parse_str(txt)`
        #[inline]
        pub fn parse_str<'a>(txt: &'a str) -> impl $item + 'a {
            Config::default().parse_str(txt)
        }
    };
}

pub mod gtrig;
pub mod nq;
pub mod nt;
pub mod rio_common;
pub mod trig;
pub mod turtle;
#[cfg(feature = "xml")]
pub mod xml;
