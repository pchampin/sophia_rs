//! API for parsing RDF syntaxes.

use crate::quad::stream::QuadSource;
use crate::triple::stream::TripleSource;

mod _location;
pub use _location::*;

/// A triple parser takes some data of type `T`,
/// and returns a [`TripleSource`].
///
/// [`TripleSource`]: ../triple/stream/trait.TripleSource.html
pub trait TripleParser<T> {
    /// The source produced by this parser
    type Source: TripleSource;

    /// Parses data into a triple source.
    fn parse(&self, data: T) -> Self::Source;

    /// Convenient shortcut method for parsing strings.
    ///
    /// It may not be available on some exotic parsers,
    /// but will be automatically supported for parsers supporting any
    /// [`BufRead`] or [`Read`].
    ///
    /// [`BufRead`]: https://doc.rust-lang.org/std/io/trait.BufRead.html
    /// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
    fn parse_str<'t>(&self, txt: &'t str) -> Self::Source
    where
        &'t str: IntoParsable<Target = T>,
    {
        self.parse(txt.into_parsable())
    }
}

/// A quad parser takes some data of type `T`,
/// and returns a [`QuadSource`].
///
/// [`QuadSource`]: ../quad/stream/trait.QuadSource.html
pub trait QuadParser<T> {
    /// The source produced by this parser
    type Source: QuadSource;

    /// Parses data into a quad source.
    fn parse(&self, data: T) -> Self::Source;

    /// Convenient shortcut method for parsing strings.
    ///
    /// It may not be available on some exotic parsers,
    /// but will be automatically supported for parsers supporting any
    /// [`BufRead`] or [`Read`].
    ///
    /// [`BufRead`]: https://doc.rust-lang.org/std/io/trait.BufRead.html
    /// [`Read`]: https://doc.rust-lang.org/std/io/trait.Read.html
    fn parse_str<'t>(&self, txt: &'t str) -> Self::Source
    where
        &'t str: IntoParsable<Target = T>,
    {
        self.parse(txt.into_parsable())
    }
}

/// Utility trait to support [`TripleParser::parse_str`] and [`QuadParser::parse_str`].
///
/// [`TripleParser::parse_str`]: trait.TripleParser.html#method.parse_str
/// [`QuadParser::parse_str`]: trait.QuadParser.html#method.parse_str
pub trait IntoParsable {
    type Target;
    fn into_parsable(self) -> Self::Target;
}
impl<'a> IntoParsable for &'a str {
    type Target = &'a [u8];
    fn into_parsable(self) -> Self::Target {
        self.as_bytes()
    }
}

/// Define convenience module-level functions for a parser implementation supporting BufRead.
#[macro_export]
macro_rules! def_mod_functions_for_bufread_parser {
    ($parser_type: ident, $parser_trait: ident) => {
        /// Convenience function for parsing a BufRead with the default parser.
        pub fn parse_bufread<B: std::io::BufRead>(
            bufread: B,
        ) -> <$parser_type as $crate::parser::$parser_trait<B>>::Source {
            $parser_type::default().parse(bufread)
        }

        /// Convenience function for parsing a str with the default parser.
        pub fn parse_str(
            txt: &str,
        ) -> <$parser_type as $crate::parser::$parser_trait<&[u8]>>::Source {
            $parser_type::default().parse_str(txt)
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
