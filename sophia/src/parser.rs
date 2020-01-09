//! API for parsing RDF syntaxes.

use crate::quad::stream::QuadSource;
use crate::triple::stream::TripleSource;

mod _location;
pub use _location::*;

/// A generic parser takes some data of type `T`,
/// and returns a [`TripleSource`] or a [`QuadSource`].
///
/// See also [`TripleParser`] and [`QuadParser`].
///
/// [`TripleParser`]: trait.TripleParser.html
/// [`TripleSource`]: ../triple/stream/trait.TripleSource.html
/// [`QuadParser`]: trait.QuadParser.html
/// [`QuadSource`]: ../quad/stream/trait.QuadSource.html
pub trait Parser<T> {
    /// The source produced by this parser, generally
    /// [`TripleSource`] or [`QuadSource`].
    ///
    /// [`TripleSource`]: ../triple/stream/trait.TripleSource.html
    /// [`QuadSource`]: ../quad/stream/trait.QuadSource.html
    type Source;

    /// The central method of `Parser`: parses data into a (triple or quad) source.
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

/// Specialization of [`Parser`] that returns a [`QuadSource`].
/// It also constrains the returned source's error to implement [`WithLocation`].
///
/// [`Parser`]: trait.Parser.html
/// [`QuadSource`]: ../quad/stream/trait.QuadSource.html
/// [`WithLocation`]: trait.WithLocation.html
pub trait QuadParser<T>: Parser<T> {}
impl<T, P> QuadParser<T> for P
where
    P: Parser<T>,
    <P as Parser<T>>::Source: QuadSource,
    <<P as Parser<T>>::Source as QuadSource>::Error: WithLocation,
{
}

/// Specialization of [`Parser`] that returns a [`QuadSource`].
/// It also constrains the returned source's error to implement [`WithLocation`].
///
/// [`Parser`]: trait.Parser.html
/// [`QuadSource`]: ../quad/stream/trait.QuadSource.html
/// [`WithLocation`]: trait.WithLocation.html
pub trait TripleParser<T>: Parser<T> {}
impl<T, P> TripleParser<T> for P
where
    P: Parser<T>,
    <P as Parser<T>>::Source: TripleSource,
    <<P as Parser<T>>::Source as TripleSource>::Error: WithLocation,
{
}

/// Utility trait to support [`Parser::parse_str`].
///
/// [`Parser::parse_str`]: trait.Parser.html#method.parse_str
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
    ($parser_type: ident) => {
        /// Convenience function for parsing a BufRead with the default parser.
        pub fn parse_bufread<B: std::io::BufRead>(
            bufread: B,
        ) -> <$parser_type as $crate::parser::Parser<B>>::Source {
            $parser_type::default().parse(bufread)
        }

        /// Convenience function for parsing a str with the default parser.
        pub fn parse_str(txt: &str) -> <$parser_type as $crate::parser::Parser<&[u8]>>::Source {
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
