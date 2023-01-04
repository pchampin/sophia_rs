//! API for parsing RDF syntaxes.

use crate::source::{QuadSource, TripleSource};

/// A parser takes some data of type `T`,
/// and returns a [`TripleSource`].
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
    /// [`BufRead`]: std::io::BufRead
    /// [`Read`]: std::io::Read
    fn parse_str<'t>(&self, txt: &'t str) -> Self::Source
    where
        &'t str: IntoParsable<Target = T>,
    {
        self.parse(txt.into_parsable())
    }
}

/// A parser takes some data of type `T`,
/// and returns a [`QuadSource`].
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
    /// [`BufRead`]: std::io::BufRead
    /// [`Read`]: std::io::Read
    fn parse_str<'t>(&self, txt: &'t str) -> Self::Source
    where
        &'t str: IntoParsable<Target = T>,
    {
        self.parse(txt.into_parsable())
    }
}

/// Utility trait to support [`TripleParser::parse_str`] and [`QuadParser::parse_str`].
pub trait IntoParsable {
    /// The parsable type this type can be converted to.
    type Target;
    /// Convert into the parsable target type
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
