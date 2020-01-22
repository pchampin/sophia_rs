//! Reusable types, functions and macros for implementing serializers.

/// This macro provides a straightforward implementation of the default functions
/// of a serializer module.
#[macro_export]
macro_rules! def_default_serializer_api {
    ($writer: ident, $stringifier: ident) => {
        /// Shortcut for `Config::default().writer(write)`
        #[inline]
        pub fn writer<W: ::std::io::Write>(write: W) -> $writer<W> {
            Config::default().writer(write)
        }

        /// Shortcut for `Config::default().stringifier()`
        #[inline]
        pub fn stringifier() -> $stringifier {
            Config::default().stringifier()
        }
    };
    () => {
        def_default_serializer_api!(Writer, Stringifier);
    };
}

/// This macro provides a straightforward implementation of the `Stringifier` type,
/// based on the `Writer` type, for triples.
///
/// See also
/// [`def_quad_stringifier`](macro.def_quad_stringifier.html)
#[macro_export]
macro_rules! def_triple_stringifier {
    ($writer: ident, $stringifier: ident) => {
        /// A `TripleSink` returned by `Config::stringifier`
        pub struct $stringifier {
            writer: $writer<Vec<u8>>,
        }

        impl $crate::serializer::TripleStringifier for $stringifier {
            type Config = Config;

            fn new(config: Config) -> $stringifier {
                $stringifier {
                    writer: $writer::new(Vec::new(), config),
                }
            }
        }

        impl $crate::triple::stream::TripleSink for $stringifier {
            type Outcome = String;
            /// <Vec as io::Write> never returns an error
            type Error = std::convert::Infallible;

            fn feed<T>(&mut self, t: &T) -> std::result::Result<(), Self::Error>
            where
                T: $crate::triple::Triple,
            {
                self.writer.feed(t).map_err(|_| unreachable!())
            }

            fn finish(&mut self) -> std::result::Result<String, Self::Error> {
                let mut v = Vec::new();
                swap(&mut self.writer.write, &mut v);
                Ok(unsafe { String::from_utf8_unchecked(v) })
            }
        }
    };
    () => {
        def_triple_stringifier!(Writer, Stringifier);
    };
}

/// This macro provides a straightforward implementation of the `Stringifier` type,
/// based on the `Writer` type, for quads.
///
/// See also
/// [`def_triple_stringifier`](macro.def_triple_stringifier.html)
#[macro_export]
macro_rules! def_quad_stringifier {
    ($writer: ident, $stringifier: ident) => {
        /// A `QuadSink` returned by `Config::stringifier`
        pub struct $stringifier {
            writer: $writer<Vec<u8>>,
        }

        impl $crate::serializer::QuadStringifier for $stringifier {
            type Config = Config;

            fn new(config: Config) -> $stringifier {
                $stringifier {
                    writer: $writer::new(Vec::new(), config),
                }
            }
        }

        impl $crate::quad::stream::QuadSink for $stringifier {
            type Outcome = String;
            /// <Vec as io::Write> never returns an error
            type Error = std::convert::Infallible;

            fn feed<T>(&mut self, t: &T) -> std::result::Result<(), Self::Error>
            where
                T: $crate::quad::Quad,
            {
                self.writer.feed(t).map_err(|_| unreachable!())
            }

            fn finish(&mut self) -> std::result::Result<String, Self::Error> {
                let mut v = Vec::new();
                swap(&mut self.writer.write, &mut v);
                Ok(unsafe { String::from_utf8_unchecked(v) })
            }
        }
    };
    () => {
        def_quad_stringifier!(Writer, Stringifier);
    };
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the ::serializer::nt::test module).
}
