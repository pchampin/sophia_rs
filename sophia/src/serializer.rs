//! Serializers for standard RDF syntaxes,
//! and tools for building new serializers.
//!
//! # Uniform interface
//!
//! Each serializer module defines a `Config` type, that
//! - implements [`Default`],
//! - has a `writer` method taking any `io::Write`
//!   and returning a [`TripleWriter`] or a [`QuadWriter`],
//! - has a `stringifier` method returning a [`StringSerializer`] or a [`QuadStringifier`].
//!
//! Each serializer module also provides two functions `writer` and a `stringifier`,
//! calling the corresponding methods from the default `Config`.
//!
//! [`TripleWriter`] and [`TripleStringifier`] are specializations of [`TripleSink`].
//! [`QuadWriter`] and [`QuadStringifier`] are specializations of [`QuadSink`].
//!
//! [`Default`]: https://doc.rust-lang.org/std/default/trait.Default.html
//! [`TripleWriter`]: trait.TripleWriter.html
//! [`TripleStringifier`]: trait.TripleStringifier.html
//! [`QuadWriter`]: trait.QuadWriter.html
//! [`QuadStringifier`]: trait.QuadStringifier.html
//! [`TripleSink`]: ../triple/stream/trait.TripleSink.html
//! [`QuadSink`]: ../triple/stream/trait.QuadSink.html

use std::io;

use crate::dataset::*;
use crate::error::*;
use crate::graph::*;
use crate::quad::{stream::*, *};
use crate::triple::{stream::*, *};

use std::result::Result; // override ::error::Result

#[macro_use]
pub mod common;
pub mod nq;
pub mod nt;

/// An extension of the [`TripleSink`] trait,
/// dedicated to serialization to IO streams.
///
/// [`TripleSink`]: ../triple/stream/trait.TripleSink.html
///
pub trait TripleWriter<W: io::Write>: TripleSink<Outcome = ()> + Sized {
    type Config;

    fn new(write: W, config: Self::Config) -> Self;

    /// Serialize the triples from the given source.
    fn write<'a, TS, T>(&mut self, mut source: TS) -> CoercedResult<(), TS::Error, Self::Error>
    where
        TS: TripleSource<'a>,
        TS::Error: CoercibleWith<Self::Error>,
    {
        source.in_sink(self)
    }

    /// Serialize the given graph.
    fn write_graph<'a, G>(&mut self, graph: &'a mut G) -> CoercedResult<(), G::Error, Self::Error>
    where
        G: Graph<'a>,
        G::Error: CoercibleWith<Self::Error>,
    {
        graph.triples().in_sink(self)
    }

    /// Serialize the given triple.
    fn write_triple<'a, T>(&mut self, t: &T) -> CoercedResult<(), Never, Self::Error>
    where
        T: Triple<'a>,
        Never: CoercibleWith<Self::Error>,
    {
        let mut source = vec![[t.s(), t.p(), t.o()]].into_iter().as_triple_source();
        source.in_sink(self)
    }
}

/// An extension of the [`TripleSink`] trait,
/// dedicated to serialization to strings,
/// with methods more explicitly named.
///
/// [`TripleSink`]: ../triple/stream/trait.TripleSink.html
///
pub trait TripleStringifier: TripleSink<Outcome = String> + Sized {
    type Config;

    fn new(config: Self::Config) -> Self;

    /// Stringify the triples from the given source.
    fn stringify<'a, TS, T>(
        &mut self,
        mut source: TS,
    ) -> CoercedResult<String, TS::Error, Self::Error>
    where
        TS: TripleSource<'a>,
        TS::Error: CoercibleWith<Self::Error>,
    {
        source.in_sink(self)
    }

    /// Stringify the given graph.
    fn stringify_graph<'a, G>(
        &mut self,
        graph: &'a mut G,
    ) -> CoercedResult<String, G::Error, Self::Error>
    where
        G: Graph<'a>,
        G::Error: CoercibleWith<Self::Error>,
    {
        graph.triples().in_sink(self)
    }

    /// Stringify the given triple.
    fn stringify_triple<'a, T>(&mut self, t: &T) -> CoercedResult<String, Never, Self::Error>
    where
        T: Triple<'a>,
        Never: CoercibleWith<Self::Error>,
    {
        let mut source = vec![[t.s(), t.p(), t.o()]].into_iter().as_triple_source();
        source.in_sink(self)
    }
}

/// An extension of the [`QuadSink`] trait,
/// dedicated to serialization to IO streams.
///
/// [`QuadSink`]: ../triple/stream/trait.QuadSink.html
///
pub trait QuadWriter<W: io::Write>: QuadSink<Outcome = ()> + Sized {
    type Config;

    fn new(write: W, config: Self::Config) -> Self;

    /// Serialize the triples from the given source.
    fn write<'a, QS, T>(&mut self, mut source: QS) -> CoercedResult<(), QS::Error, Self::Error>
    where
        QS: QuadSource<'a>,
        QS::Error: CoercibleWith<Self::Error>,
    {
        source.in_quad_sink(self)
    }

    /// Serialize the given dataset.
    fn write_dataset<'a, D>(
        &mut self,
        dataset: &'a mut D,
    ) -> CoercedResult<(), D::Error, Self::Error>
    where
        D: Dataset<'a>,
        D::Error: CoercibleWith<Self::Error>,
    {
        dataset.quads().in_quad_sink(self)
    }

    /// Serialize the given triple.
    fn write_quad<'a, Q>(&mut self, q: &Q) -> CoercedResult<(), Never, Self::Error>
    where
        Q: Quad<'a>,
        Never: CoercibleWith<Self::Error>,
    {
        let mut source = vec![([q.s(), q.p(), q.o()], q.g())]
            .into_iter()
            .as_quad_source();
        source.in_quad_sink(self)
    }
}

/// An extension of the [`QuadSink`] trait,
/// dedicated to serialization to strings,
/// with methods more explicitly named.
///
/// [`QuadSink`]: ../triple/stream/trait.QuadSink.html
///
pub trait QuadStringifier: QuadSink<Outcome = String> + Sized {
    type Config;

    fn new(config: Self::Config) -> Self;

    /// Stringify the triples from the given source.
    fn stringify<'a, QS, T>(
        &mut self,
        mut source: QS,
    ) -> CoercedResult<String, QS::Error, Self::Error>
    where
        QS: QuadSource<'a>,
        QS::Error: CoercibleWith<Self::Error>,
    {
        source.in_quad_sink(self)
    }

    /// Stringify the given dataset.
    fn stringify_dataset<'a, D>(
        &mut self,
        dataset: &'a mut D,
    ) -> CoercedResult<String, D::Error, Self::Error>
    where
        D: Dataset<'a>,
        D::Error: CoercibleWith<Self::Error>,
    {
        dataset.quads().in_quad_sink(self)
    }

    /// Stringify the given triple.
    fn stringify_quad<'a, Q>(&mut self, q: &Q) -> CoercedResult<String, Never, Self::Error>
    where
        Q: Quad<'a>,
        Never: CoercibleWith<Self::Error>,
    {
        let mut source = vec![([q.s(), q.p(), q.o()], q.g())]
            .into_iter()
            .as_quad_source();
        source.in_quad_sink(self)
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
