//! Serializers for standard RDF syntaxes,
//! and tools for building new serializers.
//! 
//! # Uniform interface
//! 
//! Each serializer module defines a `Config` type, that
//! - implements [`Default`],
//! - has a `writer` method taking any `io::Write`
//!   and returning a [`WriteSerializer`],
//! - has a `stringifier` method returning a [`StringSerializer`].
//! 
//! Each serializer module also provides two functions `writer` and a `stringifier`,
//! calling the corresponding methods from the default `Config`.
//!
//! [`WriteSerializer`] and [`StringSerializer`] are specializations of [`TripleSink`].
//! 
//! [`Default`]: https://doc.rust-lang.org/std/default/trait.Default.html
//! [`WriteSerializer`]: trait.WriteSerializer.html
//! [`StringSerializer`]: trait.StringSerializer.html
//! [`TripleSink`]: ../streams/trait.TripleSink.html

use std::io;

use ::error::Never;
use ::graph::*;
use ::streams::*;
use ::triple::*;

#[macro_use]
pub mod common;
pub mod nt;

/// An extension of the [`TripleSink`] trait,
/// dedicated to serialization to IO streams.
/// 
/// [`TripleSink`]: ../../streams/trait.TripleSink.html
/// 
pub trait WriteSerializer<W: io::Write>: TripleSink<Outcome=()> + Sized {
    type Config;

    fn new(write: W, config: Self::Config) -> Self;

    /// Serialize the triples from the given source.
    fn write<T>(&mut self, source: T) -> Result<(), WhereFrom<T::Error, Self::Error>>
        where T: TripleSource,
    {
        source.into_sink(self)
    }

    /// Serialize the given graph.
    fn write_graph<G>(&mut self, graph: &mut G) -> Result<(), WhereFrom<G::Error, Self::Error>>
        where G: Graph,
    {
        graph.iter().into_sink(self)
    }

    /// Serialize the given triple.
    fn write_triple<T>(&mut self, t: &T) -> Result<(), Self::Error>
        where T: Triple,
    {
        let source = vec![(t.s(), t.p(), t.o())].into_iter().wrap_as_oks();
        source.into_sink(self).unwrap_upstream()
    }
}

/// An extension of the [`TripleSink`] trait,
/// dedicated to serialization to strings,
/// with methods more explicitly named.
/// 
/// [`TripleSink`]: ../../streams/trait.TripleSink.html
/// 
pub trait StringSerializer: TripleSink<Outcome=String, Error=Never> + Sized {
    type Config;

    fn new(config: Self::Config) -> Self;

    /// Stringify the triples from the given source.
    fn stringify<T>(&mut self, source: T) -> Result<String, T::Error>
        where T: TripleSource,
    {
        source.into_sink(self).unwrap_downstream()
    }

    /// Stringify the given graph.
    fn stringify_graph<G>(&mut self, graph: &mut G) -> Result<String, G::Error>
        where G: Graph,
    {
        graph.iter().into_sink(self).unwrap_downstream()
    }

    /// Stringify the given triple.
    fn stringify_triple<T>(&mut self, t: &T) -> String
        where T: Triple,
    {
        let source = vec![(t.s(), t.p(), t.o())].into_iter().wrap_as_oks();
        source.into_sink(self).unwrap()
    }
}