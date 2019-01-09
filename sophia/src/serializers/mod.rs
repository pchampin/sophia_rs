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

use ::error::*;
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
    fn write<'a, TS, T>(&mut self, mut source: TS) -> Result<()> where
        TS: TripleSource<'a>,
    {
        source.in_sink(self)
    }

    /// Serialize the given graph.
    fn write_graph<'a, G>(&mut self, graph: &'a mut G) -> Result<()>
        where G: Graph<'a>,
    {
        graph.iter().in_sink(self)
    }

    /// Serialize the given triple.
    fn write_triple<'a, T>(&mut self, t: &T) -> Result<()>
        where T: Triple<'a>,
    {
        let mut source = vec![[t.s(), t.p(), t.o()]].into_iter().as_triple_source();
        source.in_sink(self)
    }
}

/// An extension of the [`TripleSink`] trait,
/// dedicated to serialization to strings,
/// with methods more explicitly named.
/// 
/// [`TripleSink`]: ../../streams/trait.TripleSink.html
/// 
pub trait StringSerializer: TripleSink<Outcome=String> + Sized {
    type Config;

    fn new(config: Self::Config) -> Self;

    /// Stringify the triples from the given source.
    fn stringify<'a, TS, T>(&mut self, mut source: TS) -> Result<String> where
        TS: TripleSource<'a>,
    {
        source.in_sink(self)
    }

    /// Stringify the given graph.
    fn stringify_graph<'a, G>(&mut self, graph: &'a mut G) -> Result<String>
        where G: Graph<'a>,
    {
        graph.iter().in_sink(self)
    }

    /// Stringify the given triple.
    fn stringify_triple<'a, T>(&mut self, t: &T) -> String
        where T: Triple<'a>,
    {
        let mut source = vec![[t.s(), t.p(), t.o()]].into_iter().as_triple_source();
        source.in_sink(self).unwrap()
    }
}



#[cfg(test)]
mod test {
    // Nothing really worth testing here
}