//! This module defines serializers for standard RDF syntaxes,
//! as well as tools for building new serializers.
//! 
//! Each serializer module defines a `Config` type, that must
//! - implement `Default`,
//! - have a `writer` method taking any `io::Write`
//!   and returning a `WriteSerializer`(TODO link),
//! - have a `stringifier` method returning a `StringSerializer`(TODO link).
//! 
//! The module must also provide a `writer` and a `serializer` function,
//! calling the corresponding methods from the default `Config`.
//!
//! `WriteSerializer` and `StringSerializer` are specializations of `TripleSink`.

use std::io;

use ::graph::*;
use ::streams::*;
use ::triple::*;

#[macro_use]
pub mod common;
pub mod nt;

// TODO documentation
pub trait WriteSerializer<W: io::Write>: TripleSink<Outcome=()> + Sized {
    type Config;

    fn new(write: W, config: Self::Config) -> Self;

    fn write<T>(&mut self, source: T) -> Result<(), WhereFrom<T::Error, Self::Error>>
        where T: TripleSource,
    {
        source.into_sink(self)
    }

    fn write_graph<G>(&mut self, graph: &mut G) -> Result<(), WhereFrom<G::Error, Self::Error>>
        where G: Graph,
    {
        graph.iter().into_sink(self)
    }

    fn write_triple<T>(&mut self, t: &T) -> Result<(), Self::Error>
        where T: Triple,
    {
        let source = vec![(t.s(), t.p(), t.o())].into_iter().wrap_as_oks();
        source.into_sink(self).unwrap_upstream()
    }
}

pub trait StringSerializer: TripleSink<Outcome=String, Error=()> + Sized {
    type Config;

    fn new(config: Self::Config) -> Self;

    fn stringify<T>(&mut self, source: T) -> Result<String, T::Error>
        where T: TripleSource,
    {
        source.into_sink(self).unwrap_downstream()
    }

    fn stringify_graph<G>(&mut self, graph: &mut G) -> Result<String, G::Error>
        where G: Graph,
    {
        graph.iter().into_sink(self).unwrap_downstream()
    }

    fn stringify_triple<T>(&mut self, t: &T) -> String
        where T: Triple,
    {
        let source = vec![(t.s(), t.p(), t.o())].into_iter().wrap_as_oks();
        source.into_sink(self).unwrap()
    }
}