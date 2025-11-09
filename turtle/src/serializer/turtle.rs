//! Serializer for the [Turtle] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! ## Pretty vs. lazy serializer
//! The option [`TurtleConfig::pretty`]
//! determines how much effort the serializer will make:
//! * when `true`, it will first analyze the whole graph,
//!   in order to group triples in an optimal way,
//!   and use as much syntactic sugar as possible;
//! * when `false`, it will serializer triples in the order they come,
//!   only using similarities with the previous triple to simplify the output.
//!
//! The first option is *much more* costly,
//! and therefore is not the default.
//!
//! [Turtle]: https://www.w3.org/TR/rdf12-turtle/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use sophia_api::prefix::{Prefix, PrefixMap, PrefixMapPair};
use sophia_api::serializer::{Stringifier, TripleSerializer};
use sophia_api::source::{SinkError, SourceError, StreamResult, TripleSource};
use sophia_api::term::{SimpleTerm, Term};
use sophia_api::triple::Triple;
use sophia_iri::Iri;
use std::io;

use super::_pretty::*;
use super::_streaming::StreamingSerializerState;

/// Turtle serializer configuration.
#[derive(Clone, Debug)]
pub struct TurtleConfig {
    pub(super) pretty: bool,
    pub(super) prefix_map: Vec<PrefixMapPair>,
    pub(super) indentation: String,
}

impl TurtleConfig {
    /// Should the parser make extra effort to produce pretty Turtle.
    ///
    /// If false (default), the triples will be serialized in streaming mode.
    /// Subject and predicate "factorization" will only occur based on the previous triple(s)
    /// in the stream. The collection syntax for `rdf:List`s will not be used.
    ///
    /// If true, extra effort will be made to group related triples together,
    /// and to use the collection syntax whenever possible.
    /// This requires storing the whole graph in memory.
    #[inline]
    pub const fn pretty(&self) -> bool {
        self.pretty
    }

    /// [`PrefixMap`] to use in serialization.
    /// (defaults to a map containing rdf:, rdfs: and xsd:)
    ///
    /// NB: currently, only used if [`pretty`][`TurtleConfig::pretty`] is `true`.
    #[inline]
    pub fn prefix_map(&self) -> &[PrefixMapPair] {
        &self.prefix_map
    }

    /// Indentation to use in serialization.
    /// (defaults to `"  "`, can only contain ASCII whitespaces)
    ///
    /// NB: currently, only used if [`pretty`][`TurtleConfig::pretty`] is `true`.
    #[inline]
    pub fn indentation(&self) -> &str {
        &self.indentation
    }

    /// Build a new default [`TurtleConfig`].
    pub fn new() -> Self {
        let pretty = false;
        let prefix_map = Self::default_prefix_map();
        let indentation = "  ".to_string();
        Self {
            pretty,
            prefix_map,
            indentation,
        }
    }

    /// Transform a [`TurtleConfig`] by setting the [`pretty`][`TurtleConfig::pretty`] flag.
    #[must_use]
    pub const fn with_pretty(mut self, b: bool) -> Self {
        self.pretty = b;
        self
    }

    /// Transform a [`TurtleConfig`] by setting the [`prefix_map`][`TurtleConfig::prefix_map`].
    /// (copying `pm` using [`PrefixMap::to_vec`]).
    #[must_use]
    pub fn with_prefix_map<P: PrefixMap + ?Sized>(self, pm: &P) -> Self {
        self.with_own_prefix_map(pm.to_vec())
    }

    /// Transform a [`TurtleConfig`] by setting the [`prefix_map`][`TurtleConfig::prefix_map`].
    #[must_use]
    pub fn with_own_prefix_map(mut self, pm: Vec<PrefixMapPair>) -> Self {
        self.prefix_map = pm;
        self
    }

    /// Transform a [`TurtleConfig`] by setting the [`indentation`][`TurtleConfig::indentation`].
    ///
    /// # Precondition
    /// `indentation` must only contain ASCII whitespaces, otherwise this method will panic.
    #[must_use]
    pub fn with_indentation<T: ToString>(mut self, indentation: T) -> Self {
        let indentation = indentation.to_string();
        assert!(indentation.chars().all(char::is_whitespace));
        self.indentation = indentation;
        self
    }

    /// Return the prefix map that is used when none is provided
    #[inline]
    pub fn default_prefix_map() -> Vec<PrefixMapPair> {
        vec![
            (
                Prefix::new_unchecked("rdf".into()),
                Iri::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#".into()),
            ),
            (
                Prefix::new_unchecked("rdfs".into()),
                Iri::new_unchecked("http://www.w3.org/2000/01/rdf-schema#".into()),
            ),
            (
                Prefix::new_unchecked("xsd".into()),
                Iri::new_unchecked("http://www.w3.org/2001/XMLSchema#".into()),
            ),
        ]
    }
}

impl Default for TurtleConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Turtle serializer.
pub struct TurtleSerializer<W> {
    pub(super) config: TurtleConfig,
    pub(super) write: W,
}

impl<W> TurtleSerializer<W>
where
    W: io::Write,
{
    /// Build a new Turtle serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, TurtleConfig::default())
    }

    /// Build a new Turtle serializer writing to `write`, with the given config.
    #[inline]
    pub const fn new_with_config(write: W, config: TurtleConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    #[inline]
    pub const fn config(&self) -> &TurtleConfig {
        &self.config
    }
}

impl<W> TripleSerializer for TurtleSerializer<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn serialize_triples<TS>(
        &mut self,
        mut source: TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: TripleSource,
    {
        if self.config.pretty {
            let mut dataset = PrettifiableDataset::new();
            let default = None as Option<SimpleTerm>;
            source
                .for_each_triple(|t| {
                    let spo = t.spo().map(Term::into_term);
                    dataset.insert((default.clone(), spo));
                })
                .map_err(SourceError)?;
            prettify(dataset, &mut self.write, &self.config, "").map_err(SinkError)?;
        } else {
            for (prefix, ns) in &self.config.prefix_map {
                writeln!(&mut self.write, "PREFIX {}: <{ns}>", prefix.as_str())
                    .map_err(SinkError)?;
            }
            let mut state = StreamingSerializerState::new(&mut self.write, &self.config);
            source.try_for_each_triple(|t| state.write_asserted_triple(t))?;
            if state.has_s() {
                state.write_all(b".\n").map_err(SinkError)?;
            }
        }
        Ok(self)
    }
}

impl TurtleSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub const fn new_stringifier_with_config(config: TurtleConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for TurtleSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

#[cfg(test)]
mod test;
