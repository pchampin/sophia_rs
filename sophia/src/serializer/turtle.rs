//! Serializer for the [Turtle] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [Turtle]: https://www.w3.org/TR/turtle/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use super::rio_common::rio_format_triples;
use rio_turtle::TurtleFormatter;
use sophia_api::prefix::{PrefixBox, PrefixMap};
use sophia_api::serializer::*;
use sophia_api::triple::stream::{SinkError, StreamResult, TripleSource};
use sophia_iri::IriBox;
use std::io;

mod _pretty;

/// Turtle serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct TurtleConfig {
    pretty: bool,
    prefix_map: Vec<(PrefixBox, IriBox)>,
}

impl TurtleConfig {
    /// Should the parser make extra effort to produce pretty Turtle.
    /// (defaults to false)
    pub fn pretty(&self) -> bool {
        self.pretty
    }

    /// [`PrefixMap`] to use in serialization.
    /// (defaults to empty)
    pub fn prefix_map(&self) -> &[(PrefixBox, IriBox)] {
        &self.prefix_map
    }

    /// Build a new default [`TurtleConfig`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Transform a [`TurtleConfig`] by setting the [`pretty`][`TurtleConfig::pretty`] flag.
    pub fn with_pretty(mut self, b: bool) -> Self {
        self.pretty = b;
        self
    }

    /// Transform a [`TurtleConfig`] by setting the [`prefix_map`][`TurtleConfig::prefix_map`] flag
    /// (copying `pm` using [`PrefixMap::to_vec`]).
    pub fn with_prefix_map<P: PrefixMap + ?Sized>(self, pm: &P) -> Self {
        self.with_own_prefix_map(pm.to_vec())
    }

    /// Transform a [`TurtleConfig`] by setting the [`prefix_map`][`TurtleConfig::prefix_map`] flag.
    pub fn with_own_prefix_map(mut self, pm: Vec<(PrefixBox, IriBox)>) -> Self {
        self.prefix_map = pm;
        self
    }
}

/// RDF/XML serializer.
pub struct TurtleSerializer<W> {
    config: TurtleConfig,
    write: W,
}

impl<W> TurtleSerializer<W>
where
    W: io::Write,
{
    /// Build a new N-Triples serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> TurtleSerializer<W> {
        Self::new_with_config(write, TurtleConfig::default())
    }

    /// Build a new N-Triples serializer writing to `write`, with the given config.
    pub fn new_with_config(write: W, config: TurtleConfig) -> TurtleSerializer<W> {
        TurtleSerializer { write, config }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &TurtleConfig {
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
        source: TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: TripleSource,
    {
        if self.config.pretty {
            for (prefix, ns) in &self.config.prefix_map {
                write!(&mut self.write, "PREFIX {}: <{}>\n", prefix.as_ref(), ns.as_ref()).map_err(SinkError)?;
            }
            _pretty::prettify(source, &mut self.write, &self.config)?;
        } else {
            let mut tf = TurtleFormatter::new(&mut self.write);
            rio_format_triples(&mut tf, source)?;
            tf.finish().map_err(SinkError)?;
        }
        Ok(self)
    }
}

impl TurtleSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        TurtleSerializer::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub fn new_stringifier_with_config(config: TurtleConfig) -> Self {
        TurtleSerializer::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for TurtleSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use sophia_api::graph::isomorphic_graphs;
    use sophia_api::ns::*;
    use sophia_term::literal::convert::AsLiteral;
    use sophia_term::*;

    #[test]
    fn roundtrip() -> Result<(), Box<dyn std::error::Error>> {
        let me = StaticTerm::new_iri("http://champin.net/#pa")?;
        let g = vec![
            [
                me,
                rdf::type_.into(),
                StaticTerm::new_iri("http://schema.org/Person")?,
            ],
            [
                me,
                StaticTerm::new_iri("http://schema.org/name")?,
                "Pierre-Antoine".as_literal().into(),
            ],
        ];
        let s = TurtleSerializer::new_stringifier()
            .serialize_graph(&g)?
            .to_string();
        let g2: Vec<[BoxTerm; 3]> = crate::parser::turtle::parse_str(&s).collect_triples()?;
        assert!(isomorphic_graphs(&g, &g2)?);
        Ok(())
    }
}
