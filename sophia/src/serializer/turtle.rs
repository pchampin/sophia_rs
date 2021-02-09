//! Serializer for the [Turtle] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [RDF/XML]: https://www.w3.org/TR/turtle/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use super::rio_common::rio_format_triples;
use rio_turtle::TurtleFormatter;
use sophia_api::serializer::*;
use sophia_api::triple::stream::{SinkError, StreamResult, TripleSource};
use std::io;
use std::mem::MaybeUninit;

/// RDF/XML serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct TurtleConfig {}

impl TurtleConfig {
    // TODO add ways to customize prefixes
}

/// RDF/XML serializer.
pub struct TurtleSerializer<W> {
    config: TurtleConfig,
    write: MaybeUninit<W>,
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
        let write = MaybeUninit::new(write);
        TurtleSerializer { write, config }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &TurtleConfig {
        &self.config
    }

    fn write(&self) -> &W {
        unsafe { &*self.write.as_ptr() }
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
        // temporarily move out self.write
        let mut buffer = MaybeUninit::uninit();
        std::mem::swap(&mut self.write, &mut buffer);
        let write = unsafe { buffer.assume_init() };

        let mut tf = TurtleFormatter::new(write);
        rio_format_triples(&mut tf, source)?;
        let write = tf.finish().map_err(SinkError)?;

        // restore self.write
        buffer = MaybeUninit::new(write);
        std::mem::swap(&mut self.write, &mut buffer);
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
        &self.write()[..]
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
