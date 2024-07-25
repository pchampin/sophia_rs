//! Serializer for the [RDF/XML] concrete syntax of RDF.
//! based on [`rio_xml`].
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! [RDF/XML]: https://www.w3.org/TR/rdf-syntax-grammar/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use rio_xml::RdfXmlFormatter;
use sophia_api::serializer::{Stringifier, TripleSerializer};
use sophia_api::source::{SinkError, StreamResult, TripleSource};
use sophia_rio::serializer::rio_format_triples;
use std::io;

/// RDF/XML serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct RdfXmlConfig {
    indentation: usize,
}

impl RdfXmlConfig {
    /// Size of the indentation to use in the serialization.
    /// (defaults to 0, meaning no indentation nor linebreaks)
    pub fn indentation(&self) -> usize {
        self.indentation
    }

    /// Build a new default [`RdfXmlConfig`]
    pub fn new() -> Self {
        Default::default()
    }

    /// Transform an [`RdfXmlConfig`] by setting the [`indentation`](RdfXmlConfig::indentation).
    pub fn with_indentation(mut self, i: usize) -> Self {
        self.indentation = i;
        self
    }
}

/// RDF/XML serializer.
pub struct RdfXmlSerializer<W> {
    config: RdfXmlConfig,
    write: W,
}

impl<W> RdfXmlSerializer<W>
where
    W: io::Write,
{
    /// Build a new N-Triples serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> RdfXmlSerializer<W> {
        Self::new_with_config(write, RdfXmlConfig::default())
    }

    /// Build a new N-Triples serializer writing to `write`, with the given config.
    pub fn new_with_config(write: W, config: RdfXmlConfig) -> RdfXmlSerializer<W> {
        RdfXmlSerializer { config, write }
    }

    /// Borrow this serializer's configuration.
    pub fn config(&self) -> &RdfXmlConfig {
        &self.config
    }
}

impl<W> TripleSerializer for RdfXmlSerializer<W>
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
        let res = if self.config.indentation > 0 {
            RdfXmlFormatter::with_indentation(&mut self.write, self.config.indentation)
        } else {
            RdfXmlFormatter::new(&mut self.write)
        };
        let mut tf = res.map_err(SinkError)?;
        rio_format_triples(&mut tf, source)?;
        tf.finish().map_err(SinkError)?;
        Ok(self)
    }
}

impl RdfXmlSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        RdfXmlSerializer::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub fn new_stringifier_with_config(config: RdfXmlConfig) -> Self {
        RdfXmlSerializer::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for RdfXmlSerializer<Vec<u8>> {
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
    use sophia_api::graph::Graph;
    use sophia_api::term::SimpleTerm;
    use sophia_isomorphism::isomorphic_graphs;

    const TESTS: &[&str] = &[r#"<?xml version="1.0" encoding="utf-8"?>
        <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns="http://example.org/ns/">
          <rdf:Description rdf:about="http://localhost/ex#me">
            <knows>
              <Person>
                <name>Alice</name>
              </Person>
            </knows>
          </rdf:Description>
        </rdf:RDF>
        "#];

    #[test]
    fn roundtrip() -> Result<(), Box<dyn std::error::Error>> {
        for rdfxml in TESTS {
            println!("==========\n{}\n----------", rdfxml);
            let g1: Vec<[SimpleTerm; 3]> = crate::parser::parse_str(rdfxml).collect_triples()?;

            let out = RdfXmlSerializer::new_stringifier()
                .serialize_triples(g1.triples())?
                .to_string();
            println!("{}", &out);

            let g2: Vec<[SimpleTerm; 3]> = crate::parser::parse_str(&out).collect_triples()?;

            assert!(isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }

    #[test]
    fn roundtrip_with_ident() -> Result<(), Box<dyn std::error::Error>> {
        let config = RdfXmlConfig::new().with_indentation(4);
        for rdfxml in TESTS {
            println!("==========\n{}\n----------", rdfxml);
            let g1: Vec<[SimpleTerm; 3]> = crate::parser::parse_str(rdfxml).collect_triples()?;

            let out = RdfXmlSerializer::new_stringifier_with_config(config.clone())
                .serialize_triples(g1.triples())?
                .to_string();
            println!("{}", &out);

            let g2: Vec<[SimpleTerm; 3]> = crate::parser::parse_str(&out).collect_triples()?;

            assert!(isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }
}
