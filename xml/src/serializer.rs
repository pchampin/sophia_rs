//! Serializer for the [RDF/XML] concrete syntax of RDF,
//! based on [`oxrdfxml`].
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cases, they should be passed a [`BufWriter`].
//!
//! [RDF/XML]: https://www.w3.org/TR/rdf12-xml/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use oxrdf::{BlankNode, Literal, NamedNode, NamedOrBlankNode, Term as OxTerm, Triple as OxTriple};
use oxrdfxml::RdfXmlSerializer as OxRdfXmlSerializer;
use sophia_api::serializer::{Stringifier, TripleSerializer};
use sophia_api::source::{StreamError, StreamResult, TripleSource};
use sophia_api::term::{Term, TermKind};
use std::io;

/// RDF/XML serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct RdfXmlConfig {
    indentation: usize,
}

impl RdfXmlConfig {
    /// Size of the indentation to use in the serialization.
    /// (defaults to 0, meaning no indentation nor linebreaks)
    #[must_use]
    pub const fn indentation(&self) -> usize {
        self.indentation
    }

    /// Build a new default [`RdfXmlConfig`]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Transform an [`RdfXmlConfig`] by setting the [`indentation`](RdfXmlConfig::indentation).
    #[must_use]
    pub const fn with_indentation(mut self, i: usize) -> Self {
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
    /// Build a new RDF/XML serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, RdfXmlConfig::default())
    }

    /// Build a new RDF/XML serializer writing to `write`, with the given config.
    pub const fn new_with_config(write: W, config: RdfXmlConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    pub const fn config(&self) -> &RdfXmlConfig {
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
        mut source: TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: TripleSource,
    {
        let ox_serializer = OxRdfXmlSerializer::new();
        if self.config.indentation != usize::default() {
            log::warn!("indentation currently ignored by sophia_xml");
        }
        let mut writer = ox_serializer.for_writer(&mut self.write);

        source.try_for_each_triple(|t| {
            if let Some(triple) = sophia_triple_to_ox(&t) {
                writer.serialize_triple(triple.as_ref())
            } else {
                // Triple was skipped due to unsupported terms (warning already logged)
                Ok(())
            }
        })?;

        writer.finish().map_err(StreamError::SinkError)?;
        Ok(self)
    }
}

impl RdfXmlSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    #[must_use]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    #[must_use]
    pub const fn new_stringifier_with_config(config: RdfXmlConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for RdfXmlSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

/// Convert a Sophia triple to an oxrdf Triple, returning None if conversion fails
fn sophia_triple_to_ox<T: sophia_api::triple::Triple>(triple: &T) -> Option<OxTriple> {
    let s = sophia_term_to_ox_subject(triple.s())?;
    let p = sophia_term_to_ox_named_node(triple.p())?;
    let o = sophia_term_to_ox_term(triple.o())?;
    Some(OxTriple::new(s, p, o))
}

/// Convert a Sophia term to an oxrdf NamedOrBlankNode (subject)
fn sophia_term_to_ox_subject<T: Term>(term: T) -> Option<NamedOrBlankNode> {
    match term.kind() {
        TermKind::Iri => {
            let iri = term.iri().unwrap();
            match NamedNode::new(iri.as_str()) {
                Ok(nn) => Some(NamedOrBlankNode::NamedNode(nn)),
                Err(e) => {
                    log::warn!("Skipping triple with invalid IRI in subject: {}", e);
                    None
                }
            }
        }
        TermKind::BlankNode => {
            let bn = term.bnode_id().unwrap();
            Some(NamedOrBlankNode::BlankNode(BlankNode::new_unchecked(
                bn.as_str(),
            )))
        }
        _ => {
            log::warn!(
                "Skipping triple with unsupported term kind in subject: {:?}",
                term.kind()
            );
            None
        }
    }
}

/// Convert a Sophia term to an oxrdf NamedNode (predicate)
fn sophia_term_to_ox_named_node<T: Term>(term: T) -> Option<NamedNode> {
    if let Some(iri) = term.iri() {
        match NamedNode::new(iri.as_str()) {
            Ok(nn) => Some(nn),
            Err(e) => {
                log::warn!("Skipping triple with invalid IRI in predicate: {}", e);
                None
            }
        }
    } else {
        log::warn!("Skipping triple with non-IRI predicate: {:?}", term.kind());
        None
    }
}

/// Convert a Sophia term to an oxrdf Term (object)
fn sophia_term_to_ox_term<T: Term>(term: T) -> Option<OxTerm> {
    match term.kind() {
        TermKind::Iri => {
            let iri = term.iri().unwrap();
            match NamedNode::new(iri.as_str()) {
                Ok(nn) => Some(OxTerm::NamedNode(nn)),
                Err(e) => {
                    log::warn!("Skipping triple with invalid IRI in object: {}", e);
                    None
                }
            }
        }
        TermKind::BlankNode => {
            let bn = term.bnode_id().unwrap();
            Some(OxTerm::BlankNode(BlankNode::new_unchecked(bn.as_str())))
        }
        TermKind::Literal => {
            let lex = term.lexical_form().unwrap();
            if let Some(tag) = term.language_tag() {
                {
                    if let Some(dir) = term.base_direction() {
                        let base_dir = match dir {
                            sophia_api::term::BaseDirection::Ltr => oxrdf::BaseDirection::Ltr,
                            sophia_api::term::BaseDirection::Rtl => oxrdf::BaseDirection::Rtl,
                        };
                        Some(OxTerm::Literal(
                            Literal::new_directional_language_tagged_literal_unchecked(
                                lex,
                                tag.as_str(),
                                base_dir,
                            ),
                        ))
                    } else {
                        Some(OxTerm::Literal(
                            Literal::new_language_tagged_literal_unchecked(lex, tag.as_str()),
                        ))
                    }
                }
            } else {
                let dt = term.datatype().unwrap();
                match NamedNode::new(dt.as_str())
                    .map(|dt_nn| Literal::new_typed_literal(lex, dt_nn))
                {
                    Ok(lit) => Some(OxTerm::Literal(lit)),
                    Err(e) => {
                        log::warn!("Skipping triple with invalid datatype IRI: {}", e);
                        None
                    }
                }
            }
        }
        TermKind::Triple => {
            let spo = term.triple().unwrap();
            let s = sophia_term_to_ox_subject(spo[0])?;
            let p = sophia_term_to_ox_named_node(spo[1])?;
            let o = sophia_term_to_ox_term(spo[2])?;
            Some(OxTerm::Triple(Box::new(OxTriple::new(s, p, o))))
        }
        TermKind::Variable => {
            log::warn!("Skipping triple with variable in object");
            None
        }
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

    const TESTS: &[&str] = &[r##"<?xml version="1.0" encoding="utf-8"?>
        <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:its="http://www.w3.org/2005/11/its"
                 xmlns="http://example.org/ns/"
                 xml:base="http://localhost/ex"
                 rdf:version="1.2"
                 its:version="2.0"
        >
          <rdf:Description rdf:about="#me">
            <knows>
              <Person>
                <name rdf:annotation="#triple1" xml:lang="en" its:dir="ltr">Alice</name>
              </Person>
            </knows>
            <knows rdf:parseType="Triple" rdf:annotationNodeID="triple2">
              <Mortal rdf:about="#socrates" />
            </knows>
          </rdf:Description>
          <rdf:Description rdf:about="#triple1">
            <since xml:datatype="http://www.w3.org/2001/XMLSchema#">2020-01-02</since>
          </rdf:Description>
          <CommonSense rdf:nodeID="triple2" />
        </rdf:RDF>
        "##];

    #[test]
    fn roundtrip() -> Result<(), Box<dyn std::error::Error>> {
        for rdfxml in TESTS {
            println!("==========\n{rdfxml}\n----------");
            let g1: Vec<[SimpleTerm; 3]> = crate::parser::parse_str(rdfxml).collect_triples()?;

            let out = RdfXmlSerializer::new_stringifier()
                .serialize_triples(g1.triples())?
                .to_string();
            println!("{}", &out);

            let g2: Vec<[SimpleTerm; 3]> = crate::parser::parse_str(&out).collect_triples()?;
            println!("{:#?}", &g2);

            assert!(isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }
}
