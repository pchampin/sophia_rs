//! Parser for the [RDF/XML] concrete syntax of RDF,
//! based on [`oxrdfxml`].
//!
//! [RDF/XML]: https://www.w3.org/TR/rdf12-xml/

use crate::model::Trusted;
use oxrdfxml::{RdfXmlParseError, RdfXmlParser as OxRdfXmlParser};
use sophia_api::parser::TripleParser;
use sophia_api::source::{Source, StreamError, StreamResult};
use sophia_iri::Iri;
use std::io::BufRead;

/// RDF/XML parser.
#[derive(Clone, Debug, Default)]
pub struct RdfXmlParser {
    /// The base IRI used by this parser to resolve relative IRI-references.
    pub base: Option<Iri<String>>,
}

impl<B: BufRead> TripleParser<B> for RdfXmlParser {
    type Source = OxRdfXmlTripleSource<B>;
    fn parse(&self, data: B) -> Self::Source {
        let mut parser = OxRdfXmlParser::new();
        if let Some(ref base) = self.base {
            parser = parser
                .with_base_iri(base.as_str())
                .expect("Invalid base IRI");
        }
        OxRdfXmlTripleSource {
            parser: parser.for_reader(data),
        }
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(RdfXmlParser, TripleParser);

/// A triple source reading from an oxrdfxml parser.
pub struct OxRdfXmlTripleSource<R: BufRead> {
    parser: oxrdfxml::ReaderRdfXmlParser<R>,
}

impl<R: BufRead> Source for OxRdfXmlTripleSource<R> {
    type Error = Error;
    type Item<'x> = Trusted<&'x oxrdf::Triple>;

    fn try_for_some_item<E, F>(&mut self, mut f: F) -> StreamResult<bool, Self::Error, E>
    where
        E: std::error::Error,
        F: FnMut(Self::Item<'_>) -> Result<(), E>,
    {
        match self.parser.next() {
            Some(Ok(triple)) => f(Trusted(&triple))
                .map(|_| true)
                .map_err(StreamError::SinkError),
            Some(Err(err)) => Err(StreamError::SourceError(err)),
            None => Ok(false),
        }
    }
}

/// Error returned by [`RdfXmlParser`].
pub type Error = RdfXmlParseError;

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::graph::Graph;
    use sophia_api::ns::rdf;
    use sophia_api::source::TripleSource;
    use sophia_api::term::{SimpleTerm, TermKind};

    type MyGraph = Vec<[SimpleTerm<'static>; 3]>;

    #[test]
    fn test_simple_xml_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let xml = r#"<?xml version="1.0" encoding="utf-8"?>
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
        "#;

        let mut g = MyGraph::new();
        let p = RdfXmlParser {
            base: Some(Iri::new_unchecked("http://localhost/ex".to_string())),
        };
        let c = p.parse_str(xml).add_to_graph(&mut g)?;
        assert_eq!(c, 3);
        assert!(
            g.triples_matching(
                [Iri::new_unchecked("http://localhost/ex#me")],
                [Iri::new_unchecked("http://example.org/ns/knows")],
                TermKind::BlankNode,
            )
            .next()
            .is_some()
        );
        assert!(
            g.triples_matching(
                TermKind::BlankNode,
                [rdf::type_],
                [Iri::new_unchecked("http://example.org/ns/Person")],
            )
            .next()
            .is_some()
        );
        assert!(
            g.triples_matching(
                TermKind::BlankNode,
                [Iri::new_unchecked("http://example.org/ns/name")],
                ["Alice"],
            )
            .next()
            .is_some()
        );
        Ok(())
    }
}
