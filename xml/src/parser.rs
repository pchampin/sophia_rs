//! Parser for the [RDF/XML] concrete syntax of RDF,
//! based on [`rio_xml`].
//!
//! [RDF/XML]: https://www.w3.org/TR/rdf-syntax-grammar/

use rio_xml::RdfXmlParser as RioRdfXmlParser;
use sophia_api::parser::TripleParser;
use sophia_iri::Iri;
use sophia_rio::parser::*;
use std::io::BufRead;

/// N-Triples parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct RdfXmlParser {
    /// The base IRI used by this parser to resolve relative IRI-references.
    pub base: Option<Iri<String>>,
}

impl<B: BufRead> TripleParser<B> for RdfXmlParser {
    type Source = StrictRioTripleSource<RioRdfXmlParser<B>>;
    fn parse(&self, data: B) -> Self::Source {
        let base = self
            .base
            .clone()
            .map(Iri::unwrap)
            .map(oxiri::Iri::parse)
            .map(Result::unwrap);
        StrictRioTripleSource(RioRdfXmlParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(RdfXmlParser, TripleParser);

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
        assert!(g
            .triples_matching(
                [Iri::new_unchecked("http://localhost/ex#me")],
                [Iri::new_unchecked("http://example.org/ns/knows")],
                TermKind::BlankNode,
            )
            .next()
            .is_some());
        assert!(g
            .triples_matching(
                TermKind::BlankNode,
                [rdf::type_],
                [Iri::new_unchecked("http://example.org/ns/Person")],
            )
            .next()
            .is_some());
        assert!(g
            .triples_matching(
                TermKind::BlankNode,
                [Iri::new_unchecked("http://example.org/ns/name")],
                ["Alice"],
            )
            .next()
            .is_some());
        Ok(())
    }
}
