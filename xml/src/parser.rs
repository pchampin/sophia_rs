//! Parser for the [RDF/XML] concrete syntax of RDF,
//! based on [`rio_xml`](https://docs.rs/rio_xml/)
//!
//! [RDF/XML]: https://www.w3.org/TR/rdf-syntax-grammar/

use std::io::BufRead;

use rio_xml::{RdfXmlError, RdfXmlParser as RioRdfXmlParser};

use sophia_api::parser::TripleParser;
use sophia_rio::parser::*;

/// N-Triples parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct RdfXmlParser {
    /// The base IRI used by this parser to resolve relative IRI-references.
    pub base: Option<String>,
}

impl<B: BufRead> TripleParser<B> for RdfXmlParser {
    type Source = StrictRioSource<RioRdfXmlParser<B>, RdfXmlError>;
    fn parse(&self, data: B) -> Self::Source {
        // TODO issue RdfXmlError if base can not be parsed
        let base = self.base.clone().and_then(|b| oxiri::Iri::parse(b).ok());
        StrictRioSource::Parser(RioRdfXmlParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(RdfXmlParser, TripleParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use sophia::graph::inmem::FastGraph;
    use sophia_api::graph::Graph;
    use sophia_api::ns::{rdf, xsd};
    use sophia_api::triple::stream::TripleSource;
    use sophia_term::matcher::ANY;
    use sophia_term::StaticTerm;

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

        let mut g = FastGraph::new();
        let p = RdfXmlParser {
            base: Some("http://localhost/ex".into()),
        };
        let c = p.parse_str(&xml).add_to_graph(&mut g)?;
        assert_eq!(c, 3);
        assert!(g
            .triples_matching(
                &StaticTerm::new_iri("http://localhost/ex#me").unwrap(),
                &StaticTerm::new_iri("http://example.org/ns/knows").unwrap(),
                &ANY,
            )
            .next()
            .is_some());
        assert!(g
            .triples_matching(
                &ANY,
                &rdf::type_,
                &StaticTerm::new_iri("http://example.org/ns/Person").unwrap(),
            )
            .next()
            .is_some());
        assert!(g
            .triples_matching(
                &ANY,
                &StaticTerm::new_iri("http://example.org/ns/name").unwrap(),
                &StaticTerm::new_literal_dt("Alice", xsd::string).unwrap(),
            )
            .next()
            .is_some());
        Ok(())
    }
}
