//! Adapter for the [RDF/XML] parser from [RIO](https://github.com/Tpt/rio/blob/master/xml)
//!
//! [RDF/XML]: https://www.w3.org/TR/rdf-syntax-grammar/

use std::io::BufRead;

use rio_xml::{RdfXmlError, RdfXmlParser as RioRdfXmlParser};

use crate::parser::rio_common::*;
use crate::parser::TripleParser;

/// N-Triples parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct RdfXmlParser {
    /// The base IRI used by this parser to resolve relative IRI-references.
    pub base: Option<String>,
}

impl<B: BufRead> TripleParser<B> for RdfXmlParser {
    type Source = StrictRioSource<RioRdfXmlParser<B>, RdfXmlError>;
    fn parse(&self, data: B) -> Self::Source {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        StrictRioSource::from(RioRdfXmlParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(RdfXmlParser, TripleParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::graph::inmem::FastGraph;
    use crate::graph::Graph;
    use crate::ns::{rdf, xsd};
    use crate::triple::stream::TripleSource;
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
