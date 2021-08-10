//! Adapter for the TriG parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)

use rio_turtle::{TriGParser as RioTriGParser, TurtleError};
use sophia_api::parser::QuadParser;
use sophia_rio::parser::*;
use std::io::BufRead;

/// TriG parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct TriGParser {
    /// The base IRI used by this parser to resolve relative IRI-references.
    pub base: Option<String>,
}

impl<B: BufRead> QuadParser<B> for TriGParser {
    type Source = StrictRioSource<RioTriGParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        // TODO issue TurtleError if base can not be parsed
        let base = self.base.clone().and_then(|b| oxiri::Iri::parse(b).ok());
        StrictRioSource::Parser(RioTriGParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(TriGParser, QuadParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::dataset::Dataset;
    use sophia_api::ns::{rdf, xsd};
    use sophia_api::quad::stream::QuadSource;
    use sophia_api::term::matcher::ANY;
    use sophia_inmem::dataset::FastDataset;
    use sophia_term::StaticTerm;

    #[test]
    fn test_simple_trig_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let trig = r#"
            @prefix : <http://example.org/ns/> .

            <#g1> {
                <#me> :knows _:alice.
            }
            <#g2> {
                _:alice a :Person ; :name "Alice".
            }
        "#;

        let mut d = FastDataset::new();
        let p = TriGParser {
            base: Some("http://localhost/ex".into()),
        };
        let c = p.parse_str(trig).add_to_dataset(&mut d)?;
        assert_eq!(c, 3);
        assert!(d
            .quads_matching(
                &StaticTerm::new_iri("http://localhost/ex#me").unwrap(),
                &StaticTerm::new_iri("http://example.org/ns/knows").unwrap(),
                &ANY,
                &Some(&StaticTerm::new_iri("http://localhost/ex#g1").unwrap()),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &rdf::type_,
                &StaticTerm::new_iri("http://example.org/ns/Person").unwrap(),
                &Some(&StaticTerm::new_iri("http://localhost/ex#g2").unwrap()),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &StaticTerm::new_iri("http://example.org/ns/name").unwrap(),
                &StaticTerm::new_literal_dt("Alice", xsd::string).unwrap(),
                &Some(&StaticTerm::new_iri("http://localhost/ex#g2").unwrap()),
            )
            .next()
            .is_some());
        Ok(())
    }
}
