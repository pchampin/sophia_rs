//! Adapter for the Generalized TriG parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/gtrig.rs)

use std::io::BufRead;

use rio_turtle::{GTriGParser as RioGTriGParser, TurtleError};

use crate::parser::rio_common::*;
use crate::parser::QuadParser;

/// TriG parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct GTriGParser {
    pub base: Option<String>,
}

impl<B: BufRead> QuadParser<B> for GTriGParser {
    type Source = GeneralizedRioSource<RioGTriGParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "",
        };
        GeneralizedRioSource::from(RioGTriGParser::new(data, base))
    }
}

def_mod_functions_for_bufread_parser!(GTriGParser, QuadParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::inmem::FastDataset;
    use crate::dataset::Dataset;
    use crate::quad::stream::QuadSource;
    use sophia_api::ns::rdf;
    use sophia_api::term::matcher::ANY;
    use sophia_term::StaticTerm;

    #[test]
    fn test_simple_gtrig_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let gtrig = r#"
            @prefix : <http://example.org/ns/> .

            <#g1> {
                <#me> :knows _:alice.
            }
            <#g2> {
                _:alice a :Person ; :name ?name.
            }
        "#;

        let mut d = FastDataset::new();
        let p = GTriGParser { base: None };
        let c = p.parse_str(&gtrig).add_to_dataset(&mut d)?;
        assert_eq!(c, 3);
        assert!(d
            .quads_matching(
                &StaticTerm::new_iri("#me").unwrap(),
                &StaticTerm::new_iri("http://example.org/ns/knows").unwrap(),
                &ANY,
                &Some(&StaticTerm::new_iri("#g1").unwrap()),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &rdf::type_,
                &StaticTerm::new_iri("http://example.org/ns/Person").unwrap(),
                &Some(&StaticTerm::new_iri("#g2").unwrap()),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &StaticTerm::new_iri("http://example.org/ns/name").unwrap(),
                &StaticTerm::new_variable("name").unwrap(),
                &Some(&StaticTerm::new_iri("#g2").unwrap()),
            )
            .next()
            .is_some());
        Ok(())
    }
}
