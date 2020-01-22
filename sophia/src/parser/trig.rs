//! Adapter for the TriG parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)

use std::io::BufRead;

use rio_turtle::{TriGParser as RioTriGParser, TurtleError};

use crate::parser::rio_common::*;
use crate::parser::Parser;

/// TriG parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct TriGParser {
    pub base: Option<String>,
}

impl<B: BufRead> Parser<B> for TriGParser {
    type Source = StrictRioSource<RioTriGParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        StrictRioSource::from(RioTriGParser::new(data, base))
    }
}

def_mod_functions_for_bufread_parser!(TriGParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::inmem::FastDataset;
    use crate::dataset::Dataset;
    use crate::ns::rdf;
    use crate::quad::stream::QuadSource;
    use crate::term::matcher::ANY;
    use crate::term::StaticTerm;

    #[test]
    fn test_is_triple_parser() {
        // check that TriGParser implements QuadParser;
        // actually, if this test compiles, it passes
        fn check_trait<P: crate::parser::QuadParser<&'static [u8]>>(_: &P) {
            assert!(true)
        }
        check_trait(&TriGParser::default());
    }

    #[test]
    fn test_simple_trig_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
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
        let c = p.parse_str(&turtle).in_dataset(&mut d)?;
        assert_eq!(c, 3);
        assert!(d
            .quads_matching(
                &StaticTerm::new_iri("http://localhost/ex#me").unwrap(),
                &StaticTerm::new_iri("http://example.org/ns/knows").unwrap(),
                &ANY,
                &StaticTerm::new_iri("http://localhost/ex#g1").unwrap(),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &rdf::type_,
                &StaticTerm::new_iri("http://example.org/ns/Person").unwrap(),
                &StaticTerm::new_iri("http://localhost/ex#g2").unwrap(),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &StaticTerm::new_iri("http://example.org/ns/name").unwrap(),
                &StaticTerm::new_literal("Alice"),
                &StaticTerm::new_iri("http://localhost/ex#g2").unwrap(),
            )
            .next()
            .is_some());
        Ok(())
    }
}
