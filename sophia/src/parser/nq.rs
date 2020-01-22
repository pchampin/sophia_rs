//! Adapter for the [N-Quads] parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/ntriples.rs)
//!
//! [N-Quads]: https://www.w3.org/TR/n-quads/

use std::io::BufRead;

use rio_turtle::{NQuadsParser as RioNQParser, TurtleError};

use crate::parser::rio_common::*;
use crate::parser::Parser;

/// N-Quads parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct NQuadsParser {}

impl<B: BufRead> Parser<B> for NQuadsParser {
    type Source = StrictRioSource<RioNQParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        StrictRioSource::from(RioNQParser::new(data))
    }
}

def_mod_functions_for_bufread_parser!(NQuadsParser);

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
        // check that NQuadsParser implements QuadParser;
        // actually, if this test compiles, it passes
        fn check_trait<P: crate::parser::QuadParser<&'static [u8]>>(_: &P) {
            assert!(true)
        }
        check_trait(&NQuadsParser::default());
    }

    #[test]
    fn test_simple_nq_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            <http://localhost/ex#me> <http://example.org/ns/knows> _:b1.
            _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ns/Person> <tag:g1>.
            _:b1 <http://example.org/ns/name> "Alice" <tag:g1>.
        "#;

        let mut d = FastDataset::new();
        let p = NQuadsParser {};
        let c = p.parse_str(&turtle).in_dataset(&mut d)?;
        assert_eq!(c, 3);
        assert!(d
            .quads_matching(
                &StaticTerm::new_iri("http://localhost/ex#me").unwrap(),
                &StaticTerm::new_iri("http://example.org/ns/knows").unwrap(),
                &ANY,
                &(None as Option<&StaticTerm>),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &rdf::type_,
                &StaticTerm::new_iri("http://example.org/ns/Person").unwrap(),
                &Some(&StaticTerm::new_iri("tag:g1").unwrap()),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &StaticTerm::new_iri("http://example.org/ns/name").unwrap(),
                &StaticTerm::new_literal("Alice"),
                &Some(&StaticTerm::new_iri("tag:g1").unwrap()),
            )
            .next()
            .is_some());
        Ok(())
    }
}
