//! Adapter for the [N-Quads] parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/ntriples.rs)
//!
//! [N-Quads]: https://www.w3.org/TR/n-quads/

use crate::parser::rio_common::*;
use rio_turtle::{NQuadsParser as RioNQParser, TurtleError};
use sophia_api::parser::QuadParser;
use std::io::BufRead;

/// N-Quads parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct NQuadsParser {}

impl<B: BufRead> QuadParser<B> for NQuadsParser {
    type Source = StrictRioSource<RioNQParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        StrictRioSource::Parser(RioNQParser::new(data))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(NQuadsParser, QuadParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::inmem::FastDataset;
    use crate::dataset::Dataset;
    use crate::quad::stream::QuadSource;
    use sophia_api::ns::{rdf, xsd};
    use sophia_api::term::matcher::ANY;
    use sophia_term::StaticTerm;

    #[test]
    fn test_simple_nq_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            <http://localhost/ex#me> <http://example.org/ns/knows> _:b1.
            _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ns/Person> <tag:g1>.
            _:b1 <http://example.org/ns/name> "Alice" <tag:g1>.
        "#;

        let mut d = FastDataset::new();
        let p = NQuadsParser {};
        let c = p.parse_str(&turtle).add_to_dataset(&mut d)?;
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
                &StaticTerm::new_literal_dt("Alice", xsd::string).unwrap(),
                &Some(&StaticTerm::new_iri("tag:g1").unwrap()),
            )
            .next()
            .is_some());
        Ok(())
    }
}
