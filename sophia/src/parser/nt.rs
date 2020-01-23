//! Adapter for the [N-Triples] parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/ntriples.rs)
//!
//! [N-Triples]: https://www.w3.org/TR/n-triples/

use std::io::BufRead;

use rio_turtle::{NTriplesParser as RioNTParser, TurtleError};

use crate::parser::rio_common::*;
use crate::parser::TripleParser;

/// N-Triples parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct NTriplesParser {}

impl<B: BufRead> TripleParser<B> for NTriplesParser {
    type Source = StrictRioSource<RioNTParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        StrictRioSource::from(RioNTParser::new(data))
    }
}

def_mod_functions_for_bufread_parser!(NTriplesParser, TripleParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::graph::inmem::FastGraph;
    use crate::graph::Graph;
    use crate::ns::{rdf, xsd};
    use crate::term::matcher::ANY;
    use crate::term::StaticTerm;
    use crate::triple::stream::TripleSource;

    #[test]
    fn test_simple_nt_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            <http://localhost/ex#me> <http://example.org/ns/knows> _:b1.
            _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ns/Person>.
            _:b1 <http://example.org/ns/name> "Alice".
        "#;

        let mut g = FastGraph::new();
        let p = NTriplesParser {};
        let c = p.parse_str(&turtle).in_graph(&mut g)?;
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
