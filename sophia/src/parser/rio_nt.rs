//! Adapter for the [N-Triples] parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)
//!
//! [N-Triples]: https://www.w3.org/TR/n-triples/

use std::io::{BufRead, BufReader, Cursor, Read};

use rio_turtle::NTriplesParser;

use crate::error::*;
use crate::parser::rio_common::*;
use crate::triple::stream::TripleSource;

/// RIO Turtle parser configuration.
///
/// For more information,
/// see the [uniform interface] of parsers.
///
/// [uniform interface]: ../index.html#uniform-interface
#[derive(Clone, Debug, Default)]
pub struct Config {}

impl Config {
    #[inline]
    pub fn parse_bufread<'a, B: BufRead + 'a>(
        &self,
        bufread: B,
    ) -> impl TripleSource<Error = Error> + 'a {
        RioSource::from(NTriplesParser::new(bufread))
    }

    #[inline]
    pub fn parse_read<'a, R: Read + 'a>(&self, read: R) -> impl TripleSource<Error = Error> + 'a {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str) -> impl TripleSource<Error = Error> + 'a {
        self.parse_bufread(Cursor::new(txt.as_bytes()))
    }
}

def_default_triple_parser_api! {}

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
        let cfg = Config {};
        let c = cfg.parse_str(&turtle).in_graph(&mut g)?;
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
