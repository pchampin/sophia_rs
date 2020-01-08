//! Adapter for the Turtle parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)

use std::io::{BufRead, BufReader, Cursor, Read};

use rio_turtle::{TurtleError, TurtleParser};

use crate::def_default_triple_parser_api;
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
pub struct Config {
    pub base: Option<String>,
}

impl Config {
    #[inline]
    pub fn parse_bufread<'a, B: BufRead + 'a>(
        &self,
        bufread: B,
    ) -> impl TripleSource<Error = Error> + 'a {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        RioSource::from(TurtleParser::new(bufread, base))
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

// Convert RIO error to Sophia Error
impl From<TurtleError> for Error {
    fn from(err: TurtleError) -> Error {
        let message = format!("{:?}", err);
        let location = Location::Unknown; // TODO improve once Rio exposes this info
        Error::with_chain(err, ErrorKind::ParserError(message, location))
    }
}

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
    fn test_simple_turtle_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            @prefix : <http://example.org/ns/> .

            <#me> :knows [ a :Person ; :name "Alice" ].
        "#;

        let mut g = FastGraph::new();
        let cfg = Config {
            base: Some("http://localhost/ex".into()),
        };
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
