//! Adapter for the Turtle parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)

use std::io::BufRead;

use rio_api::parser::ParseError;
use rio_turtle::{TurtleError, TurtleParser as RioTurtleParser};

use crate::parser::rio_common::*;
use crate::parser::{Location, Parser, WithLocation};

/// Turtle parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct TurtleParser {
    pub base: Option<String>,
}

impl<B: BufRead> Parser<B> for TurtleParser {
    type Source = StrictRioSource<RioTurtleParser<B>, TurtleError>;
    fn parse(&self, data: B) -> Self::Source {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        StrictRioSource::from(RioTurtleParser::new(data, base))
    }
}

impl WithLocation for TurtleError {
    fn location(&self) -> Location {
        match self.textual_position() {
            None => Location::Unknown,
            Some(pos) => Location::from_lico(pos.line_number() + 1, pos.byte_number() + 1),
        }
    }
}

def_mod_functions_for_bufread_parser!(TurtleParser);

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
    fn test_is_triple_parser() {
        // check that TurtleParser implements TripleParser;
        // actually, if this test compiles, it passes
        fn check_trait<P: crate::parser::TripleParser<&'static [u8]>>(_: &P) {
            assert!(true)
        }
        check_trait(&TurtleParser::default());
    }

    #[test]
    fn test_simple_turtle_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            @prefix : <http://example.org/ns/> .

            <#me> :knows [ a :Person ; :name "Alice" ].
        "#;

        let mut g = FastGraph::new();
        let p = TurtleParser {
            base: Some("http://localhost/ex".into()),
        };
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
