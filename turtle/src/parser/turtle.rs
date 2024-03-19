//! Adapter for the Turtle parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)
use rio_turtle::TurtleParser as RioTurtleParser;
use sophia_api::parser::TripleParser;
use sophia_iri::Iri;
use sophia_rio::parser::*;
use std::io::BufRead;

/// Turtle parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct TurtleParser {
    /// The base IRI used by this parser to resolve relative IRI-references.
    pub base: Option<Iri<String>>,
}

impl<B: BufRead> TripleParser<B> for TurtleParser {
    type Source = StrictRioTripleSource<RioTurtleParser<B>>;
    fn parse(&self, data: B) -> Self::Source {
        let base = self
            .base
            .clone()
            .map(Iri::unwrap)
            .map(oxiri::Iri::parse)
            .map(Result::unwrap);
        StrictRioTripleSource(RioTurtleParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(TurtleParser, TripleParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::graph::Graph;
    use sophia_api::ns::rdf;
    use sophia_api::source::TripleSource;
    use sophia_api::term::{SimpleTerm, TermKind};
    use std::collections::HashSet;

    type MyGraph = Vec<[SimpleTerm<'static>; 3]>;

    #[test]
    fn test_simple_turtle_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            @prefix : <http://example.org/ns/> .

            <#me> :knows [ a :Person ; :name "Alice" ] {|
                :since 2002 ;
            |}.
        "#;

        let mut g = MyGraph::new();
        let p = TurtleParser {
            base: Some(Iri::new_unchecked("http://localhost/ex".to_string())),
        };
        let c = p.parse_str(turtle).add_to_graph(&mut g)?;
        assert_eq!(c, 4);
        assert_eq!(
            g.triples_matching(
                [Iri::new_unchecked("http://localhost/ex#me")],
                [Iri::new_unchecked("http://example.org/ns/knows")],
                TermKind::BlankNode,
            )
            .count(),
            1
        );
        assert_eq!(
            g.triples_matching(
                TermKind::BlankNode,
                [&rdf::type_],
                [Iri::new_unchecked("http://example.org/ns/Person")],
            )
            .count(),
            1
        );
        assert_eq!(
            g.triples_matching(
                TermKind::BlankNode,
                [Iri::new_unchecked("http://example.org/ns/name")],
                ["Alice"],
            )
            .count(),
            1
        );
        assert_eq!(
            g.triples_matching(
                (
                    [Iri::new_unchecked("http://localhost/ex#me")],
                    [Iri::new_unchecked("http://example.org/ns/knows")],
                    TermKind::BlankNode,
                ),
                [Iri::new_unchecked("http://example.org/ns/since")],
                [2002],
            )
            .count(),
            1
        );
        assert_eq!(g.blank_nodes().collect::<HashSet<_>>().len(), 1);
        Ok(())
    }
}
