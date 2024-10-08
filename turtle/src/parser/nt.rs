//! Adapter for the [N-Triples] parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/ntriples.rs)
//!
//! [N-Triples]: https://www.w3.org/TR/n-triples/
use rio_turtle::NTriplesParser as RioNTParser;
use sophia_api::parser::TripleParser;
use sophia_rio::parser::StrictRioTripleSource;
use std::io::BufRead;

/// N-Triples parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct NTriplesParser {}

impl<B: BufRead> TripleParser<B> for NTriplesParser {
    type Source = StrictRioTripleSource<RioNTParser<B>>;
    fn parse(&self, data: B) -> Self::Source {
        StrictRioTripleSource(RioNTParser::new(data))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(NTriplesParser, TripleParser);

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
    use sophia_iri::Iri;
    use std::collections::HashSet;

    type MyGraph = Vec<[SimpleTerm<'static>; 3]>;

    #[test]
    fn test_simple_nt_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let nt = r#"
            <http://localhost/ex#me> <http://example.org/ns/knows> _:b1.
            _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ns/Person>.
            _:b1 <http://example.org/ns/name> "Alice".
            << <http://localhost/ex#me> <http://example.org/ns/knows> _:b1 >> <http://example.org/ns/since> "2002"^^<http://www.w3.org/2001/XMLSchema#integer>.
        "#;

        let mut g = MyGraph::new();
        let p = NTriplesParser {};
        let c = p.parse_str(nt).add_to_graph(&mut g)?;
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
