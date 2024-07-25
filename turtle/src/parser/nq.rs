//! Adapter for the [N-Quads] parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/nquads.rs)
//!
//! [N-Quads]: https://www.w3.org/TR/n-quads/
use rio_turtle::NQuadsParser as RioNQParser;
use sophia_api::parser::QuadParser;
use sophia_rio::parser::*;
use std::io::BufRead;

/// N-Quads parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct NQuadsParser {}

impl<B: BufRead> QuadParser<B> for NQuadsParser {
    type Source = StrictRioQuadSource<RioNQParser<B>>;
    fn parse(&self, data: B) -> Self::Source {
        StrictRioQuadSource(RioNQParser::new(data))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(NQuadsParser, QuadParser);

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::dataset::Dataset;
    use sophia_api::ns::rdf;
    use sophia_api::quad::Spog;
    use sophia_api::source::QuadSource;
    use sophia_api::term::{SimpleTerm, TermKind};
    use sophia_iri::Iri;
    use std::collections::HashSet;

    type MyDataset = Vec<Spog<SimpleTerm<'static>>>;

    #[test]
    fn test_simple_nq_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let nq = r#"
            <http://localhost/ex#me> <http://example.org/ns/knows> _:b1.
            _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ns/Person> <tag:g1>.
            _:b1 <http://example.org/ns/name> "Alice" <tag:g1>.
            << <http://localhost/ex#me> <http://example.org/ns/knows> _:b1 >> <http://example.org/ns/since> "2002"^^<http://www.w3.org/2001/XMLSchema#integer>.
        "#;

        let mut d = MyDataset::new();
        let p = NQuadsParser {};
        let c = p.parse_str(nq).add_to_dataset(&mut d)?;
        assert_eq!(c, 4);
        assert_eq!(
            d.quads_matching(
                [Iri::new_unchecked("http://localhost/ex#me")],
                [Iri::new_unchecked("http://example.org/ns/knows")],
                TermKind::BlankNode,
                [None as Option<&SimpleTerm>],
            )
            .count(),
            1
        );
        assert_eq!(
            d.quads_matching(
                TermKind::BlankNode,
                [&rdf::type_],
                [Iri::new_unchecked("http://example.org/ns/Person")],
                [Some(Iri::new_unchecked("tag:g1"))],
            )
            .count(),
            1
        );
        assert_eq!(
            d.quads_matching(
                TermKind::BlankNode,
                [Iri::new_unchecked("http://example.org/ns/name")],
                ["Alice"],
                [Some(Iri::new_unchecked("tag:g1"))],
            )
            .count(),
            1
        );
        assert_eq!(
            d.quads_matching(
                (
                    [Iri::new_unchecked("http://localhost/ex#me")],
                    [Iri::new_unchecked("http://example.org/ns/knows")],
                    TermKind::BlankNode,
                ),
                [Iri::new_unchecked("http://example.org/ns/since")],
                [2002],
                [None as Option<&SimpleTerm>],
            )
            .count(),
            1
        );
        assert_eq!(d.blank_nodes().collect::<HashSet<_>>().len(), 1);
        Ok(())
    }
}
