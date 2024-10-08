//! Adapter for the Generalized `TriG` parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/gtrig.rs)

use rio_turtle::GTriGParser as RioGTriGParser;
use sophia_api::parser::QuadParser;
use sophia_iri::Iri;
use sophia_rio::parser::GeneralizedRioSource;
use std::io::BufRead;

/// `TriG` parser based on RIO.
#[derive(Clone, Debug, Default)]
pub struct GTriGParser {
    /// The base IRI used by this parser to resolve relative IRI-references.
    pub base: Option<Iri<String>>,
}

impl<B: BufRead> QuadParser<B> for GTriGParser {
    type Source = GeneralizedRioSource<RioGTriGParser<B>>;
    fn parse(&self, data: B) -> Self::Source {
        let base = self
            .base
            .clone()
            .map(Iri::unwrap)
            .map(oxiri::Iri::parse)
            .map(Result::unwrap);
        GeneralizedRioSource(RioGTriGParser::new(data, base))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(GTriGParser, QuadParser);

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
    use sophia_api::term::{SimpleTerm, TermKind, VarName};
    use sophia_iri::Iri;
    use std::collections::HashSet;

    type MyDataset = Vec<Spog<SimpleTerm<'static>>>;

    #[test]
    fn test_simple_gtrig_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let gtrig = r"
            @prefix : <http://example.org/ns/> .

            <#me> :knows _:alice {|
                :since 2002 ;
            |}.
            <tag:g1> {
                _:alice a :Person ; :name ?name.
            }
        ";

        let mut d = MyDataset::new();
        let p = GTriGParser {
            base: Some(Iri::new_unchecked("http://localhost/ex".to_string())),
        };
        let c = p.parse_str(gtrig).add_to_dataset(&mut d)?;
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
                [VarName::new_unchecked("name")],
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
