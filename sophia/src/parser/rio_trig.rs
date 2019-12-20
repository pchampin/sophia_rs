//! Adapter for the TriG parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)

use std::io::{BufRead, BufReader, Cursor, Read};

use rio_turtle::TriGParser;

use crate::error::*;
use crate::parser::rio_common::*;
use crate::quad::stream::QuadSource;

/// RIO TriG parser configuration.
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
    ) -> impl QuadSource<Error = Error> + 'a {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        RioSource::from(TriGParser::new(bufread, base))
    }

    #[inline]
    pub fn parse_read<'a, R: Read + 'a>(&self, read: R) -> impl QuadSource<Error = Error> + 'a {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str) -> impl QuadSource<Error = Error> + 'a {
        self.parse_bufread(Cursor::new(txt.as_bytes()))
    }
}

def_default_quad_parser_api! {}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::inmem::FastDataset;
    use crate::dataset::Dataset;
    use crate::ns::{rdf, xsd};
    use crate::quad::stream::QuadSource;
    use crate::term::matcher::ANY;
    use crate::term::StaticTerm;

    #[test]
    fn test_simple_trig_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            @prefix : <http://example.org/ns/> .

            <#g1> {
                <#me> :knows _:alice.
            }
            <#g2> {
                _:alice a :Person ; :name "Alice".
            }
        "#;

        let mut d = FastDataset::new();
        let cfg = Config {
            base: Some("http://localhost/ex".into()),
        };
        let c = cfg.parse_str(&turtle).in_dataset(&mut d)?;
        assert_eq!(c, 3);
        assert!(d
            .quads_matching(
                &StaticTerm::new_iri("http://localhost/ex#me").unwrap(),
                &StaticTerm::new_iri("http://example.org/ns/knows").unwrap(),
                &ANY,
                &StaticTerm::new_iri("http://localhost/ex#g1").unwrap(),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &rdf::type_,
                &StaticTerm::new_iri("http://example.org/ns/Person").unwrap(),
                &StaticTerm::new_iri("http://localhost/ex#g2").unwrap(),
            )
            .next()
            .is_some());
        assert!(d
            .quads_matching(
                &ANY,
                &StaticTerm::new_iri("http://example.org/ns/name").unwrap(),
                &StaticTerm::new_literal_dt("Alice", xsd::string).unwrap(),
                &StaticTerm::new_iri("http://localhost/ex#g2").unwrap(),
            )
            .next()
            .is_some());
        Ok(())
    }
}
