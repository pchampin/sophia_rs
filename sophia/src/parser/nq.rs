//! Adapter for the [N-Quads] parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/ntriples.rs)
//!
//! [N-Quads]: https://www.w3.org/TR/n-quads/

use std::io::{BufRead, BufReader, Cursor, Read};

use rio_turtle::NQuadsParser;

use crate::def_default_quad_parser_api;
use crate::error::*;
use crate::parser::rio_common::*;
use crate::quad::stream::QuadSource;

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
    ) -> impl QuadSource<Error = Error> + 'a {
        StrictRioSource::from(NQuadsParser::new(bufread))
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
    fn test_simple_nq_string() -> std::result::Result<(), Box<dyn std::error::Error>> {
        let turtle = r#"
            <http://localhost/ex#me> <http://example.org/ns/knows> _:b1.
            _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/ns/Person> <tag:g1>.
            _:b1 <http://example.org/ns/name> "Alice" <tag:g1>.
        "#;

        let mut d = FastDataset::new();
        let cfg = Config {};
        let c = cfg.parse_str(&turtle).in_dataset(&mut d)?;
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
