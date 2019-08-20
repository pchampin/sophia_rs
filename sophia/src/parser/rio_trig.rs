//! Adapter for the TriG parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)

/// RIO parser configuration.
///
/// For more information,
/// see the [uniform interface] of parsers.
///
/// [uniform interface]: ../index.html#uniform-interface
use std::io::{BufRead, BufReader, Cursor, Read};
use std::iter::once;

use rio_api::parser::QuadParser;
use rio_turtle::TriGParser;

use crate::error::Result;
use crate::parser::rio_turtle::rio2sophia;
use crate::quad::Quad;
use crate::term::BoxTerm;

#[derive(Clone, Debug, Default)]
pub struct Config {
    pub base: Option<String>,
}

type MyQuad = ([BoxTerm; 3], Option<BoxTerm>);

impl Config {
    pub fn parse_bufread<'a, B: BufRead + 'a>(
        &self,
        bufread: B,
    ) -> Box<dyn Iterator<Item = Result<MyQuad>> + 'a> {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        match TriGParser::new(bufread, base) {
            Ok(parser) => Box::new(parser.into_iter(move |q| {
                Ok((
                    [
                        rio2sophia(q.subject.into())?,
                        rio2sophia(q.predicate.into())?,
                        rio2sophia(q.object)?,
                    ],
                    if let Some(n) = q.graph_name {
                        Some(rio2sophia(n.into())?)
                    } else {
                        None
                    },
                ))
            })),
            Err(err) => Box::new(once(Err(err.into()))),
        }
    }

    #[inline]
    pub fn parse_read<'a, R: Read + 'a>(
        &self,
        read: R,
    ) -> impl Iterator<Item = Result<MyQuad>> + 'a {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str) -> impl Iterator<Item = Result<MyQuad>> + 'a {
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
    use crate::quad::stream::QuadSource;

    #[test]
    fn test_simple_trig_string() -> Result<()> {
        let turtle = r#"
            @prefix : <http://example.org/ns> .

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
        Ok(())
    }
}
