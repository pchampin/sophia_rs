//! Adapter for the Turtle parser from [RIO](https://github.com/Tpt/rio/blob/master/turtle/src/turtle.rs)

/// RIO parser configuration.
///
/// For more information,
/// see the [uniform interface] of parsers.
///
/// [uniform interface]: ../index.html#uniform-interface
use std::io::{BufRead, BufReader, Cursor, Read};
use std::iter::once;

use pest::error::{InputLocation, LineColLocation};
use rio_api::model::*;
use rio_api::parser::TripleParser;
use rio_turtle::{TurtleError, TurtleParser};

use crate::error::{Error, ErrorKind, Result};
use crate::ns::xsd;
use crate::term::{BoxTerm, RefTerm};
use crate::triple::Triple;

#[derive(Clone, Debug, Default)]
pub struct Config {
    pub base: Option<String>,
}

impl Config {
    pub fn parse_bufread<'a, B: BufRead + 'a>(
        &self,
        bufread: B,
    ) -> Box<dyn Iterator<Item = Result<[BoxTerm; 3]>> + 'a> {
        let base: &str = match &self.base {
            Some(base) => &base,
            None => "x-no-base:///",
        };
        match TurtleParser::new(bufread, base) {
            Ok(parser) => Box::new(parser.into_iter(move |t| {
                Ok([
                    rio2sophia(t.subject.into())?,
                    rio2sophia(t.predicate.into())?,
                    rio2sophia(t.object)?,
                ])
            })),
            Err(err) => Box::new(once(Err(err.into()))),
        }
    }

    #[inline]
    pub fn parse_read<'a, R: Read + 'a>(
        &self,
        read: R,
    ) -> impl Iterator<Item = Result<[BoxTerm; 3]>> + 'a {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str) -> impl Iterator<Item = Result<[BoxTerm; 3]>> + 'a {
        self.parse_bufread(Cursor::new(txt.as_bytes()))
    }
}

def_default_triple_parser_api! {}

// Convert RIO error to Sophia Error
impl From<TurtleError> for Error {
    fn from(err: TurtleError) -> Error {
        let message = format!("{:?}", err);
        let location = InputLocation::Pos(1); // TODO
        let line_col = LineColLocation::Pos((1, 1)); // TODO
        Error::with_chain(err, ErrorKind::ParserError(message, location, line_col))
    }
}

/// Convert RIO term to Sophia term
pub fn rio2sophia(t: Term) -> Result<BoxTerm> {
    use Literal::*;
    let refterm = match t {
        Term::BlankNode(b) => RefTerm::new_bnode(b.id),
        Term::NamedNode(n) => RefTerm::new_iri(n.iri),
        Term::Literal(Simple { value }) => RefTerm::new_literal_dt(value, xsd::string),
        Term::Literal(LanguageTaggedString { value, language }) => {
            RefTerm::new_literal_lang(value, language)
        }
        Term::Literal(Typed { value, datatype }) => {
            RefTerm::new_literal_dt(value, RefTerm::new_iri(datatype.iri)?)
        }
    }?;
    Ok(BoxTerm::from_with(&refterm, Box::from))
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::graph::inmem::FastGraph;
    use crate::triple::stream::TripleSource;

    #[test]
    fn test_simple_turtle_string() -> Result<()> {
        let turtle = r#"
            @prefix : <http://example.org/ns> .

            <#me> :knows [ a :Person ; :name "Alice" ].
        "#;

        let mut g = FastGraph::new();
        let cfg = Config {
            base: Some("http://localhost/ex".into()),
        };
        let c = cfg.parse_str(&turtle).in_graph(&mut g)?;
        assert_eq!(c, 3);
        Ok(())
    }
}
