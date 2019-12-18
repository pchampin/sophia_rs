//! Parser for [N-Quads], a simple line-oriented syntax for serializing RDF dataset.
//!
//! [N-Quads]: https://www.w3.org/TR/n-quads/
//!
//! # Example
//! ```
//! // TODO make example similar to the N-Triples
//! ```

use std::borrow::Cow;
use std::io::{BufRead, BufReader, Read};
use std::result::Result as StdResult;

use pest::error::Error as PestError;
use pest::{iterators::Pairs, Parser};

use super::common::*;
use super::nt::{pair_to_term, PestNtqParser, Rule};
use crate::error::*;
use crate::quad::stream::*;
use crate::term::Term;

/// N-Quads parser configuration.
///
/// For more information,
/// see the [uniform interface] of parsers.
///
/// [uniform interface]: ../index.html#uniform-interface
///
#[derive(Clone, Debug, Default)]
pub struct Config {
    /// Should the [strict] RDF model be used ? (defaults to `false`)
    ///
    /// [strict]: ../../index.html#generalized-vs-strict-rdf-model
    pub strict: bool,
}

type ParseStrResult<'a> =
    Box<dyn Iterator<Item = Result<([Term<Cow<'a, str>>; 3], Option<Term<Cow<'a, str>>>)>> + 'a>;

impl Config {
    #[inline]
    pub fn parse_bufread<'a, B: BufRead + 'a>(&self, bufread: B) -> impl QuadSource<Error = Error> {
        let config = self.clone();
        let rule = if config.strict {
            Rule::nquads_line
        } else {
            Rule::generalized_nq_line
        };
        BufReadParser {
            config,
            rule,
            bufread,
        }
    }

    #[inline]
    pub fn parse_read<'a, R: Read + 'a>(&self, read: R) -> impl QuadSource<Error = Error> {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str) -> ParseStrResult<'a> {
        let config = self.clone();
        let rule = if config.strict {
            Rule::nquads_doc
        } else {
            Rule::generalized_nq_doc
        };
        let quad_pairs = match PestNtqParser::parse(rule, txt) {
            Ok(pairs) => pairs,
            Err(err) => {
                return Box::new(std::iter::once(Err(convert_pest_err(err, 0))));
            }
        };
        Box::new(
            quad_pairs
                .take_while(|quad_pair| quad_pair.as_rule() != Rule::EOI)
                .map(move |quad_pair| {
                    pairs_to_quad(&config, quad_pair.into_inner())
                        .map_err(|err| convert_pest_err(err, 0))
                }),
        )
    }
}

struct BufReadParser<B> {
    config: Config,
    rule: Rule,
    bufread: B,
}

impl<B> QuadSource for BufReadParser<B>
where
    B: BufRead,
{
    type Error = Error;

    fn in_sink<TS: QuadSink>(
        &mut self,
        sink: &mut TS,
    ) -> CoercedResult<TS::Outcome, Self::Error, TS::Error>
    where
        Self::Error: CoercibleWith<TS::Error>,
    {
        for (lineidx, line) in (&mut self.bufread).lines().enumerate() {
            let line = match line {
                Ok(line) => line,
                Err(ioerr) => {
                    let msg = format!("{}", ioerr);
                    return Err(Error::with_chain(ioerr, make_parser_error(msg, lineidx)).into());
                }
            };
            let trimmed = line.trim_start();
            if trimmed.is_empty() || trimmed.as_bytes()[0] == b'#' {
                continue;
            }
            let quad = parse_rule_from_line(&self.config, self.rule, line.trim_start())
                .map_err(|err| convert_pest_err(err, lineidx))?;
            sink.feed(&quad)?;
        }
        Ok(sink.finish()?)
    }
}

def_default_quad_parser_api! {}

type ResultQuad<'a> =
    StdResult<([Term<Cow<'a, str>>; 3], Option<Term<Cow<'a, str>>>), PestError<Rule>>;

fn parse_rule_from_line<'a>(config: &Config, rule: Rule, txt: &'a str) -> ResultQuad<'a> {
    let quad_pair = PestNtqParser::parse(rule, txt)?.next().unwrap();
    pairs_to_quad(config, quad_pair.into_inner())
}

fn pairs_to_quad<'a>(config: &Config, mut pairs: Pairs<'a, Rule>) -> ResultQuad<'a> {
    let s = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let p = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let o = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let g = match pairs.next() {
        None => None,
        Some(gn) => Some(pair_to_term(gn, config.strict)?),
    };
    Ok(([s, p, o], g))
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::term::BoxTerm;
    use crate::triple::stream::SourceError;
    use std::collections::HashSet;
    use std::ffi::OsStr;
    use std::fs::{read_dir, File};
    use std::io;
    use std::path::Path;

    type HashSetDataset = HashSet<([BoxTerm; 3], Option<BoxTerm>)>;

    static STRICT: Config = Config { strict: true };

    static DOC: &str = r#"
      # a comment
      <tag:foo> <tag:bar> <tag:baz>. # a trailing comment

      _:foo <tag:bar> _:baz.
      _:foo <tag:bar> "baz" .
      _:foo <tag:bar> "baz"@en <tag:gn>.
      _:foo <tag:bar> "baz"^^<tag:str> <tag:gn>.
      # a final comment
    "#;

    #[test]
    fn strict_parse_str() {
        let mut d = HashSetDataset::new();
        let res = STRICT.parse_str(DOC).in_dataset(&mut d);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 5);
        assert_eq!(d.len(), 5);
    }

    static GENERALIZED_DOC: &str = r#"
      # a comment
      <tag:foo> <tag:bar> <tag:baz> . # a trailing comment

      _:foo _:bar.prop _:baz <tag:gn1>.
      _:foo _:bar.prop _:baz <gn2>.
      _:foo _:bar.prop _:baz "gn3".
      _:foo _:bar.prop _:baz _:gn4.
      _:foo _:bar.prop _:baz ?gn5.
      _:foo <bar> "baz".
      _:foo _:bar "baz" .
      _:foo "bar" "baz"  <tag:gn1>.
      "foo" <tag:bar> "baz" .
      ?foo <tag:bar> "baz" .
      # a final comment
    "#;

    #[test]
    fn default_parse_str() {
        let mut d = HashSetDataset::new();
        let res = parse_str(GENERALIZED_DOC).in_dataset(&mut d);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 11);
        assert_eq!(d.len(), 11);
    }

    #[test]
    fn strict_parse_str_refuses_generalized() {
        let mut d = HashSetDataset::new();
        let res = STRICT.parse_str(GENERALIZED_DOC).in_dataset(&mut d);
        assert!(res.is_err());
        assert!(d.len() < 11);
    }

    #[test]
    fn strict_parse_read() {
        let mut d = HashSetDataset::new();
        let reader = io::Cursor::new(DOC);
        let res = STRICT.parse_read(reader).in_dataset(&mut d);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 5);
        assert_eq!(d.len(), 5);
    }

    #[test]
    fn default_parse_read() {
        let mut d = HashSetDataset::new();
        let reader = io::Cursor::new(GENERALIZED_DOC);
        let res = parse_read(reader).in_dataset(&mut d);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 11);
        assert_eq!(d.len(), 11);
    }

    #[test]
    fn strict_parse_read_refuses_generalized() {
        use crate::error::ErrorKind::*;

        let mut d = HashSetDataset::new();
        let reader = io::Cursor::new(GENERALIZED_DOC);
        let res = STRICT.parse_read(reader).in_dataset(&mut d);
        if let Err(SourceError(Error(ParserError(_, location), _))) = res {
            let line_no = match location {
                Location::Pos(Position::LiCo(line_no, _)) => line_no,
                Location::Span(Position::LiCo(line_no, _), _) => line_no,
                _ => 0,
            };
            assert_eq!(line_no, 5);
        } else {
            assert!(false, "res should be an error");
        }
        assert!(d.len() < 11);
    }

    #[test]
    fn spurious_tail() {
        let mut d = HashSetDataset::new();
        let txt = r#"
          <tag:foo> <tag:bar> <tag:baz> . bla bla bla
        "#;
        let res = parse_str(txt).in_dataset(&mut d);
        assert!(res.is_err());
        assert_eq!(d.len(), 0);
    }

    #[test]
    fn w3c_test_suite() {
        fn do_test_suite() -> io::Result<()> {
            let nq_ext = Some(OsStr::new("nq"));

            let suite = Path::new("..").join("rdf-tests").join("nquads");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests/nquads not found, can not check W3C test-suite. cf README.md");
            }

            let mut tested = 0;
            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nq_ext {
                    continue;
                }

                tested += 1;
                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut d = HashSetDataset::new();
                let res = STRICT.parse_read(f).in_dataset(&mut d);
                let path = path.to_str().unwrap();
                if path.contains("-bad-") {
                    assert!(
                        res.is_err(),
                        format!("{} should NOT parse without error", path)
                    );
                } else {
                    assert!(res.is_ok(), format!("{} should parse without error", path));
                }
            }
            assert_ne!(
                tested, 0,
                "No test found in W3C test-suite, something must be wrong"
            );
            Ok(())
        }
        do_test_suite().unwrap()
    }

    #[test]
    fn w3c_test_suite_generalized() {
        fn do_test_suite() -> io::Result<()> {
            let nq_ext = Some(OsStr::new("nq"));

            let suite = Path::new("..").join("rdf-tests").join("nquads");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests/nquads not found, can not check W3C test-suite. cf README.md");
            }

            let mut tested = 0;
            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nq_ext {
                    continue;
                }
                if path.to_str().unwrap().contains("-bad-") {
                    continue;
                }
                // "bad" tests may or may not pass with the generalized parser,
                // so we skip them

                tested += 1;
                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut d = HashSetDataset::new();
                let res = parse_read(f).in_dataset(&mut d);
                assert!(
                    res.is_ok(),
                    format!("{} should parse without error", path.to_str().unwrap())
                );
            }
            assert_ne!(
                tested, 0,
                "No test found in W3C test-suite, something must be wrong"
            );
            Ok(())
        }
        do_test_suite().unwrap()
    }
}
