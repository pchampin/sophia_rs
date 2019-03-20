//! Parser [N-Quads], a simple line-oriented syntax for serializing RDF dataset.
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

use pest::{Parser, iterators::Pairs};
use pest::error::{Error as PestError};

use crate::error::*;
use crate::quad::Quad;
use crate::term::{Term, graph_key::GraphKey};
use crate::triple::Triple;
use super::common::*;
use super::nt::{PestNtqParser, Rule, pair_to_term};


/// NQ parser configuration.
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

impl Config {
    #[inline]
    pub fn parse_bufread<'a, B: BufRead+'a>(&self, bufread: B)
    -> impl Iterator<Item=Result<NqQuad>>+'a {
        let config = self.clone();
        let rule = if config.strict {Rule::nquads_line} else {Rule::generalized_nq_line};
        bufread.lines().enumerate()
        .filter_map(move |(lineidx, line)| {
            let line = match line {
                Ok(line) => line,
                Err(ioerr) => {
                    let msg = format!("{}", ioerr);
                    return Some(Err(Error::with_chain(
                        ioerr, make_parser_error(msg, lineidx),
                    )));
                }
            };
            {
                let trimmed = line.trim_left();
                if trimmed.len() == 0
                || trimmed.as_bytes()[0] == '#' as u8 {
                    return None;
                }
            }
            Some(NqQuad::try_new(
                line,
                |line| parse_rule_from_line(&config, rule, line.trim_left()),
            ).map_err(|err| convert_pest_err(err.0, lineidx)))
        })
    }

    #[inline]
    pub fn parse_read<'a, R: Read+'a>(&self, read: R)
    -> impl Iterator<Item=Result<NqQuad>>+'a {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str)
    -> Box<dyn Iterator<Item=Result<([Term<Cow<'a, str>>;3], GraphKey<Cow<'a, str>>)>>+'a> {
        let config = self.clone();
        let rule = if config.strict {Rule::nquads_doc} else {Rule::generalized_nq_doc};
        let triple_pairs = match PestNtqParser::parse(rule, txt) {
            Ok(pairs) => pairs,
            Err(err) => {
                return Box::new(std::iter::once(Err(convert_pest_err(err, 0))));
            }
        };
        Box::new(
            triple_pairs
            .take_while(|triple_pair| triple_pair.as_rule() != Rule::EOI)
            .map(move |triple_pair|
                pairs_to_quad(&config, triple_pair.into_inner())
                .map_err(|err| convert_pest_err(err, 0))
            )
        )
    }
}

def_default_quad_parser_api!{}



rental! {
    pub mod nq_quad {
        use super::*;

        #[rental(covariant)]
        pub struct NqQuad {
            line: String,
            quad: ([Term<Cow<'line, str>>;3], GraphKey<Cow<'line, str>>),
        }
    }
}
pub use self::nq_quad::NqQuad;
impl<'a> Triple<'a> for NqQuad {
    type TermData = Cow<'a, str>;
    fn s(&self) -> &Term<Self::TermData> { unsafe { std::mem::transmute(self.suffix().s()) } }
    fn p(&self) -> &Term<Self::TermData> { unsafe { std::mem::transmute(self.suffix().p()) } }
    fn o(&self) -> &Term<Self::TermData> { unsafe { std::mem::transmute(self.suffix().o()) } }
    // The compiler can not figure out the correct lifetime for self in the methods above,
    // so I use transmute() to force the cast.
}
impl<'a> Quad<'a> for NqQuad {
    fn g(&self) -> &GraphKey<Self::TermData> { unsafe { std::mem::transmute(self.suffix().g()) } }
    // The compiler can not figure out the correct lifetime for self in the methods above,
    // so I use transmute() to force the cast.
}


fn parse_rule_from_line<'a> (config: &Config, rule: Rule, txt: &'a str) -> StdResult<([Term<Cow<'a, str>>;3], GraphKey<Cow<'a, str>>), PestError<Rule>> {
    let triple_pair = PestNtqParser::parse(rule, txt)?.next().unwrap();
    pairs_to_quad(config, triple_pair.into_inner())
}

fn pairs_to_quad<'a> (config: &Config, mut pairs: Pairs<'a, Rule>) -> StdResult<([Term<Cow<'a, str>>;3], GraphKey<Cow<'a, str>>), PestError<Rule>> {
    let s = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let p = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let o = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let g = match pairs.next() {
        None => GraphKey::Default,
        Some(gn) => GraphKey::Name(pair_to_term(gn, config.strict)?),
    };
    Ok(([s, p, o], g))
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use std::collections::HashSet;
    use std::ffi::OsStr;
    use std::fs::{File, read_dir};
    use std::io;
    use std::path::Path;
    use crate::term::BoxTerm;
    use crate::quad::stream::*;
    use super::*;

    type HashSetDataset = HashSet<([BoxTerm;3], GraphKey<Box<str>>)>;

    static STRICT: Config = Config{ strict: true };

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

    static GENERALIZED_DOC: &str =  r#"
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
        assert!(dbg!(&res).is_ok());
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
        if let Err(Error(ParserError(_,_,line_pos), _)) = res {
            use ::pest::error::LineColLocation::*;
            let lineno = match line_pos {
                Pos((lineno, _)) => lineno,
                Span((lineno, _), _) => lineno,
            };
            assert_eq!(lineno, 5);
        } else {
            assert!(false, "res should be an error");
        }
        assert  !(d.len() < 11);
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
            let nt_ext = Some(OsStr::new("nt"));

            let suite = Path::new("..").join("rdf-tests").join("nquads");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests/nquads not found, can not check W3C test-suite");
            }

            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nt_ext { continue }

                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut d = HashSetDataset::new();
                let res = STRICT.parse_read(f).in_dataset(&mut d);
                let path = path.to_str().unwrap();
                if path.contains("-bad-") {
                    assert!(res.is_err(), format!("{} should NOT parse without error", path));
                } else {
                    assert!(res.is_ok(), format!("{} should parse without error", path));
                }
            }
            Ok(())
        }
        do_test_suite().unwrap()
    }

    #[test]
    fn w3c_test_suite_generalized() {
        fn do_test_suite() -> io::Result<()> {
            let nt_ext = Some(OsStr::new("nt"));

            let suite = Path::new("..").join("rdf-tests").join("nquads");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests/nquads not found, can not check W3C test-suite");
            }

            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nt_ext { continue }
                if path.to_str().unwrap().contains("-bad-") { continue }
                // "bad" tests may or may not pass with the generalized parser,
                // so we skip them

                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut d = HashSetDataset::new();
                let res = parse_read(f).in_dataset(&mut d);
                assert!(res.is_ok(), format!("{} should parse without error", path.to_str().unwrap()));
            }
            Ok(())
        }
        do_test_suite().unwrap()
    }
}