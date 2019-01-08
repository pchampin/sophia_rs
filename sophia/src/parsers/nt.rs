//! Parser for the [N-Triples] concrete syntax of RDF.
//! 
//! [N-Triples]: https://www.w3.org/TR/n-triples/
//! 
//! # Example
//! ```
//! use sophia::graph::inmem::FastGraph;
//! use sophia::parsers::nt;
//! use sophia::streams::*;
//! 
//! static NT_DOC: &str = r#"
//!   <http://champin.net/#pa> <http://schema.org/name> "Pierre-Antoine Champin".
//! "#;
//! 
//! let mut g = FastGraph::new();
//! let inserted = nt::parse_str(NT_DOC).into_graph(&mut g);
//! 
//! assert_eq!(inserted.unwrap(), 1);
//! ```

use std::borrow::Cow;
use std::io::{BufRead, BufReader, Read};

use pest::{Parser, iterators::{Pair, Pairs}};
use pest::error::{Error as PestError, ErrorVariant};

use ::error::{Error, ErrorKind};
use ::ns::xsd;
use ::streams::*;
use ::term::Term;
use super::common::*;


#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("nt.pest");

#[derive(Parser)]
#[grammar = "parsers/nt.pest"]
pub(crate) struct PestNtParser;


/// NT parser configuration.
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
    pub fn parse_bufread<B: BufRead>(&self, bufread: B) -> IoParser<B> {
        IoParser::new(bufread, self.clone())
    }

    #[inline]
    pub fn parse_read<R: Read>(&self, read: R) -> IoParser<BufReader<R>> {
        self.parse_bufread(BufReader::new(read))
    }

    #[inline]
    pub fn parse_str<'a>(&self, txt: &'a str) -> StrParser<'a> {
        StrParser::new(txt, self.clone())
    }
}

def_default_parser_api!(IoParser, StrParser);

/// A [`TripleSource`] returned by [`Config::parse_bufread`]
/// or [`Config::parse_read`].
/// 
/// [`TripleSource`]: ../../streams/trait.TripleSource.html
/// [`Config::parse_bufread`]: struct.Config.html#method.parse_bufread
/// [`Config::parse_read`]: struct.Config.html#method.parse_read
pub struct IoParser<B: BufRead> {
    bufread: B,
    config: Config,
}

impl<B: BufRead> IoParser<B> {
    pub fn new(bufread: B, config: Config) -> Self {
        IoParser{ bufread, config }
    }
}

impl<B: BufRead> TripleSource for IoParser<B> {
    type Error = Error;

    fn into_sink<TS: TripleSink>(self, sink: &mut TS) -> Result<TS::Outcome, WhereFrom<Self::Error, TS::Error>> {
        let bufread = self.bufread;
        let config = self.config;
        let rule = if config.strict {Rule::ntriple_line} else {Rule::generalized_line};
        for (lineidx, line) in bufread.lines().enumerate() {
            let line = line
                .map_err(|io_err| Upstream(io_err.into()))?;
            let line = line.trim_left();
            if line.len() == 0
            || line.as_bytes()[0] == '#' as u8 {
                continue;
            }
            let triple = parse_rule_from_line(&config, rule, &line)
                .map_err(|err| Upstream(convert_pest_err(err, lineidx)))?;
            sink.feed(&triple)
                .map_err(|err| Downstream(err))?;
        }
        sink.finish()
            .map_err(|err| Downstream(err))
    }
}



/// A [`TripleSource`] returned by [`Config::parse_str`].
/// 
/// [`TripleSource`]: ../../streams/trait.TripleSource.html
/// [`Config::parse_str`]: struct.Config.html#method.parse_str
pub struct StrParser<'a> {
    txt: &'a str,
    config: Config,
}

impl<'a> StrParser<'a> {
    pub fn new(txt: &'a str, config: Config) -> Self {
        StrParser{ txt, config }
    }
}

impl<'a> TripleSource for StrParser<'a> {
    type Error = Error;

    fn into_sink<TS: TripleSink>(self, sink: &mut TS) -> Result<TS::Outcome, WhereFrom<Self::Error, TS::Error>> {
        let txt = self.txt;
        let config = self.config;
        let rule = if config.strict {Rule::ntriples_doc} else {Rule::generalized_doc};
        let triple_pairs = PestNtParser::parse(rule, txt)
            .map_err(|err| Upstream(convert_pest_err(err, 0)))?;
        for triple_pair in triple_pairs {
            if triple_pair.as_rule() == Rule::EOI { break }
            let triple = pairs_to_triple(&config, triple_pair.into_inner())
                .map_err(|err| Upstream(convert_pest_err(err, 0)))?;
            sink.feed(&triple)
                .map_err(|err| Downstream(err))?;
        }
        sink.finish()
            .map_err(|err| Downstream(err))
    }
}




fn parse_rule_from_line<'a> (config: &Config, rule: Rule, txt: &'a str) -> Result<[CowTerm<'a>;3], PestError<Rule>> {
    let triple_pair = PestNtParser::parse(rule, txt)?.next().unwrap();
    pairs_to_triple(config, triple_pair.into_inner())
}

fn pairs_to_triple<'a> (config: &Config, mut pairs: Pairs<'a, Rule>) -> Result<[CowTerm<'a>;3], PestError<Rule>> {
    let s = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let p = pair_to_term(pairs.next().unwrap(), config.strict)?;
    let o = pair_to_term(pairs.next().unwrap(), config.strict)?;
    Ok([s, p, o])
}

fn pair_to_term<'a> (pair: Pair<'a, Rule>, strict: bool) -> Result<CowTerm<'a>, PestError<Rule>> {
    match pair.as_rule() {
        Rule::iriref => {
            let cow = unescape_str(pair.clone(), 1)?;
            Term::new_iri(cow)
        }
        Rule::blank_node_label => {
            let txt = pair.as_str();
            Term::new_bnode(Cow::Borrowed(&txt[2..]))
        }
        Rule::literal => {
            let mut pairs = pair.clone().into_inner();
            let value_pair = pairs.next().unwrap();
            let value_cow = unescape_str(value_pair, 1)?;

            match pairs.next() {
                None => {
                    Term::new_literal_dt(value_cow, CowTerm::from(&xsd::string))
                }
                Some(ref subpair) if subpair.as_rule() == Rule::iriref => {
                    let dt_txt = unescape_str(subpair.clone(), 1)?;
                    Term::new_iri(dt_txt)
                    .and_then(|datatype| {
                        Term::new_literal_dt(value_cow, datatype)
                    })
                }
                Some(ref subpair) if subpair.as_rule() == Rule::langtag => {
                    Term::new_literal_lang(value_cow, &subpair.as_str()[1..])
                }
                _ => unreachable!()
            }
        }
        Rule::variable => {
            let txt = pair.as_str();
            Term::new_variable(Cow::Borrowed(&txt[1..]))
        }
        _ => panic!(format!("Unsupported rule {:?}", pair.as_rule())),
    }
    .and_then(|t|
        if !strict || t.is_absolute() {
            Ok(t)
        } else {
            Err(ErrorKind::IriMustBeAbsolute(format!("{:?}", t)).into())
        }
    )
    .map_err(|err| PestError::new_from_span(
        ErrorVariant::CustomError{
            message: format!("{:?}", err)
        },
        pair.as_span(),
    ))
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
    use pest::{Parser, error::Error as PestError, iterators::Pairs};
    use ::term::BoxTerm;
    use super::*;

    type HashSetGraph = HashSet<[BoxTerm;3]>;

    static STRICT: Config = Config{ strict: true };

    fn parse(rule: Rule, txt: &str) -> Result<Pairs<Rule>, PestError<Rule>> {
        PestNtParser::parse(rule, txt)
    }

    /* does not work since the rule 'comment' has been made silent
    #[test]
    fn comment() {
        let rule = Rule::comment;
        test_rule(&parse, rule, &[
            "#",
            "# hello world  "
        ]);
        test_rule_partial(&parse, rule, &[
            ("#\n", 1),
            ("#\r", 1),
            ("# hello world  \n <> <> <> .", 15),
            ("# hello world  \r <> <> <> .", 15),
        ]);
        test_rule_negative(&parse, rule, &[
            "", " ", "a",
        ]);
    }
    */

    #[test]
    fn echar() {
        let rule = Rule::echar;
        test_rule(&parse, rule, &[
            r"\t", r"\b", r"\n", r"\r", r"\f", r#"\""#, r"\'", r"\\",
        ]);
        test_rule_partial(&parse, rule, &[
            (r"\ta", 2), (r"\ba",   2), (r"\na", 2), (r"\ra", 2),
            (r"\fa", 2), (r#"\"a"#, 2), (r"\'a", 2), (r"\\a", 2),
        ]);
        test_rule_negative(&parse, rule, &[
            r"", r"a", r"\x",
        ]);
    }

    #[test]
    fn uchar() {
        let rule = Rule::uchar;
        test_rule(&parse, rule, &[
            r"\u1234", r"\uabcd", r"\uABCD",
            r"\U12345678", r"\Uabcdef00", r"\UABCDEF00",
        ]);
        test_rule_partial(&parse, rule, &[
            (r"\u12345", 6),
            (r"\U123456789", 10),
        ]);
        test_rule_negative(&parse, rule, &[
            r"", r"a", r"\x",
            r"\u1", r"\u12", r"\u123",
            r"\U1", r"\U12", r"\U1234", r"\U12345", r"\U123456", r"\U1234567",
        ]);
    }

    #[test]
    fn blank_node_label() {
        use ::term::test::*;
        let rule = Rule::blank_node_label;
        let positive: Vec<_> =
            POSITIVE_1CHAR_IDS.iter()
            .chain(POSITIVE_N3_BNODE_IDS.iter())
            .chain(NT_ONLY_BNODE_IDS.iter())
            .map(|txt| format!("_:{}", txt))
            .collect();
        test_rule(&parse, rule, &positive[..]);
        test_rule_partial(&parse, rule, &[
            ("_:a ", 3), ("_:a.", 3), ("_:a.b.c.d.", 9),
        ]);
        let negative: Vec<_> =
            NEGATIVE_1CHAR_IDS.iter()
            .filter(|txt| txt != &&":")
            .chain(NEGATIVE_N3_BNODE_IDS.iter())
            .map(|txt| format!("_:{}", txt))
            .collect();
        test_rule_negative(&parse, rule, &negative[..]);
    }

    #[test]
    fn variable() {
        use ::term::test::*;
        let rule = Rule::variable;
        let positive: Vec<_> =
            POSITIVE_1CHAR_IDS.iter()
            .chain(POSITIVE_VARIABLES.iter())
            .map(|txt| format!("?{}", txt))
            .collect();
        test_rule(&parse, rule, &positive[..]);
        test_rule_partial(&parse, rule, &[
            ("?a ", 2), ("?a.", 2),
        ]);
        let negative: Vec<_> =
            NEGATIVE_1CHAR_IDS.iter()
            .chain(NEGATIVE_VARIABLES.iter())
            .map(|txt| format!("?{}", txt))
            .collect();
        test_rule_negative(&parse, rule, &negative[..]);
    }

    #[test]
    fn string_literal_inner() {
        let rule = Rule::string_literal_quote;
        test_rule(&parse, rule, &[
            r#""""#, r#""hello world""#, r#"" hello world ""#,
            r#""éぁ⚠☃""#,
            r#""\u1234""#,
            r#""\t\b\n\r\f\"\'\\""#,
            r#""\u12345\U123456789""#,
        ]);
        test_rule_partial(&parse, rule, &[
            (r#""" "#, 2),  (r#""hello world" "#, 13),
            (r#"""."#, 2),  (r#""hello world"."#, 13),
        ]);
        test_rule_negative(&parse, rule, &[
            "", "a", "\"", r#""\""#,
            r#""\x""#,
            r#""\u1""#, r#""\u12""#, r#""\u123""#,
            r#""\u1g00""#, r#""\u12g0""#, r#""\u123g""#,
            r#""\U1""#, r#""\U12""#, r#""\U123""#, r#""\U12345""#, r#""\U12346""#, r#""\U12347""#,
            r#""\U1g00000""#, r#""\U12g00000""#, r#""\U123g0000""#, r#""\U12345g00""#, r#""\U12346g0""#, r#""\U12347g""#,
        ]);
    }

    #[test]
    fn iriref() {
        let rule = Rule::iriref;
        test_rule(&parse, rule, &[
            "<>", "<foo>", "<.>",
            "<http://champin.net/>",
            r"<\u12345>", r"<\U123456789>",
        ]);
        test_rule_partial(&parse, rule, &[
            ("<> ", 2), ("<>.", 2), ("<>>", 2),
            ("<foo> ", 5), ("<foo>.", 5), ("<foo>>", 5),
        ]);
        test_rule_negative(&parse, rule, &[
            "", "foo", "foaf:knows", "http://champin.net/",
            "< >", "<\x00>", "<\n>",
            r"<\u123>", r"<\U1234567>"
        ]);
    }

    #[test]
    fn langtag() {
        let rule = Rule::langtag;
        test_rule(&parse, rule, &[
            "@fr", "@fr-FR", "@ab-CD-01",
        ]);
        test_rule_partial(&parse, rule, &[
            ("@fr ", 3), ("@fr.", 3), ("@fr- ", 3),
        ]);
        test_rule_negative(&parse, rule, &[
            "", "@", "@0",
        ]);
    }

    static DOC: &str = r#"
      # a comment
      <tag:foo> <tag:bar> <tag:baz>. # a trailing comment

      _:foo <tag:bar> _:baz.
      _:foo <tag:bar> "baz" .
      _:foo <tag:bar> "baz"@en.
      _:foo <tag:bar> "baz"^^<tag:str>.
      # a final comment
    "#;

    #[test]
    fn strict_parse_str() {
        let mut g = HashSetGraph::new();
        let res = STRICT.parse_str(DOC).into_graph(&mut g);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 5);
        assert_eq!(g.len(), 5);
    }

    static GENERALIZED_DOC: &str =  r#"
      # a comment
      <tag:foo> <tag:bar> <tag:baz> . # a trailing comment

      _:foo _:bar.prop _:baz.
      _:foo <bar> "baz" .
      _:foo _:bar "baz" .
      _:foo "bar" "baz" .
      "foo" <tag:bar> "baz" .
      ?foo <tag:bar> "baz" .
      # a final comment
    "#;

    #[test]
    fn default_parse_str() {
        let mut g = HashSetGraph::new();
        let res = parse_str(GENERALIZED_DOC).into_graph(&mut g);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 7);
        assert_eq!(g.len(), 7);
    }

    #[test]
    fn strict_parse_str_refuses_generalized() {
        let mut g = HashSetGraph::new();
        let res = STRICT.parse_str(GENERALIZED_DOC).into_graph(&mut g);
        assert!(res.is_err());
        assert!(g.len() < 7);
    }

    #[test]
    fn strict_parse_read() {
        let mut g = HashSetGraph::new();
        let reader = io::Cursor::new(DOC);
        let res = STRICT.parse_read(reader).into_graph(&mut g);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 5);
        assert_eq!(g.len(), 5);
    }

    #[test]
    fn default_parse_read() {
        let mut g = HashSetGraph::new();
        let reader = io::Cursor::new(GENERALIZED_DOC);
        let res = parse_read(reader).into_graph(&mut g);
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), 7);
        assert_eq!(g.len(), 7);
    }

    #[test]
    fn strict_parse_read_refuses_generalized() {
        use ::error::ErrorKind::*;

        let mut g = HashSetGraph::new();
        let reader = io::Cursor::new(GENERALIZED_DOC);
        let res = STRICT.parse_read(reader).into_graph(&mut g);
        if let Err(Upstream(Error(Parsing(_,_,line_pos), _))) = res {
            use ::pest::error::LineColLocation::*;
            let lineno = match line_pos {
                Pos((lineno, _)) => lineno,
                Span((lineno, _), _) => lineno,
            };
            assert_eq!(lineno, 5);
        } else {
            assert!(false, "res should be an error");
        }
        assert  !(g.len() < 7);
    }

    #[test]
    fn spurious_tail() {
        let mut g = HashSetGraph::new();
        let txt = r#"
          <tag:foo> <tag:bar> <tag:baz> . bla bla bla
        "#;
        let res = parse_str(txt).into_graph(&mut g);
        assert!(res.is_err());
        assert_eq!(g.len(), 0);
    }

    #[test]
    fn w3c_test_suite() {
        fn do_test_suite() -> io::Result<()> {
            let nt_ext = Some(OsStr::new("nt"));

            let suite = Path::new("..").join("rdf-tests").join("ntriples");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests/ntriples not found, can not check W3C test-suite");
            }

            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nt_ext { continue }

                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut g = HashSetGraph::new();
                let res = STRICT.parse_read(f).into_graph(&mut g);
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

            let suite = Path::new("..").join("rdf-tests").join("ntriples");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests/ntriples not found, can not check W3C test-suite");
            }

            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nt_ext { continue }
                if path.to_str().unwrap().contains("-bad-") { continue }
                // "bad" tests may or may not pass with the generalized parser,
                // so we skip them

                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut g = HashSetGraph::new();
                let res = parse_read(f).into_graph(&mut g);
                assert!(res.is_ok(), format!("{} should parse without error", path.to_str().unwrap()));
            }
            Ok(())
        }
        do_test_suite().unwrap()
    }
}