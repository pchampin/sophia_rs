use std::io::{BufRead};

use pest::{Error, Parser, iterators::Pair};

use ::graph::MutableGraph;
use ::ns::xsd;
use ::term::{Err, Term};
use super::common::*;


#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("nt.pest");

#[derive(Parser)]
#[grammar = "parsers/nt.pest"]
pub struct NtParser;

pub fn parse_str_into<'a, G> (txt: &'a str, graph: &mut G) -> Result<usize, Error<'a, Rule>> where
    G: MutableGraph,
{
    parse_rule_from_str_into(Rule::generalized_ntriples_doc, txt, graph)
}

pub fn parse_read_into<R, G> (reader: R, graph: &mut G) -> Result<usize, super::Error> where
    R: BufRead,
    G: MutableGraph,
{
    parse_rule_from_read_into(Rule::generalized_triple, reader, graph)
}

pub mod strict {
    use pest::{Error};

    use ::graph::MutableGraph;
    use super::*;

    pub fn parse_str_into<'a, G> (txt: &'a str, graph: &mut G) -> Result<usize, Error<'a, Rule>> where
        G: MutableGraph,
    {
        parse_rule_from_str_into(Rule::ntriples_doc, txt, graph)
    }

    pub fn parse_read_into<R, G> (reader: R, graph: &mut G) -> Result<usize, super::super::Error> where
        R: BufRead,
        G: MutableGraph,
    {
        parse_rule_from_read_into(Rule::triple, reader, graph)
    }

}

pub fn parse_rule_from_str_into<'a, G> (rule: Rule, txt: &'a str, graph: &mut G) -> Result<usize, Error<'a, Rule>> where
    G: MutableGraph,
{
    let mut added = 0;
    let strict = rule == Rule::ntriples_doc || rule == Rule::triple;
    for triple in NtParser::parse(rule, txt)? {
        let mut pairs = triple.into_inner();
        let s = pair_to_term(pairs.next().unwrap(), strict)?;
        let p = pair_to_term(pairs.next().unwrap(), strict)?;
        let o = pair_to_term(pairs.next().unwrap(), strict)?;
        if graph.insert(&s, &p, &o) { added += 1; }
    }
    Ok(added)
}

pub fn parse_rule_from_read_into<R, G> (rule: Rule, reader: R, graph: &mut G) -> Result<usize, super::Error> where
    R: BufRead,
    G: MutableGraph,
{
    let mut total = 0;
    for (lineidx, line) in reader.lines().enumerate() {
        let line = line
            .map_err(|err| super::Error::from_io(err, lineidx+1, total))?;
        let line = line.trim_left();
        if line.len() == 0
        || line.as_bytes()[0] == '#' as u8 {
            continue;
        }
        let added = parse_rule_from_str_into(rule, &line, graph)
            .map_err(|err| super::Error::from_pest(err, lineidx+1, total))?;
        total += added;

    }
    Ok(total)
}

fn pair_to_term<'a> (pair: Pair<'a, Rule>, strict: bool) -> Result<BooTerm<'a>, Error<'a,Rule>> {
    let mut ref_pair = pair.clone();
    match pair.as_rule() {
        Rule::iriref => {
            let boo = unescape_str(pair, 1)?;
            Term::new_iri(boo)
        }
        Rule::blank_node_label => {
            let txt = pair.as_str();
            Term::new_bnode(BooStr::B(&txt[2..]))
        }
        Rule::literal => {
            let mut pairs = pair.into_inner();
            let value_pair = pairs.next().unwrap();
            let value_boo = unescape_str(value_pair, 1)?;

            match pairs.next() {
                None => {
                    Term::new_literal_dt(value_boo, BooTerm::from(&xsd::string))
                }
                Some(ref subpair) if subpair.as_rule() == Rule::iriref => {
                    ref_pair = subpair.clone();
                    let dt_txt = unescape_str(subpair.clone(), 1)?;
                    Term::new_iri(dt_txt)
                    .and_then(|datatype| {
                        Term::new_literal_dt(value_boo, datatype)
                    })
                }
                Some(ref subpair) if subpair.as_rule() == Rule::langtag => {
                    ref_pair = subpair.clone();
                    Term::new_literal_lang(value_boo, &subpair.as_str()[1..])
                }
                _ => unreachable!()
            }
        }
        Rule::variable => {
            let txt = pair.as_str();
            Term::new_variable(BooStr::B(&txt[1..]))
        }
        _ => panic!(format!("Unsupported rule {:?}", pair.as_rule())),
    }
    .and_then(|t|
        if !strict || t.is_absolute() {
            Ok(t)
        } else {
            Err(Err::IriMustBeAbsolute(format!("{:?}", t)))
        }
    )
    .map_err(|err| Error::CustomErrorSpan{
        message: format!("{:?}", err),
        span: ref_pair.into_span(),
    })
}

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use std::ffi::OsStr;
    use std::fs::{File, read_dir};
    use std::io;
    use std::path::Path;
    use pest::{Error, Parser, iterators::Pairs};
    use ::graph::{Graph, inmem::SimpleGraph};
    use super::*;

    fn parse(rule: Rule, txt: &str) -> Result<Pairs<Rule>, Error<Rule>> {
        NtParser::parse(rule, txt)
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
        let mut g = SimpleGraph::new();
        let res = strict::parse_str_into(DOC, &mut g);
        assert_eq!(res, Ok(5));
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
    fn parse_str() {
        let mut g = SimpleGraph::new();
        let res = parse_str_into(GENERALIZED_DOC, &mut g);
        assert_eq!(res, Ok(7));
        assert_eq!(g.len(), 7);
    }

    #[test]
    fn strict_parse_str_refuses_generalized() {
        let mut g = SimpleGraph::new();
        let res = strict::parse_str_into(GENERALIZED_DOC, &mut g);
        assert!(res.is_err());
        assert_eq!(g.len(), 0);
    }

    #[test]
    fn strict_parse_read() {
        let mut g = SimpleGraph::new();
        let reader = io::Cursor::new(DOC);
        let res = strict::parse_read_into(reader, &mut g);
        assert_eq!(res, Ok(5));
        assert_eq!(g.len(), 5);
    }

    #[test]
    fn parse_read() {
        let mut g = SimpleGraph::new();
        let reader = io::Cursor::new(GENERALIZED_DOC);
        let res = parse_read_into(reader, &mut g);
        assert_eq!(res, Ok(7));
        assert_eq!(g.len(), 7);
    }

    #[test]
    fn strict_parse_read_refuses_generalized() {
        let mut g = SimpleGraph::new();
        let reader = io::Cursor::new(GENERALIZED_DOC);
        let res = strict::parse_read_into(reader, &mut g);
        use super::super::Error::Parsing;
        if let Err(Parsing{lineno, already_added, ..}) = res {
            assert_eq!(lineno, 5);
            assert_eq!(already_added, 1);
        } else {
            assert!(false, "res should be an error");
        }
        assert_eq!(g.len(), 1);
    }

    #[test]
    fn spurious_tail() {
        let mut g = SimpleGraph::new();
        let txt = r#"
          <tag:foo> <tag:bar> <tag:baz> . bla bla bla
        "#;
        let res = parse_str_into(txt, &mut g);
        assert!(res.is_err());
        assert_eq!(g.len(), 0);
    }

    #[test]
    fn w3c_test_suite() {
        fn do_test_suite() -> io::Result<()> {
            let nt_ext = Some(OsStr::new("nt"));

            let suite = Path::new("..").join("rdf-tests").join("ntriples");
            if !suite.exists() || !suite.is_dir() {
                panic!("rdf-tests not found, can not check W3C test-suite");
            }

            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nt_ext { continue }

                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut g = SimpleGraph::new();
                let res = strict::parse_read_into(f, &mut g);
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
                panic!("rdf-tests not found, can not check W3C test-suite");
            }

            for entry in read_dir(&suite)? {
                let path = entry?.path();
                if path.extension() != nt_ext { continue }
                if path.to_str().unwrap().contains("-bad-") { continue }
                // "bad" tests may or may not pass with the generalized parser,
                // so we skip them

                let f = File::open(&path)?;
                let f = io::BufReader::new(f);
                let mut g = SimpleGraph::new();
                let res = parse_read_into(f, &mut g);
                assert!(res.is_ok(), format!("{} should parse without error", path.to_str().unwrap()));
            }
            Ok(())
        }
        do_test_suite().unwrap()
    }
}
