use std::convert::Infallible;

use crate::{
    _term::IndexedTerm,
    test::{LazyMap, ttl_samples},
};

use super::*;
use sophia_api::{
    ns::{NsTerm, rdf},
    parser::TripleParser,
    source::{StreamError::SourceError, TripleSource},
    term::{BaseDirection, SimpleTerm, Term},
};
use test_case::{test_case, test_matrix};

#[test_case("@basex", (0, 1); "at expect base or prefix")]
#[test_case("@base tag:", (0, 6); "at base expect opening bracket")]
#[test_case("BASE tag:", (0, 5); "sparql base expect opening bracket")]
#[test_case("@base <tag:", (0, 6); "at base expect closing bracket")]
#[test_case("BASE <tag:", (0, 5); "sparql base expect closing bracket")]
#[test_case("@base <tag:> []", (0, 13); "at base expect period")]
#[test_case("@prefix p", (0, 8); "at prefix expect colon")]
#[test_case("PREFIX p", (0, 7); "sparql prefix expect colon")]
#[test_case("@prefix p: tag:", (0, 11); "at prefix expect opening bracket")]
#[test_case("PREFIX p: tag:", (0, 10); "sparql prefix expect opening bracket")]
#[test_case("@prefix p: <tag:", (0, 11); "at prefix expect closing bracket")]
#[test_case("PREFIX p: <tag:", (0, 10); "sparql prefix expect closing bracket")]
#[test_case("@prefix p: <tag:> p:s p:p p:o .", (0, 18); "at prefix expect period")]
#[test_case("@version 1", (0, 9); "at version expect string")]
#[test_case("VERSION 1", (0, 8); "version expect string")]
#[test_case("p", (0, 1); "prefix name expect colon")]
#[test_case("p a:", (0, 1); "prefix name expect colon before whitespace")]
#[test_case("&:suf", (0, 0); "prefixed name invalid")]
#[test_case("_", (0, 1); "bnode label expect colon")]
#[test_case("[] a +", (0, 5); "numeric literal expect digit")]
#[test_case("[] a +1e", (0, 7); "numeric literal expect exponent 1")]
#[test_case("[] a 1e+", (0, 6); "numeric literal expect exponent 2")]
#[test_case(r#"[] a "a"#, (0, 7); "unclosed quote in literal")]
#[test_case("[] a 'a", (0, 7); "unclosed single quote in literal")]
#[test_case("[] a 'a' ^", (0, 10); "incomplete datatype marker in literal")]
#[test_case("[] a 'a'@1", (0, 9); "invalid language tag in literal")]
#[test_case("[] a 'a'@,", (0, 9); "empty language tag in literal")]
#[test_case("[] a 'a'@en--foo,", (0, 13); "wrong base dir in literal")]
#[test_case("[] a 1 ~ [<", (0, 10); "property in anon reifier")]
#[test_case("<< [<", (0, 4); "property in anon rtSubject")]
#[test_case("<< [] a [<", (0, 9); "property in anon rtObject")]
#[test_case("[] a <<( [<]", (0, 10); "property in anon ttSubject")]
#[test_case("[] a <<( [] a [<", (0, 15); " property in anon ttObject")]
#[test_case("[] a 1 {", (0, 8); "incomplete opening annotation bracket eof")]
#[test_case("[] a 1 { ", (0, 8); "incomplete opening annotation bracket space")]
#[test_case("[] a 1 |", (0, 8); "incomplete closing annotation bracket in triples eof")]
#[test_case("[] a 1 | ", (0, 8); "incomplete closing annotation bracket in triples space")]
#[test_case("[] a 1 <", (0, 7); "unexpected fourth component in triples")]
#[test_case("[] a 1]", (0, 6); "unexpected closing square bracket in triples")]
#[test_case("[] a 1|}", (0, 6); "unexpected closing annotation bracket in triples")]
#[test_case("[] a 1; |", (0, 9); "incomplete closing annotation bracket in triples after semi eof")]
#[test_case("[] a 1; | ", (0, 9); "incomplete closing annotation bracket in triples after semi space")]
#[test_case("[] a 1;,", (0, 7); "unexpected comma in triples after semi")]
#[test_case("[] a 1;]", (0, 7); "unexpected closing square bracket in triples after semi")]
#[test_case("[] a 1;|}", (0, 7); "unexpected closing annotation bracket in triples after semi")]
#[test_case("[] a 1 ~ >", (0, 9); "unexpected closing bracket after empty reifier")]
#[test_case("[] a 1 {| a 3 |", (0, 15); "incomplete closing bracket in annotation eof")]
#[test_case("[] a 1 {| a 3 | ", (0, 15); "incomplete closing bracket in annotation space")]
#[test_case("[] a 1 {| a 3 .", (0, 14); "unexpected period in annotation")]
#[test_case("[] a 1 {| a 3 <", (0, 14); "unexpected fourth component in annotation")]
#[test_case("[] a 1 {| a 3 ]", (0, 14); "unexpected closing square bracket in annotation")]
#[test_case("[] a 1 {| a 3; |", (0, 16); "incomplete closing bracket in annotation after semi eof")]
#[test_case("[] a 1 {| a 3; | ", (0, 16); "incomplete closing bracket in annotation after semi space")]
#[test_case("[] a 1 {| a 3; ,", (0, 15); "unexpected comma in annotation after semi")]
#[test_case("[] a 1 {| a 3; .", (0, 15); "unexpected period in annotation after semi")]
#[test_case("[] a 1 {| a 3; ]", (0, 15); "unexpected closing square bracket in annotation after semi")]
#[test_case("[] a [ a 2 |", (0, 12); "incomplete closing annotation bracket in blank node eof")]
#[test_case("[] a [ a 2 | ", (0, 12); "incomplete closing annotation bracket in blank node space")]
#[test_case("[] a [ a 2 .", (0, 11); "unexpected period in blank node")]
#[test_case("[] a [ a 2 <", (0, 11); "unexpected fourth component in blank node")]
#[test_case("[] a [ a 2|}", (0, 10); "unexpected closing annotation bracket in blank node")]
#[test_case("[] a [ a 2; ,", (0, 12); "unexpected comma in blank node after semi")]
#[test_case("[] a [ a 2; .", (0, 12); "unexpected period in blank node after semi")]
#[test_case("[] a [ a 2;|}", (0, 11); "unexpected closing annotation bracket in blank node after semi")]
#[test_case("<< [] a 1 <", (0, 10); "unexpected fourth component in reifiedTriple")]
#[test_case("<< [] a 1 ,", (0, 10); "unexpected comma in reifiedTriple")]
#[test_case("<< [] a 1 ;", (0, 10); "unexpected semicolon in reifiedTriple")]
#[test_case("<< [] a 1 .", (0, 10); "unexpected period in reifiedTriple")]
#[test_case("<< [] a 1 >", (0, 10); "incomplete closing bracket for reifiedTriple eof")]
#[test_case("<< [] a 1 > ", (0, 10); "incomplete closing bracket for reifiedTriple space")]
#[test_case("[] a <<( [] a 1 <", (0, 16); "unexpected fourth component in tripleTerm")]
#[test_case("[] a <<( [] a 1 ~", (0, 16); "unexpected reifier in tripleTerm")]
#[test_case("[] a <<( [] a 1 ,", (0, 16); "unexpected comma in tripleTerm")]
#[test_case("[] a <<( [] a 1 ;", (0, 16); "unexpected semicolon in tripleTerm")]
#[test_case("[] a <<( [] a 1 .", (0, 16); "unexpected period in tripleTerm")]
#[test_case("[] a <<( [] a 1 )", (0, 16); "incomplete closing bracket for tripleTerm 1")]
#[test_case("[] a <<( [] a 1 )>", (0, 16); "incomplete closing bracket for tripleTerm 2")]
#[test_case("[] a <<( [] a << ", (0, 14); "reified triple in triple term")]
fn err_expected(input: &str, exp_pos: (usize, usize)) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        let Err(SourceError(err)) = ts.try_for_each_triple(no_callback) else {
            panic!()
        };
        assert!(
            matches!(dbg!(err.kind()), ErrorKind::Expected(_)),
            "{err:#?}"
        );
        assert_eq!(err.position(), exp_pos);
    }
    Ok(())
}

#[test_case("@base <http://ex.co/ >.", (0, 7); "at prefix space ")]
#[test_case("BASE <http://ex.co/ >", (0, 6); "sparql prefix space")]
#[test_case("@base <rel>.", (0, 7); "at prefix relative")]
#[test_case("BASE <rel>", (0, 6); "sparql prefix relative")]
fn err_iri(input: &str, exp_pos: (usize, usize)) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        let Err(SourceError(err)) = ts.try_for_each_triple(no_callback) else {
            panic!()
        };
        assert!(matches!(err.kind(), ErrorKind::Iri(_)), "{err:#?}");
        assert_eq!(err.position(), exp_pos);
    }
    Ok(())
}

#[test_case("@prefix &: <http://ex.co/>.", (0, 9); "at prefix invalid")]
#[test_case("@prefix a b: <http://ex.co/>.", (0, 9); "at prefix space")]
#[test_case("PREFIX &: <http://ex.co/>", (0, 8); "sparql prefix invalid")]
#[test_case("PREFIX a b: <http://ex.co/>", (0, 8); "sparql prefix space")]
fn err_invalid_prefix(input: &str, exp_pos: (usize, usize)) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        let Err(SourceError(err)) = dbg!(ts.try_for_each_triple(no_callback)) else {
            panic!()
        };
        assert!(
            matches!(err.kind(), ErrorKind::InvalidPrefix(_)),
            "{err:#?}"
        );
        assert_eq!(err.position(), exp_pos);
    }
    Ok(())
}

#[test_case("p:suf", (0, 0))]
fn err_unknown_prefix(input: &str, exp_pos: (usize, usize)) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        let Err(SourceError(err)) = dbg!(ts.try_for_each_triple(no_callback)) else {
            panic!()
        };
        assert!(
            matches!(err.kind(), ErrorKind::UnknownPrefix(_)),
            "{err:#?}"
        );
        assert_eq!(err.position(), exp_pos);
    }
    Ok(())
}

#[test_case(r"[] a '\a", (0, 6))]
#[test_case(r"[] a '\u123", (0, 6))]
#[test_case(r"[] a '\U1234567", (0, 6))]
#[test_case(r"[] a '\u123G", (0, 6))]
#[test_case(r"[] a '\U1234567G", (0, 6))]
fn err_invalid_escape(input: &str, exp_pos: (usize, usize)) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        let Err(SourceError(err)) = dbg!(ts.try_for_each_triple(no_callback)) else {
            panic!()
        };
        assert!(matches!(err.kind(), ErrorKind::InvalidEscape), "{err:#?}");
        assert_eq!(err.position(), exp_pos);
    }
    Ok(())
}

#[test_case("_:", (0, 2); "bnode label expect non-empty label")]
#[test_case("_:-", (0, 2); "bnode label expect valid label")]
fn err_bnode(input: &str, exp_pos: (usize, usize)) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        let Err(SourceError(err)) = dbg!(ts.try_for_each_triple(no_callback)) else {
            panic!()
        };
        assert!(matches!(err.kind(), ErrorKind::Bnode), "{err:#?}");
        assert_eq!(err.position(), exp_pos);
    }
    Ok(())
}

#[test_case("@base <tag:> .", "tag:"; "at base")]
#[test_case("@base<tag:>.", "tag:"; "at base no space")]
#[test_case("@base\n <tag:>\n.", "tag:"; "at base multiline")]
#[test_case("base <tag:>", "tag:"; "sparql base lower")]
#[test_case("base<tag:>", "tag:"; "sparql base lower no space")]
#[test_case("base\n <tag:>", "tag:"; "sparql base lower multiline")]
#[test_case("BASE <tag:>", "tag:"; "sparql base upper")]
#[test_case("BASE<tag:>", "tag:"; "sparql base upper no space")]
#[test_case("BASE\n <tag:>", "tag:"; "sparql base upper multiline")]
#[test_case("bAsE <tag:>", "tag:"; "sparql base mixed")]
#[test_case("bAsE<tag:>", "tag:"; "sparql base mixed no space")]
#[test_case("bAsE\n <tag:>", "tag:"; "sparql base mixed multiline")]
#[test_case("Base <tag:>", "tag:"; "sparql base camel")]
#[test_case("Base<tag:>", "tag:"; "sparql base camel no space")]
#[test_case("Base\n <tag:>", "tag:"; "sparql base camel multiline")]
#[test_case("BASE <tag:> BASE <foo/>", "tag:foo/"; "combined bases")]
#[test_case("BASE<tag:>BASE<foo/>", "tag:foo/"; "combined bases no space")]
#[test_case("BASE\n<tag:>\nBASE\n <foo/>", "tag:foo/"; "combined bases multiline")]
#[test_case("BASE#\n<tag:>#\nBASE#\n <foo/>#", "tag:foo/"; "combined bases multiline with comments")]
fn base(input: &str, exp: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        ts.try_for_each_triple(no_callback)?;
        assert!(ts.extra.base.is_some());
        assert_eq!(ts.extra.base.as_ref().unwrap().as_str(), exp);
    }
    Ok(())
}

#[test_case("@prefix p: <tag:> .", "p", "tag:"; "at prefix")]
#[test_case("@prefix p:<tag:>.", "p", "tag:"; "at prefix no space")]
#[test_case("@prefix\n p:\n<tag:>\n.", "p", "tag:"; "at prefix multiline")]
#[test_case("@prefix 🎉: <tag:> .", "🎉", "tag:"; "at prefix unicode")]
#[test_case("@prefix : <tag:> .", "", "tag:"; "at prefix empty")]
#[test_case("prefix p: <tag:>", "p", "tag:"; "sparql prefix lower")]
#[test_case("prefix p:<tag:>", "p", "tag:"; "sparql prefix lower no space")]
#[test_case("prefix\n p:\n<tag:>", "p", "tag:"; "sparql prefix lower multiline")]
#[test_case("prefix 🎉: <tag:>", "🎉", "tag:"; "sparql prefix lower unicode")]
#[test_case("prefix : <tag:>", "", "tag:"; "sparql prefix lower empty")]
#[test_case("PREFIX p: <tag:>", "p", "tag:"; "sparql prefix upper")]
#[test_case("PREFIX p: <tag:>", "p", "tag:"; "sparql prefix upper no space")]
#[test_case("PREFIX\n p:\n<tag:>", "p", "tag:"; "sparql prefix upper multiline")]
#[test_case("PREFIX 🎉: <tag:>", "🎉", "tag:"; "sparql prefix upper unicode")]
#[test_case("PREFIX : <tag:>", "", "tag:"; "sparql prefix upper empty")]
#[test_case("pReFiX p: <tag:>", "p", "tag:"; "sparql prefix mixed")]
#[test_case("pReFiX p:<tag:>", "p", "tag:"; "sparql prefix mixed no space")]
#[test_case("pReFiX\n p:\n<tag:>", "p", "tag:"; "sparql prefix mixed multiline")]
#[test_case("pReFiX 🎉: <tag:>", "🎉", "tag:"; "sparql prefix mixed unicode")]
#[test_case("pReFiX : <tag:>", "", "tag:"; "sparql prefix mixed empty")]
#[test_case("Prefix p: <tag:>", "p", "tag:"; "sparql prefix camel")]
#[test_case("Prefix p:<tag:>", "p", "tag:"; "sparql prefix camel no space")]
#[test_case("Prefix\n p:\n<tag:>", "p", "tag:"; "sparql prefix camel multiline")]
#[test_case("Prefix 🎉: <tag:>", "🎉", "tag:"; "sparql prefix camel unicode")]
#[test_case("Prefix : <tag:>", "", "tag:"; "sparql prefix camel empty")]
#[test_case("BASE <tag:> PREFIX p: <foo/>", "p", "tag:foo/"; "relative prefix")]
#[test_case("BASE<tag:>PREFIX p:<foo/>", "p", "tag:foo/"; "relative prefix no space")]
#[test_case("BASE\n<tag:>\nPREFIX\np:\n<foo/>", "p", "tag:foo/"; "relative prefix multiline")]
#[test_case("PREFIX#\np:#\n<tag:>", "p", "tag:"; "separated by comments")]
fn prefix(input: &str, exp1: &str, exp2: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let p = TurtleParser::new();
        let mut ts = p.parse_str(i);
        ts.try_for_each_triple(no_callback)?;
        assert_eq!(ts.extra.prefixes.len(), 1);
        let (got1, got2) = &ts.extra.prefixes[0];
        assert_eq!(got1.as_str(), exp1);
        assert_eq!(got2.as_str(), exp2);
    }
    Ok(())
}

#[test_matrix(
    [Version::Rdf11, Version::Rdf12Basic, Version::Rdf12],
    ["at", "sparql"],
    ["space", "tab", "newline", "comment"],
    ["double", "single"],
    ["eol", "comment"]
)]
fn version(version: Version, keyword: &str, sep: &str, quote: &str, suffix: &str) -> TestResult {
    // the 'indirect' input are needed by test_matrix to generate distinct test names
    let keyword = match keyword {
        "at" => "@version",
        "sparql" => "VERSION",
        _ => unreachable!(),
    };
    let sep = match sep {
        "space" => " ",
        "tab" => "\t",
        "newline" => "\n",
        "comment" => "#\n",
        _ => unreachable!(),
    };
    let quote = match quote {
        "double" => "\"",
        "single" => "'",
        _ => unreachable!(),
    };
    let colon = match keyword {
        "@version" => ".",
        "VERSION" => "",
        _ => unreachable!(),
    };
    let suffix = match suffix {
        "eol" => "",
        "comment" => "\n # this is a comment",
        _ => unreachable!(),
    };
    let line = format!("{keyword}{sep}{quote}{version}{quote}{colon}{suffix}");
    let p = TurtleParser::new();
    let mut ts = p.parse_str(&line);
    dbg!(ts.try_for_each_triple(no_callback)).unwrap();
    assert_eq!(ts.inner.version, version);
    Ok(())
}

#[test_matrix(
    ["at", "sparql"],
    ["space", "tab", "newline", "comment"],
    ["double", "single"],
    ["eol", "comment"]
)]
fn unrecognized_version(keyword: &str, sep: &str, quote: &str, suffix: &str) -> TestResult {
    // the 'indirect' input are needed by test_matrix to generate distinct test names
    let keyword = match keyword {
        "at" => "@version",
        "sparql" => "VERSION",
        _ => unreachable!(),
    };
    let sep = match sep {
        "space" => " ",
        "tab" => "\t",
        "newline" => "\n",
        "comment" => "#\n",
        _ => unreachable!(),
    };
    let quote = match quote {
        "double" => "\"",
        "single" => "'",
        _ => unreachable!(),
    };
    let colon = match keyword {
        "@version" => ".",
        "VERSION" => "",
        _ => unreachable!(),
    };
    let suffix = match suffix {
        "eol" => "",
        "comment" => "\n # this is a comment",
        _ => unreachable!(),
    };
    let line = format!("{keyword}{sep}{quote}foo{quote}{colon}{suffix}");
    let p = TurtleParser::new();
    let mut ts = p.parse_str(&line);
    dbg!(ts.try_for_each_triple(no_callback)).unwrap();
    assert_eq!(ts.inner.version, Version::default());
    Ok(())
}

#[test_case(r"<tag:x>", "tag:x")]
#[test_case(r"<tag:\u0073>", "tag:s"; "short numeric escape")]
#[test_case(r"<tag:\U00000073>", "tag:s"; "long numeric escape")]
#[test_case(r"BASE <tag:> <x>", "tag:x")]
#[test_case(r"PREFIX p:<tag:> p:x", "tag:x")]
#[test_case(r"PREFIX :<tag:> :y", "tag:y")]
#[test_case(r"PREFIX p:<tag:> p:", "tag:")]
#[test_case(r"PREFIX :<tag:> :", "tag:")]
#[test_case(r"PREFIX p:<tag:> p:x\~\.\-\!\$\&\'\(\)\*\+\,\;\=\/\?\#\@\_\$\%42", "tag:x~.-!$&'()*+,;=/?#@_$%42"; "reserved escaped")]
#[test_case(r"PREFIX 🎉:<tag:> 🎉:🎉", "tag:🎉"; "unicode")]
#[test_case(r"PREFIX longprefix:<tag:> longprefix:longsuffix", "tag:longsuffix")]
#[test_case(r"PREFIX p:<tag:> p::", "tag::"; "double colon")]
#[test_case(r"PREFIX p:<tag:> p:1.a:", "tag:1.a:")]
fn iri(input: &str, exp: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        assert_eq!(&ts.inner.terms[..], &[IndexedTerm::Iri(0)]);
        assert_eq!(ts.inner.buffers.top(), exp);
    }
    Ok(())
}

#[test_case(r"_:b", "b")]
#[test_case(r"_:a.a ", "a.a")]
#[test_case(r"[]", "bn0001")]
#[test_case(r"_:bn0001", "bn0001d")]
#[test_case(r"_:bn0001d", "bn0001dd")]
fn bnode(input: &str, exp: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        assert_eq!(&ts.inner.terms[..], &[IndexedTerm::BlankNode(0)]);
        assert_eq!(ts.inner.buffers.top(), exp);
    }
    Ok(())
}

#[test_case(r"[] a true ", true)]
#[test_case(r"[] a false ", false)]
fn boolean_literal(input: &str, exp: bool) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        assert_eq!(ts.inner.terms.last(), Some(&IndexedTerm::BoolLiteral(exp)));
    }
    Ok(())
}

#[test_case(r"[] a 12", "12", "#integer")]
#[test_case(r"[] a 12.3", "12.3", "#decimal")]
#[test_case(r"[] a .1", ".1", "#decimal")]
#[test_case(r"[] a 12e3", "12e3", "#double")]
#[test_case(r"[] a 12.3e4", "12.3e4", "#double")]
#[test_case(r"[] a .1e-23", ".1e-23", "#double")]
#[test_case(r"[] a 1.e+23", "1.e+23", "#double")]
fn numeric_literal(input: &str, exp_lex: &str, exp_dt: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        assert!(matches!(
            ts.inner.terms.last(),
            Some(&IndexedTerm::XsdLiteral(_, _))
        ));
        let Some(IndexedTerm::XsdLiteral(_, got_dt)) = ts.inner.terms.last() else {
            unreachable!()
        };
        assert_eq!(ts.inner.buffers.top(), exp_lex);
        assert_eq!(got_dt.as_str(), exp_dt);
    }
    Ok(())
}

#[test_case(r#"[] a "hello world""#, "hello world")]
#[test_case(r#"[] a "\b\f\n\r\t\"\'\\""#, "\x08\x0C\n\r\t\"'\\"; "echars")]
#[test_case(r#"[] a "\u00e9""#, "é"; "u16")]
#[test_case(r#"[] a "\U0001F389""#, "🎉"; "u32")]
#[test_case(r#"[] a 'hello world'"#, "hello world"; "hello world single quote")]
#[test_case(r#"[] a '\b\f\n\r\t\"\'\\'"#, "\x08\x0C\n\r\t\"'\\"; "echars single quote")]
#[test_case(r#"[] a '\u00e9'"#, "é"; "u16 single quote")]
#[test_case(r#"[] a '\U0001F389'"#, "🎉"; "u32 single quote")]
#[test_case(r#"[] a """hello world""""#, "hello world"; "hello world long")]
#[test_case(r#"[] a """\b\f\n\r\t\"\'\\""""#, "\x08\x0C\n\r\t\"'\\"; "echars long")]
#[test_case(r#"[] a """\u00e9""""#, "é"; "u16 long")]
#[test_case(r#"[] a """\U0001F389""""#, "🎉"; "u32 long")]
#[test_case(r#"[] a '''hello world'''"#, "hello world"; "hello world single quote long")]
#[test_case(r#"[] a '''\b\f\n\r\t\"\'\\'''"#, "\x08\x0C\n\r\t\"'\\"; "echars single quote long")]
#[test_case(r#"[] a '''\u00e9'''"#, "é"; "u16 single quote long")]
#[test_case(r#"[] a '''\U0001F389'''"#, "🎉"; "u32 single quote long")]
#[test_case(r#"[] a ''''a'''"#, "'a"; "tricky single quote long")]
#[test_case(r#"[] a """
""''""""#, "\n\"\"''"; "unescaped newline and quotes in long")]
#[test_case(r#"[] a '''
''""'''"#, "\n''\"\""; "unescaped newline and quotes in single quote long")]
fn rdf_literal_lex(input: &str, exp: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        assert_eq!(ts.inner.buffers.top(), exp);
    }
    Ok(())
}

#[test]
fn rdf_literal_lex_multiline() -> TestResult {
    let data = "[] a '''\n a \n '''";
    let mut ts = TurtleSource::new(());
    ts.parse_line(data, no_callback)?;
    assert_eq!(ts.inner.buffers.top(), "\n a \n ");
    Ok(())
}

#[test_case(r#"[] a "hello" {|"#, "hello")]
fn rdf_literal_simple(input: &str, exp_lex: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        let idx = ts.inner.terms.len() - 4; // because a reifying triple was pushed on top
        assert!(matches!(
            dbg!(ts.inner.terms.get(idx)),
            Some(&IndexedTerm::XsdLiteral(_, _))
        ));
        let Some(IndexedTerm::XsdLiteral(idx, got_dt)) = ts.inner.terms.get(idx) else {
            unreachable!()
        };
        assert_eq!(ts.inner.buffers.get(*idx).unwrap(), exp_lex);
        assert_eq!(got_dt, "#string");
    }
    Ok(())
}

#[test_case("[] a true .", rdf::type_; "space")]
#[test_case("[] a true\n.", rdf::type_; "new line")]
#[test_case("[] a true\r.", rdf::type_; "vertical tab")]
#[test_case("[] a true\t.", rdf::type_; "tab")]
#[test_case("[] a true# comment\n.", rdf::type_; "comment")]
#[test_case("[] a true~ <x:r>.", rdf::type_; "reifier")]
#[test_case("[] a true, 42.", rdf::type_; "comma")]
#[test_case("[] a true; <x:p> 42.", rdf::type_; "semicolon")]
#[test_case("[] a true.", rdf::type_; "period")]
#[test_case("[] a true{| a 42 |}.", rdf::type_; "annotation")]
#[test_case("[  a true].", rdf::type_; "closing square bracket")]
#[test_case("[] a (true[]).", rdf::first; "square bracket")]
#[test_case("[] a (true()).", rdf::first; "list")]
#[test_case("[] a (true).", rdf::first; "closing list")]
#[test_case("[] <x:p> 42 {| a true|}.", rdf::type_; "closing annotation")]
fn rdf_literal_boolean(input: &str, pred: NsTerm) -> TestResult {
    let p = TurtleParser::new();
    let g: Vec<[SimpleTerm; 3]> = p.parse_str(input).collect_triples()?;
    for t in g {
        if pred == t[1] {
            assert!(Term::eq(&t[2], true));
            return Ok(());
        }
    }
    panic!("boolean not found in parsed triples");
}

#[test_case("<<[] a true >>."; "RT space")]
#[test_case("<<[] a true\n>>."; "RT new line")]
#[test_case("<<[] a true\r>>."; "RT vertical tab")]
#[test_case("<<[] a true\t>>."; "RT tab")]
#[test_case("<<[] a true# comment\n>>."; "RT comment")]
#[test_case("<<[] a true~ <x:r>>>."; "RT reifier")]
#[test_case("<<[] a true>>."; "RT closing")]
#[test_case("<x:s> <x:p> <<([] a true )>>."; "TT space")]
#[test_case("<x:s> <x:p> <<([] a true\n)>>."; "TT new line")]
#[test_case("<x:s> <x:p> <<([] a true\r)>>."; "TT vertical tab")]
#[test_case("<x:s> <x:p> <<([] a true\t)>>."; "TT tab")]
#[test_case("<x:s> <x:p> <<([] a true# comment\n)>>."; "TT comment")]
#[test_case("<x:s> <x:p> <<([] a true)>>."; "TT closing")]
fn rdf_literal_boolean_in_triple_term(input: &str) -> TestResult {
    let p = TurtleParser::new();
    let g: Vec<[SimpleTerm; 3]> = p.parse_str(input).collect_triples()?;
    for t in g {
        if let Some(tt) = t[2].triple() {
            assert!(Term::eq(&tt[2], true));
            return Ok(());
        }
    }
    panic!("boolean not found in parsed triples");
}

#[test_case(r#"[] a "hello" ^^ <ex:dt>"#, "hello", "ex:dt")]
#[test_case(r#"PREFIX : <ex:> [] a "hello" ^^ :dt"#, "hello", "ex:dt")]
fn rdf_literal_typed(input: &str, exp_lex: &str, exp_dt: &str) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        assert!(matches!(
            dbg!(ts.inner.terms.last()),
            Some(&IndexedTerm::TypedLiteral(_, _))
        ));
        let Some(IndexedTerm::TypedLiteral(idx_lex, idx_dt)) = ts.inner.terms.last() else {
            unreachable!()
        };
        assert_eq!(ts.inner.buffers.get(*idx_lex).unwrap(), exp_lex);
        assert_eq!(ts.inner.buffers.get(*idx_dt).unwrap(), exp_dt);
    }
    Ok(())
}

#[test_case(r#"[] a "hello" @en"#, "hello", "en", None)]
#[test_case(
    r#"[] a "hello" @en-UK--ltr"#,
    "hello",
    "en-UK",
    Some(BaseDirection::Ltr)
)]
#[test_case(
    r#"[] a "ספרים בינלאומיים!" @he--rtl"#,
    "ספרים בינלאומיים!",
    "he",
    Some(BaseDirection::Rtl)
)]
fn rdf_literal_langstr(
    input: &str,
    exp_lex: &str,
    exp_tag: &str,
    exp_dir: Option<BaseDirection>,
) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let mut ts = TurtleSource::new(());
        ts.parse_line(i, no_callback)?;
        assert!(matches!(
            dbg!(ts.inner.terms.last()),
            Some(&IndexedTerm::LangString(_, _, _))
        ));
        let Some(IndexedTerm::LangString(idx_lex, idx_tag, got_dir)) = ts.inner.terms.last() else {
            unreachable!()
        };
        assert_eq!(ts.inner.buffers.get(*idx_lex).unwrap(), exp_lex);
        assert_eq!(ts.inner.buffers.get(*idx_tag).unwrap(), exp_tag);
        assert_eq!(*got_dir, exp_dir);
    }
    Ok(())
}

#[test_matrix(
    ["<x:s>", ":s", "[\n]", "_:bs"],
    ["<x:p>", ":p", "a"],
    ["<x:o>", ":o", "[\n]", "_:bo", "1", "1.2", "1.2e3", "true", r#""simple""#, r#""typed"^^:dt"#, r#""lang"@en"#, r#""langdir"@en--ltr"#, r#""""very\nlong""""#, "<<( :s2 :p2 :o2 )>>"]
)]
fn triple_term(s: &str, p: &str, o: &str) -> TestResult {
    let asserted = format!("PREFIX : <x:> {s} {p} {o} .");
    let triple_term = format!("PREFIX : <x:> : a <<( {s} {p} {o} )>> .");

    let p = TurtleParser::new();
    let g1 = p
        .parse_str(&asserted)
        .collect_triples::<Vec<[SimpleTerm<'static>; 3]>>()?;
    let g2 = p
        .parse_str(&triple_term)
        .collect_triples::<Vec<[SimpleTerm<'static>; 3]>>()?;
    assert_eq!(g1.len(), 1);
    assert_eq!(g2.len(), 1);
    let [s1, p1, o1] = &g1[0];
    let [s2, p2, o2] = g2[0][2].triple().unwrap();
    assert_eq!([s1, p1, o1], [s2, p2, o2]);
    Ok(())
}

#[test_matrix(
    ["<x:s>", ":s", "[\n]", "_:bs"],
    ["<x:p>", ":p", "a"],
    ["<x:o>", ":o", "[\n]", "_:bo", "1", "1.2", "1.2e3", "true", r#""simple""#, r#""typed"^^:dt"#, r#""lang"@en"#, r#""langdir"@en--ltr"#, r#""""very\nlong""""#, "<<( :s2 :p2 :o2 )>>"]
)]
fn reified_triple(s: &str, p: &str, o: &str) -> TestResult {
    let asserted = format!("PREFIX : <x:> {s} {p} {o} .");
    let triple_term = format!("PREFIX : <x:> << {s} {p} {o} ~ :r >> .");

    let ts = TurtleParser::new();
    let g1 = ts
        .parse_str(&asserted)
        .collect_triples::<Vec<[SimpleTerm<'static>; 3]>>()?;
    let g2 = ts
        .parse_str(&triple_term)
        .collect_triples::<Vec<[SimpleTerm<'static>; 3]>>()?;
    assert_eq!(g1.len(), 1);
    assert_eq!(g2.len(), 1);
    let [s1, p1, o1] = &g1[0];
    let [s2, p2, o2] = g2[0][2].triple().unwrap();
    assert_eq!([s1, p1, o1], [s2, p2, o2]);
    Ok(())
}

#[test]
fn nested_reified_triple() -> TestResult {
    let input = r"
        PREFIX : <x:>
        << << :a :b :c ~ :d >> :e << :f :g :h ~ :i >> ~ :j >> :k :l .
    ";
    let expected = r"
        PREFIX : <x:>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

        :d rdf:reifies <<( :a :b :c )>>.
        :i rdf:reifies <<( :f :g :h )>>.
        :j rdf:reifies <<( :d :e :i )>>.
        :j :k :l.
    ";

    let ts = TurtleParser::new();
    let g1 = ts
        .parse_str(input)
        .collect_triples::<Vec<[SimpleTerm<'static>; 3]>>()?;
    let g2 = ts
        .parse_str(expected)
        .collect_triples::<Vec<[SimpleTerm<'static>; 3]>>()?;
    assert_eq!(g1, g2);
    Ok(())
}

#[test_case(r"[] ", 0)]
#[test_case(r"[] a ", 0)]
#[test_case(r"[] a 1 ", 1; "1 triple with space")]
#[test_case(r"[] a 1, 2 ", 2; "2 triples with comma")]
#[test_case(r"[] a 1; :p 2 ", 2; "2 triples with semicolon")]
#[test_case(r"[] a 1;; :p 2 ", 2; "2 triples with repeated semicolon")]
#[test_case(r"[] a 1; :p 2 ;.", 2; "2 triples with spurious semicolon at end")]
#[test_case(r"[] a 1. :s :p 2 ", 2; "2 triples with period")]
#[test_case(r"[] a 1 {|", 2; "1p1 triples with annotation")]
#[test_case(r"[] a 1 {| a 3", 3; "1p2 triples with annotation")]
#[test_case(r"[] a 1 {| a 3, 4", 4; "1p3 triples with annotation")]
#[test_case(r"[] a 1 {| a 3, 4; :p 5", 5; "1p4 triples with annotation")]
#[test_case(r"[] a 1 {| a 3, 4;; :p 5", 5; "1p4 triples with annotation with repeated semicolon")]
#[test_case(r"[] a 1, 2 {| a 4", 4; "2p2 triples with annotation")]
#[test_case(r"[] a 1, 2 {| a 4 ; |}", 4; "2p2 triples with annotation with spurious semicolon at end")]
#[test_case(r"[] a 1 ~.", 2; "2 triples with empty reifier")]
#[test_case(r"[] a 1 ~ <ex:r>", 2; "2 triples with IRI reifier")]
#[test_case(r"[] a 1 ~ :", 2; "2 triples with prefixed reifier")]
#[test_case(r"[] a 1 ~ _:r", 2; "2 triples with blank reifier")]
#[test_case(r"[] a 1 ~ []", 2; "2 triples with anon reifier")]
#[test_case(r"[] a 1 ~ :1 ~ :2", 3; "3 triples with prefixed reifiers")]
#[test_case(r"[] a 1 ~ ~,", 3; "3 triples with empty reifiers")]
#[test_case(r"[] a 1 {| a 3", 3; "3 triples with implicit reifier and annotation")]
#[test_case(r"[] a 1 ~ {| a 3", 3; "3 triples with empty reifier and annotation")]
#[test_case(r"[] a 1 ~ :r2 {| a 3", 3; "3 triples with reifier and annotation")]
#[test_case(r"[] a 1 ~ :r2 ~ [] {| a 4", 4; "3 triples with 2 reifiers and annotation")]
#[test_case(r"[] a 1 ~ :r2 {| a 3 |} ~ :r4 {| a 5", 5; "5 triples with 2 annotated reifiers")]
#[test_case(r"[:p :o] :p2 :o2", 2; "2 triples mixing inside and outside bnode prop list")]
#[test_case(r"[:p :o].", 1; "1 triple with standalone bnode prop list")]
#[test_case(r"[:p :o; :q :r].", 2; "2 triples with standalone bnode prop list")]
#[test_case(r"[:p :o;; :q :r].", 2; "2 triples with standalone bnode prop list with repeated semicolon")]
#[test_case(r"[:p :o; :q :r;].", 2; "2 triples with standalone bnode prop list with spurious semicolon at end")]
#[test_case(r"[] a 1 {| a 3 |}.", 3; "1p2 triples with annotation and period")]
#[test_case(r"[] a '1'.", 1; "1 triple with simple literal")]
#[test_case(r"[] a '1'^^:.", 1; "1 triple with typed literal")]
#[test_case(r"[] a '1'@en.", 1; "1 triple with langstring")]
#[test_case(r"[] a '''1'''.", 1; "1 triple with long literal untyped")]
#[test_case(r"[] a '''1'''^^:.", 1; "1 triple with long literal typed")]
#[test_case(r"[] a ().", 1; "1 triple with empty collection as object")]
#[test_case(r"(1) a 2.", 3; "3 triples with collection as subject")]
#[test_case(r"[] a (1).", 3; "3 triples with collection as object")]
#[test_case(r"(1 2) a 3.", 5; "5 triples with collection as subject")]
#[test_case(r"[] a (1 2).", 5; "5 triples with collection as object")]
#[test_case(r"(1 2.0 3e0 '4' <ex:5> :6 true ) a [].", 15; "15 triples with collection as object")]
#[test_case(r"[] a <<( :s :p :o )>>.", 1; "1 triple with triple term")]
#[test_case(r"<< :s :p :o >>.", 1; "1 triple with implicitly reified triple")]
#[test_case(r"<< :s :p :o ~ >>.", 1; "1 triple with empty reified triple")]
#[test_case(r"<< :s :p :o ~ :r >>.", 1; "1 triple with explicitly reified triple")]
#[test_case(r"<< :s :p :o >> :a :b", 2; "1p1 triples with implicitly reified triple")]
#[test_case(r"<< :s :p :o ~ >> :a :b", 2; "1p1 triples with empty reified triple")]
#[test_case(r"<< :s :p :o ~ :r >> :a :b", 2; "1p1 triples with explicitly reified triple")]
fn count_triples(input: &str, exp: usize) -> TestResult {
    for i in [input, &format!("{input}\n # this is a comment")] {
        let data = format!("PREFIX : <ex:> {i}");
        let mut got = 0;
        let mut ts = TurtleSource::<()>::new(());
        ts.parse_line(&data, |_| -> Result<(), Infallible> {
            got += 1;
            println!(">> emitted {got}");
            Ok(())
        })?;
        assert_eq!(got, exp)
    }
    Ok(())
}

#[test_case("empty")]
#[test_case("comment")]
#[test_case("version")]
#[test_case("triple i i i")]
#[test_case("triple b i i")]
#[test_case("triple i i b")]
#[test_case("triple b i b")]
#[test_case("triple i i l")]
#[test_case("triple b i l")]
#[test_case("triple i i ld")]
#[test_case("triple b i ld")]
#[test_case("triple i i ll")]
#[test_case("triple b i ll")]
#[test_case("triple i i lb")]
#[test_case("triple b i lb")]
#[test_case("triple i i t")]
#[test_case("triple b i t")]
#[test_case("escape")]
#[test_case("escape useless")]
#[test_case("factorized triples")]
#[test_case("pretty literals")]
#[test_case("unpretty literals")]
#[test_case("lists")]
#[test_case("subject list")]
#[test_case("malformed list")]
#[test_case("bnode cycles")]
#[test_case("reified subject")]
#[test_case("reified object")]
#[test_case("reified nested")]
#[test_case("annotation")]
#[test_case("annotation nested")]
#[test_case("anon in list")]
#[test_case("rdf:nil in reified triple")]
fn samples(name: &str) {
    static TESTS: LazyMap = ttl_samples();

    let (line, count) = TESTS.get(name).unwrap();
    let mut c = 0;
    let nt_parser = TurtleParser::new();
    nt_parser
        .parse_str(line)
        .for_each_triple(|t| {
            println!("{t:?}");
            c += 1;
        })
        .unwrap();
    debug_assert_eq!(c, *count)
}

type TestResult = Result<(), Box<dyn std::error::Error>>;

fn no_callback(_: [StashedTerm<'_>; 3]) -> Result<(), Infallible> {
    Ok(())
}
