#![allow(clippy::unnecessary_wraps)]
use std::{collections::HashSet, str::FromStr, sync::Arc};

use crate::{
    ResultTerm, SparqlQuery, SparqlWrapper,
    expression::{EvalResult, StringLiteral, StringLiteralRef},
    value::{SparqlNumber, SparqlValue, XsdDateTime},
};

use sophia_api::{
    ns::{rdf, xsd},
    sparql::{Query, SparqlDataset},
    term::{BaseDirection, FromTerm, IriRef, LanguageTag, Term},
};
use sophia_inmem::dataset::LightDataset;
use sophia_term::{ArcTerm, GenericLiteral};
use test_case::test_case;

#[test_case("tag:x")]
fn str_iri(arg: &str) -> TestResult {
    let iri = IriRef::new_unchecked(Arc::<str>::from(arg));
    let got = Some(super::str_iri(&iri));
    let exp = Some(EvalResult::from(Arc::<str>::from(arg)));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", "")]
#[test_case("chat", "en--ltr", "")]
fn str_literal(lex: &str, tag: &str, dt: &str) -> TestResult {
    let lit = if tag.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.into()))
    } else {
        let (lang, dir) = parse_lang(tag);
        GenericLiteral::LanguageString(lex.into(), lang, dir)
    };
    let got = Some(super::str_literal(lit));
    let exp = Some(EvalResult::from(Arc::<str>::from(lex)));
    assert!(eval_eq(got, exp));
    Ok(())
}

fn parse_lang(tag: &str) -> (LanguageTag<Arc<str>>, Option<BaseDirection>) {
    let (lang, dir) = tag.split_once("--").unwrap_or((tag, ""));
    let dir = match dir {
        "" => None,
        txt => Some(txt.parse().unwrap()),
    };
    (LanguageTag::new_unchecked(lang.into()), dir)
}

#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", "")]
#[test_case("chat", "en--ltr", "")]
fn lang(lex: &str, tag: &str, dt: &str) -> TestResult {
    let lit = if tag.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.into()))
    } else {
        let (lang, dir) = dbg!(parse_lang(tag));
        GenericLiteral::LanguageString(lex.into(), lang, dir)
    };
    let got = Some(super::lang(lit));
    let exp = Some(EvalResult::from(Arc::<str>::from(&tag[..2.min(tag.len())])));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", "")]
#[test_case("chat", "en--ltr", "")]
fn lang_dir(lex: &str, tag: &str, dt: &str) -> TestResult {
    let lit = if tag.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.into()))
    } else {
        let (lang, dir) = dbg!(parse_lang(tag));
        GenericLiteral::LanguageString(lex.into(), lang, dir)
    };
    let got = Some(super::lang_dir(lit));
    let exp = Some(EvalResult::from(Arc::<str>::from(&tag[4.min(tag.len())..])));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[allow(clippy::needless_pass_by_value)]
#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", rdf::langString)]
#[test_case("chat", "en--ltr", rdf::dirLangString)]
fn datatype<T: ToString>(lex: &str, tag: &str, dt: T) -> TestResult {
    let lit = if tag.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.to_string().into()))
    } else {
        let (lang, dir) = parse_lang(tag);
        GenericLiteral::LanguageString(lex.into(), lang, dir)
    };
    let got = Some(super::datatype(lit));
    let exp = Some(EvalResult::from(IriRef::new_unchecked(Arc::<str>::from(
        dt.to_string(),
    ))));
    assert!(eval_eq(dbg!(got), dbg!(exp)));
    Ok(())
}

#[test]
fn bnode1() -> TestResult {
    let got = super::bnode1(&Arc::from("foo"));
    assert!(got.as_term().is_blank_node());
    Ok(())
}

#[test]
fn bnode0() -> TestResult {
    const N: usize = 5;
    let mut set = HashSet::new();
    for _ in 1..=N {
        let EvalResult::Term(term) = super::bnode0() else {
            panic!();
        };
        let bnid = term.bnode_id().unwrap().unwrap().to_string();
        set.insert(bnid);
    }
    assert_eq!(set.len(), N);
    Ok(())
}

#[test]
fn rand_all_diff() -> TestResult {
    const N: usize = 5;
    let mut set = HashSet::new();
    for _ in 1..=N {
        let EvalResult::Value(SparqlValue::Number(Some(SparqlNumber::Double(val)))) = super::rand()
        else {
            panic!();
        };
        assert!((0.0..1.0).contains(&val));
        set.insert(val.to_string());
    }
    assert_eq!(set.len(), N);
    Ok(())
}

// See https://www.w3.org/TR/sparql12-query/#func-concat
#[test_case(vec!["foo", "bar"], "foobar")]
#[test_case(vec!["foo@en", "bar@en"], "foobar@en")]
#[test_case(vec!["foo@en--ltr", "bar@en--ltr"], "foobar@en--ltr")]
#[test_case(vec!["foo@en", "bar"], "foobar")]
#[test_case(vec!["foo", "bar@en"], "foobar")]
#[test_case(vec!["foo@en", "bar@es"], "foobar")]
#[test_case(vec!["foo@en", "bar@en--ltr"], "foobar")]
#[test_case(vec!["abc"], "abc")]
#[test_case(vec!["abc@en"], "abc@en")]
#[test_case(vec!["abc@en--ltr"], "abc@en--ltr")]
#[test_case(vec![], "")]
// More arguments
#[test_case(vec!["a", "b", "c"], "abc")]
#[test_case(vec!["a", "b", "c", "d"], "abcd")]
fn concat(input: Vec<&str>, exp: &str) {
    let input: Vec<_> = input.into_iter().map(txt2pair).collect();
    let args: Vec<_> = input.iter().map(pair2ref).collect();

    let exp = Some(EvalResult::from(txt2pair(exp)));
    assert!(eval_eq(Some(super::concat(&args)), exp));
}

fn txt2pair(txt: &str) -> StringLiteral {
    let (lex, tag) = txt.split_once('@').unwrap_or((txt, ""));
    (
        Arc::from(lex),
        if tag.is_empty() {
            None
        } else {
            let (lang, dir) = tag.split_once("--").unwrap_or((tag, ""));
            let lang = LanguageTag::new_unchecked(Arc::from(lang));
            let dir = (!dir.is_empty()).then(|| dir.parse().unwrap());
            Some((lang, dir))
        },
    )
}

fn pair2ref(pair: &StringLiteral) -> StringLiteralRef<'_> {
    (&pair.0, pair.1.as_ref().map(|(lang, dir)| (lang, *dir)))
}

#[test_case("en", "*", true)]
#[test_case("EN", "en", true)]
#[test_case("en-UK", "en", true)]
#[test_case("en-uk", "en-UK", true)]
#[test_case("en-US", "en-UK", false)]
#[test_case("en", "en-UK", false)]
#[test_case("es", "en", false)]
#[test_case("enx", "en", false)]
fn lang_matches(tag: &str, range: &str, exp: bool) -> TestResult {
    let tag = Arc::<str>::from(tag);
    let range = Arc::<str>::from(range);
    let exp = Some(EvalResult::from(exp));
    assert!(eval_eq(super::lang_matches(&tag, &range), exp.clone()));
    Ok(())
}

#[test_case("foobar", 4.0, None, Some("bar"))]
#[test_case("foobar@en", 4.0, None, Some("bar@en"))]
#[test_case("foobar@en--ltr", 4.0, None, Some("bar@en--ltr"))]
#[test_case("foobar", 4.0, Some(1.0), Some("b"))]
#[test_case("foobar@en", 4.0, Some(1.0), Some("b@en"))]
#[test_case("foobar@en--ltr", 4.0, Some(1.0), Some("b@en--ltr"))]
#[test_case("foobar", -2.0, Some(6.0), Some("foo"))]
#[test_case("foobar", -2.0, None, Some("foobar"))]
#[test_case("foobar", 4.0, Some(0.0), Some(""))]
#[test_case("foobar", 4.0, Some(-1.0), Some(""))]
#[test_case("foobar", 0.9, Some(1.1), Some("f"))]
#[test_case("foobar", 1.1, Some(0.9), Some("f"))]
#[test_case("食べ物", 1.0, Some(1.0), Some("食"))]
#[test_case("食べ物", 2.0, None, Some("べ物"))]
#[test_case(
    "פעילות הבינאום, W3C@hb--rtl",
    5.0,
    None,
    Some("ות הבינאום, W3C@hb--rtl")
)]
#[test_case("פעילות הבינאום, W3C@hb--rtl", 1.0, Some(2.0), Some("פע@hb--rtl"))]
fn sub_str(source: &str, start: f64, length: Option<f64>, exp: Option<&str>) -> TestResult {
    let pair = txt2pair(source);
    let source = pair2ref(&pair);
    let exp = exp.map(|txt| EvalResult::from(txt2pair(txt)));
    assert!(eval_eq(super::sub_str(source, start, length), exp));
    Ok(())
}

#[test_case("foobar", 6)]
#[test_case("foobar@en", 6)]
#[test_case("foobar@en--ltr", 6)]
#[test_case("é", 1)]
#[test_case("é@fr", 1)]
#[test_case("é@fr--ltr", 1)]
#[test_case("⛄", 1; "snowman")]
#[test_case("⛄@en", 1; "snowman en")]
#[test_case("⛄@en--rtl", 1; "snowman en--rtl")]
fn str_len(string: &str, exp: isize) -> TestResult {
    let pair = txt2pair(string);
    let string = &pair.0;
    let exp = EvalResult::from(SparqlNumber::from(exp));
    assert!(eval_eq(Some(super::str_len(string)), Some(exp)));
    Ok(())
}

#[test_case("abcd", "X", "Z", None, Some("abcd"))]
#[test_case("abcd@en", "X", "Z", None, Some("abcd@en"))]
#[test_case("abcd@en--ltr", "X", "Z", None, Some("abcd@en--ltr"))]
#[test_case("abcd", "b", "Z", None, Some("aZcd"))]
#[test_case("abcd@en", "b", "Z", None, Some("aZcd@en"))]
#[test_case("abcd@en--ltr", "b", "Z", None, Some("aZcd@en--ltr"))]
#[test_case("abcb", "B", "Z", Some("i"), Some("aZcZ"))]
#[test_case("abcb", "B.", "Z", Some("i"), Some("aZb"))]
#[test_case("abracadabra", "bra", "*", None, Some("a*cada*"))]
#[test_case("abracadabra", "a.*a", "*", None, Some("*"))]
#[test_case("abracadabra", "a", "", None, Some("brcdbr"))]
#[test_case("abracadabra", ".*?", "$1", None, None)]
#[test_case("AAAA", "A+", "b", None, Some("b"))]
#[test_case("AAAA", "A+?", "b", None, Some("bbbb"))]
#[test_case("darted", "^(.*?)d(.*)$", "$1c$2", None, Some("carted"))]
fn replace(
    arg: &str,
    pattern: &str,
    replacement: &str,
    flags: Option<&str>,
    exp: Option<&str>,
) -> TestResult {
    let pair = txt2pair(arg);
    let arg = pair2ref(&pair);
    let flags = flags.map(Arc::<str>::from);
    let exp = exp.map(|txt| EvalResult::from(txt2pair(txt)));
    assert!(eval_eq(
        super::replace(arg, pattern, replacement, flags.as_ref()),
        exp
    ));
    Ok(())
}

#[test_case("foo", "FOO")]
#[test_case("foo@en", "FOO@en")]
#[test_case("foo@en--ltr", "FOO@en--ltr")]
#[test_case("FOO", "FOO"; "noop")]
#[test_case("FOO@en", "FOO@en"; "noop en")]
#[test_case("FOO@en--ltr", "FOO@en--ltr"; "noop en--ltr")]
#[test_case("fooBAR 1!⛄xY", "FOOBAR 1!⛄XY")]
#[test_case("fooBAR 1!⛄xY@en", "FOOBAR 1!⛄XY@en")]
#[test_case("fooBAR 1!⛄xY@en--ltr", "FOOBAR 1!⛄XY@en--ltr")]
#[test_case("àéîôù", "ÀÉÎÔÙ"; "accents")]
#[test_case("àéîôù@fr", "ÀÉÎÔÙ@fr"; "accents fr")]
#[test_case("àéîôù@fr--ltr", "ÀÉÎÔÙ@fr--ltr"; "accents fr--ltr")]
#[test_case("ﬀ ŉ", "FF ʼN"; "multichar")]
#[test_case("ﬀ ŉ@en", "FF ʼN@en"; "multichar en")]
#[test_case("ﬀ ŉ@en--ltr", "FF ʼN@en--ltr"; "multichar en--ltr")]
fn u_case(string: &str, exp: &str) -> TestResult {
    let pair = txt2pair(string);
    let source = pair2ref(&pair);
    let exp = EvalResult::from(txt2pair(exp));
    assert!(eval_eq(Some(super::u_case(source)), Some(exp)));
    Ok(())
}

#[test_case("FOO", "foo")]
#[test_case("FOO@en", "foo@en")]
#[test_case("FOO@en--ltr", "foo@en--ltr")]
#[test_case("foo", "foo"; "noop")]
#[test_case("foo@en", "foo@en"; "noop en")]
#[test_case("foo@en--ltr", "foo@en--ltr"; "noop en--ltr")]
#[test_case("fooBAR 1!⛄xY", "foobar 1!⛄xy")]
#[test_case("fooBAR 1!⛄xY@en", "foobar 1!⛄xy@en")]
#[test_case("fooBAR 1!⛄xY@en--ltr", "foobar 1!⛄xy@en--ltr")]
#[test_case("ÀÉÎÔÙ", "àéîôù"; "accents")]
#[test_case("ÀÉÎÔÙ@fr", "àéîôù@fr"; "accents fr")]
#[test_case("ÀÉÎÔÙ@fr--ltr", "àéîôù@fr--ltr"; "accents fr--ltr")]
fn l_case(string: &str, exp: &str) -> TestResult {
    let pair = txt2pair(string);
    let source = pair2ref(&pair);
    let exp = EvalResult::from(txt2pair(exp));
    assert!(eval_eq(Some(super::l_case(source)), Some(exp)));
    Ok(())
}

#[test_case("Los Angeles", "Los%20Angeles")]
#[test_case("Los Angeles@en", "Los%20Angeles")]
#[test_case("Los Angeles@en--ltr", "Los%20Angeles")]
#[test_case(
    "http://www.example.com/00/Weather/CA/Los%20Angeles#ocean",
    "http%3A%2F%2Fwww.example.com%2F00%2FWeather%2FCA%2FLos%2520Angeles%23ocean"
)]
#[test_case("~bébé", "~b%C3%A9b%C3%A9")]
#[test_case("100% organic", "100%25%20organic")]
#[test_case("⛄", "%E2%9B%84")]
fn encode_for_uri(string: &str, exp: &str) -> TestResult {
    let pair = txt2pair(string);
    let source = &pair.0;
    let exp = EvalResult::from(txt2pair(exp));
    assert!(eval_eq(Some(super::encode_for_uri(source)), Some(exp)));
    Ok(())
}

#[test_case("", "", Some(true))]
#[test_case("@en", "@en", Some(true))]
#[test_case("@en--ltr", "@en--ltr", Some(true))]
#[test_case("@en", "", Some(true))]
#[test_case("@en--ltr", "", Some(true))]
#[test_case("foobar", "", Some(true))]
#[test_case("foobar", "foo", Some(true))]
#[test_case("foobar", "oba", Some(true))]
#[test_case("foobar", "bar", Some(true))]
#[test_case("foobar@en", "@en", Some(true))]
#[test_case("foobar@en", "foo@en", Some(true))]
#[test_case("foobar@en", "oba@en", Some(true))]
#[test_case("foobar@en", "bar@en", Some(true))]
#[test_case("foobar@en", "", Some(true))]
#[test_case("foobar@en", "foo", Some(true))]
#[test_case("foobar@en", "oba", Some(true))]
#[test_case("foobar@en", "bar", Some(true))]
#[test_case("foobar@en--ltr", "@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "foo@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "oba@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "bar@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "", Some(true))]
#[test_case("foobar@en--ltr", "foo", Some(true))]
#[test_case("foobar@en--ltr", "oba", Some(true))]
#[test_case("foobar@en--ltr", "bar", Some(true))]
#[test_case("", "foo", Some(false))]
#[test_case("@en", "foo@en", Some(false))]
#[test_case("@en--ltr", "foo@en--ltr", Some(false))]
#[test_case("@en", "foo", Some(false))]
#[test_case("@en--ltr", "foo", Some(false))]
#[test_case("foobar", "BAR", Some(false))]
#[test_case("foobar", "baz", Some(false))]
#[test_case("foobar@en", "BAR@en", Some(false))]
#[test_case("foobar@en", "baz@en", Some(false))]
#[test_case("foobar@en", "BAR", Some(false))]
#[test_case("foobar@en", "baz", Some(false))]
#[test_case("foobar@en--ltr", "BAR@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "baz@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "BAR", Some(false))]
#[test_case("foobar@en--ltr", "baz", Some(false))]
#[test_case("", "@fr", None)]
#[test_case("", "@fr--ltr", None)]
#[test_case("foobar", "bar@fr", None)]
#[test_case("foobar", "bar@fr--ltr", None)]
#[test_case("foobar", "baz@fr", None)]
#[test_case("foobar", "baz@fr--ltr", None)]
#[test_case("@en", "@fr", None)]
#[test_case("@en", "@en--ltr", None)]
#[test_case("@en--ltr", "@en", None)]
#[test_case("@en--ltr", "@en--rtl", None)]
#[test_case("foobar@en", "bar@fr", None)]
#[test_case("foobar@en", "bar@en--ltr", None)]
#[test_case("foobar@en", "baz@fr", None)]
#[test_case("foobar@en", "baz@en--ltr", None)]
#[test_case("foobar@en--ltr", "bar@en", None)]
#[test_case("foobar@en--ltr", "baz@en", None)]
#[test_case("foobar@en--ltr", "bar@en--rtl", None)]
#[test_case("foobar@en--ltr", "baz@en--rtl", None)]
fn contains(heystack: &str, needle: &str, exp: Option<bool>) -> TestResult {
    let pair1 = txt2pair(heystack);
    let heystack = pair2ref(&pair1);
    let pair2 = txt2pair(needle);
    let needle = pair2ref(&pair2);
    let exp = exp.map(EvalResult::from);
    assert!(eval_eq(super::contains(heystack, needle), exp));
    Ok(())
}

#[test_case("", "", Some(true))]
#[test_case("@en", "@en", Some(true))]
#[test_case("@en--ltr", "@en--ltr", Some(true))]
#[test_case("@en", "", Some(true))]
#[test_case("@en--ltr", "", Some(true))]
#[test_case("foobar", "", Some(true))]
#[test_case("foobar", "foo", Some(true))]
#[test_case("foobar", "oba", Some(false))]
#[test_case("foobar", "bar", Some(false))]
#[test_case("foobar@en", "@en", Some(true))]
#[test_case("foobar@en", "foo@en", Some(true))]
#[test_case("foobar@en", "oba@en", Some(false))]
#[test_case("foobar@en", "bar@en", Some(false))]
#[test_case("foobar@en", "", Some(true))]
#[test_case("foobar@en", "foo", Some(true))]
#[test_case("foobar@en", "oba", Some(false))]
#[test_case("foobar@en", "bar", Some(false))]
#[test_case("foobar@en--ltr", "@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "foo@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "oba@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "bar@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "", Some(true))]
#[test_case("foobar@en--ltr", "foo", Some(true))]
#[test_case("foobar@en--ltr", "oba", Some(false))]
#[test_case("foobar@en--ltr", "bar", Some(false))]
#[test_case("", "foo", Some(false))]
#[test_case("@en", "foo@en", Some(false))]
#[test_case("@en--ltr", "foo@en--ltr", Some(false))]
#[test_case("@en", "foo", Some(false))]
#[test_case("@en--ltr", "foo", Some(false))]
#[test_case("foobar", "FOO", Some(false))]
#[test_case("foobar", "baz", Some(false))]
#[test_case("foobar@en", "FOO@en", Some(false))]
#[test_case("foobar@en", "baz@en", Some(false))]
#[test_case("foobar@en", "FOO", Some(false))]
#[test_case("foobar@en", "baz", Some(false))]
#[test_case("foobar@en--ltr", "FOO@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "baz@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "FOO", Some(false))]
#[test_case("foobar@en--ltr", "baz", Some(false))]
#[test_case("", "@fr", None)]
#[test_case("", "@fr--ltr", None)]
#[test_case("foobar", "bar@fr", None)]
#[test_case("foobar", "bar@fr--ltr", None)]
#[test_case("foobar", "baz@fr", None)]
#[test_case("foobar", "baz@fr--ltr", None)]
#[test_case("@en", "@fr", None)]
#[test_case("@en", "@en--ltr", None)]
#[test_case("@en--ltr", "@en", None)]
#[test_case("@en--ltr", "@en--rtl", None)]
#[test_case("foobar@en", "foo@fr", None)]
#[test_case("foobar@en", "foo@en--ltr", None)]
#[test_case("foobar@en", "baz@fr", None)]
#[test_case("foobar@en", "baz@en--ltr", None)]
#[test_case("foobar@en--ltr", "foo@en", None)]
#[test_case("foobar@en--ltr", "foo@en--rtl", None)]
#[test_case("foobar@en--ltr", "baz@en", None)]
#[test_case("foobar@en--ltr", "baz@en--rtl", None)]
fn strstarts(heystack: &str, needle: &str, exp: Option<bool>) -> TestResult {
    let pair1 = txt2pair(heystack);
    let heystack = pair2ref(&pair1);
    let pair2 = txt2pair(needle);
    let needle = pair2ref(&pair2);
    let exp = exp.map(EvalResult::from);
    assert!(eval_eq(super::strstarts(heystack, needle), exp));
    Ok(())
}

#[test_case("", "", Some(true))]
#[test_case("@en", "@en", Some(true))]
#[test_case("@en--ltr", "@en--ltr", Some(true))]
#[test_case("@en", "", Some(true))]
#[test_case("@en--ltr", "", Some(true))]
#[test_case("foobar", "", Some(true))]
#[test_case("foobar", "foo", Some(false))]
#[test_case("foobar", "oba", Some(false))]
#[test_case("foobar", "bar", Some(true))]
#[test_case("foobar@en", "@en", Some(true))]
#[test_case("foobar@en", "foo@en", Some(false))]
#[test_case("foobar@en", "oba@en", Some(false))]
#[test_case("foobar@en", "bar@en", Some(true))]
#[test_case("foobar@en", "", Some(true))]
#[test_case("foobar@en", "foo", Some(false))]
#[test_case("foobar@en", "oba", Some(false))]
#[test_case("foobar@en", "bar", Some(true))]
#[test_case("foobar@en--ltr", "@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "foo@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "oba@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "bar@en--ltr", Some(true))]
#[test_case("foobar@en--ltr", "", Some(true))]
#[test_case("foobar@en--ltr", "foo", Some(false))]
#[test_case("foobar@en--ltr", "oba", Some(false))]
#[test_case("foobar@en--ltr", "bar", Some(true))]
#[test_case("", "foo", Some(false))]
#[test_case("@en", "foo@en", Some(false))]
#[test_case("@en--ltr", "foo@en--ltr", Some(false))]
#[test_case("@en", "foo", Some(false))]
#[test_case("@en--ltr", "foo", Some(false))]
#[test_case("foobar", "BAR", Some(false))]
#[test_case("foobar", "baz", Some(false))]
#[test_case("foobar@en", "BAR@en", Some(false))]
#[test_case("foobar@en", "baz@en", Some(false))]
#[test_case("foobar@en", "BAR", Some(false))]
#[test_case("foobar@en", "baz", Some(false))]
#[test_case("foobar@en--ltr", "BAR@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "baz@en--ltr", Some(false))]
#[test_case("foobar@en--ltr", "BAR", Some(false))]
#[test_case("foobar@en--ltr", "baz", Some(false))]
#[test_case("", "@fr", None)]
#[test_case("", "@fr--ltr", None)]
#[test_case("foobar", "bar@fr", None)]
#[test_case("foobar", "bar@fr--ltr", None)]
#[test_case("foobar", "baz@fr", None)]
#[test_case("foobar", "baz@fr--ltr", None)]
#[test_case("@en", "@fr", None)]
#[test_case("@en", "@en--ltr", None)]
#[test_case("@en--ltr", "@en", None)]
#[test_case("@en--ltr", "@en--rtl", None)]
#[test_case("foobar@en", "bar@fr", None)]
#[test_case("foobar@en", "bar@en--ltr", None)]
#[test_case("foobar@en", "baz@fr", None)]
#[test_case("foobar@en", "baz@en--ltr", None)]
#[test_case("foobar@en--ltr", "bar@en", None)]
#[test_case("foobar@en--ltr", "bar@en--rtl", None)]
#[test_case("foobar@en--ltr", "baz@en", None)]
#[test_case("foobar@en--ltr", "baz@en--rtl", None)]
fn strends(heystack: &str, needle: &str, exp: Option<bool>) -> TestResult {
    let pair1 = txt2pair(heystack);
    let heystack = pair2ref(&pair1);
    let pair2 = txt2pair(needle);
    let needle = pair2ref(&pair2);
    let exp = exp.map(EvalResult::from);
    assert!(eval_eq(super::strends(heystack, needle), exp));
    Ok(())
}

#[test_case("", "", Some(""))]
#[test_case("", "a", Some(""))]
#[test_case("@en", "@en", Some("@en"))]
#[test_case("@en", "a@en", Some(""))]
#[test_case("@en", "", Some("@en"))]
#[test_case("@en", "a", Some(""))]
#[test_case("@en--ltr", "@en--ltr", Some("@en--ltr"))]
#[test_case("@en--ltr", "a@en--ltr", Some(""))]
#[test_case("@en--ltr", "", Some("@en--ltr"))]
#[test_case("@en--ltr", "a", Some(""))]
#[test_case("abcbde", "", Some(""))]
#[test_case("abcbde", "B", Some(""))]
#[test_case("abcbde", "b", Some("a"))]
#[test_case("abcbde", "bd", Some("abc"))]
#[test_case("abcbde", "xyz", Some(""))]
#[test_case("abcbde@en", "@en", Some("@en"))]
#[test_case("abcbde@en", "B@en", Some(""))]
#[test_case("abcbde@en", "b@en", Some("a@en"))]
#[test_case("abcbde@en", "bd@en", Some("abc@en"))]
#[test_case("abcbde@en", "xyz@en", Some(""))]
#[test_case("abcbde@en", "", Some("@en"))]
#[test_case("abcbde@en", "B", Some(""))]
#[test_case("abcbde@en", "b", Some("a@en"))]
#[test_case("abcbde@en", "bd", Some("abc@en"))]
#[test_case("abcbde@en", "xyz", Some(""))]
#[test_case("abcbde@en--ltr", "@en--ltr", Some("@en--ltr"))]
#[test_case("abcbde@en--ltr", "B@en--ltr", Some(""))]
#[test_case("abcbde@en--ltr", "b@en--ltr", Some("a@en--ltr"))]
#[test_case("abcbde@en--ltr", "bd@en--ltr", Some("abc@en--ltr"))]
#[test_case("abcbde@en--ltr", "xyz@en--ltr", Some(""))]
#[test_case("abcbde@en--ltr", "", Some("@en--ltr"))]
#[test_case("abcbde@en--ltr", "B", Some(""))]
#[test_case("abcbde@en--ltr", "b", Some("a@en--ltr"))]
#[test_case("abcbde@en--ltr", "bd", Some("abc@en--ltr"))]
#[test_case("abcbde@en--ltr", "xyz", Some(""))]
#[test_case("abcbde", "b@fr", None)]
#[test_case("abcbde", "b@fr--ltr", None)]
#[test_case("abcbde@en", "b@fr", None)]
#[test_case("abcbde@en", "b@en--ltr", None)]
#[test_case("abcbde@en--ltr", "b@en", None)]
#[test_case("abcbde@en--ltr", "b@en--rtl", None)]
fn strbefore(heystack: &str, needle: &str, exp: Option<&str>) -> TestResult {
    let pair1 = txt2pair(heystack);
    let heystack = pair2ref(&pair1);
    let pair2 = txt2pair(needle);
    let needle = pair2ref(&pair2);
    let exp = exp.map(txt2pair).map(EvalResult::from);
    assert!(eval_eq(super::strbefore(heystack, needle), exp));
    Ok(())
}

#[test_case("", "", Some(""))]
#[test_case("", "a", Some(""))]
#[test_case("@en", "@en", Some("@en"))]
#[test_case("@en", "a@en", Some(""))]
#[test_case("@en", "", Some("@en"))]
#[test_case("@en", "a", Some(""))]
#[test_case("@en--ltr", "@en--ltr", Some("@en--ltr"))]
#[test_case("@en--ltr", "a@en--ltr", Some(""))]
#[test_case("@en--ltr", "", Some("@en--ltr"))]
#[test_case("@en--ltr", "a", Some(""))]
#[test_case("abcbde", "", Some("abcbde"))]
#[test_case("abcbde", "B", Some(""))]
#[test_case("abcbde", "b", Some("cbde"))]
#[test_case("abcbde", "bd", Some("e"))]
#[test_case("abcbde", "xyz", Some(""))]
#[test_case("abcbde@en", "@en", Some("abcbde@en"))]
#[test_case("abcbde@en", "B@en", Some(""))]
#[test_case("abcbde@en", "b@en", Some("cbde@en"))]
#[test_case("abcbde@en", "bd@en", Some("e@en"))]
#[test_case("abcbde@en", "xyz@en", Some(""))]
#[test_case("abcbde@en", "", Some("abcbde@en"))]
#[test_case("abcbde@en", "B", Some(""))]
#[test_case("abcbde@en", "b", Some("cbde@en"))]
#[test_case("abcbde@en", "bd", Some("e@en"))]
#[test_case("abcbde@en", "xyz", Some(""))]
#[test_case("abcbde@en--ltr", "@en--ltr", Some("abcbde@en--ltr"))]
#[test_case("abcbde@en--ltr", "B@en--ltr", Some(""))]
#[test_case("abcbde@en--ltr", "b@en--ltr", Some("cbde@en--ltr"))]
#[test_case("abcbde@en--ltr", "bd@en--ltr", Some("e@en--ltr"))]
#[test_case("abcbde@en--ltr", "xyz@en--ltr", Some(""))]
#[test_case("abcbde@en--ltr", "", Some("abcbde@en--ltr"))]
#[test_case("abcbde@en--ltr", "B", Some(""))]
#[test_case("abcbde@en--ltr", "b", Some("cbde@en--ltr"))]
#[test_case("abcbde@en--ltr", "bd", Some("e@en--ltr"))]
#[test_case("abcbde@en--ltr", "xyz", Some(""))]
#[test_case("abcbde", "b@fr", None)]
#[test_case("abcbde", "b@fr--ltr", None)]
#[test_case("abcbde@en", "b@fr", None)]
#[test_case("abcbde@en", "b@en--ltr", None)]
#[test_case("abcbde@en--ltr", "b@en", None)]
#[test_case("abcbde@en--ltr", "b@en--rtl", None)]
fn strafter(heystack: &str, needle: &str, exp: Option<&str>) -> TestResult {
    let pair1 = txt2pair(heystack);
    let heystack = pair2ref(&pair1);
    let pair2 = txt2pair(needle);
    let needle = pair2ref(&pair2);
    let exp = exp.map(txt2pair).map(EvalResult::from);
    assert!(eval_eq(super::strafter(heystack, needle), exp));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", 2025)]
#[test_case("2025-01-18T12:34:56Z", 2025)]
#[test_case("2025-01-18T12:34:56+01:00", 2025)]
#[test_case("-0002-01-18T12:34:56", -2)]
#[test_case("-0002-01-18T12:34:56Z", -2)]
#[test_case("-0002-01-18T12:34:56+01:00", -2)]
#[test_case("2024-12-31T24:00:00", 2025)]
#[test_case("2024-12-31T24:00:00Z", 2025)]
#[test_case("2024-12-31T24:00:00+01:00", 2025)]
fn year(date_time: &str, exp: isize) -> TestResult {
    let date_time: XsdDateTime = date_time.parse()?;
    let exp = EvalResult::from(SparqlNumber::from(exp));
    assert!(eval_eq(Some(super::year(&date_time)), Some(exp)));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", 1)]
#[test_case("2025-02-18T12:34:56Z", 2)]
#[test_case("2025-03-18T12:34:56+01:00", 3)]
#[test_case("-0002-10-18T12:34:56", 10)]
#[test_case("-0002-11-18T12:34:56Z", 11)]
#[test_case("-0002-12-18T12:34:56+01:00", 12)]
#[test_case("2024-12-31T24:00:00", 1)]
#[test_case("2024-12-31T24:00:00Z", 1)]
#[test_case("2024-12-31T24:00:00+01:00", 1)]
fn month(date_time: &str, exp: isize) -> TestResult {
    let date_time: XsdDateTime = date_time.parse()?;
    let exp = EvalResult::from(SparqlNumber::from(exp));
    assert!(eval_eq(Some(super::month(&date_time)), Some(exp)));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", 18)]
#[test_case("2025-01-19T12:34:56Z", 19)]
#[test_case("2025-01-20T12:34:56+01:00", 20)]
#[test_case("-0002-01-21T12:34:56", 21)]
#[test_case("-0002-01-22T12:34:56Z", 22)]
#[test_case("-0002-01-23T12:34:56+01:00", 23)]
#[test_case("2024-12-31T24:00:00", 1)]
#[test_case("2024-12-31T24:00:00Z", 1)]
#[test_case("2024-12-31T24:00:00+01:00", 1)]
fn day(date_time: &str, exp: isize) -> TestResult {
    let date_time: XsdDateTime = date_time.parse()?;
    let exp = EvalResult::from(SparqlNumber::from(exp));
    assert!(eval_eq(Some(super::day(&date_time)), Some(exp)));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", 12)]
#[test_case("2025-01-18T13:34:56Z", 13)]
#[test_case("2025-01-18T14:34:56+01:00", 14)]
#[test_case("-0002-01-18T15:34:56", 15)]
#[test_case("-0002-01-18T16:34:56Z", 16)]
#[test_case("-0002-01-18T17:34:56+01:00", 17)]
#[test_case("2024-12-31T24:00:00", 0)]
#[test_case("2024-12-31T24:00:00Z", 0)]
#[test_case("2024-12-31T24:00:00+01:00", 0)]
fn hours(date_time: &str, exp: isize) -> TestResult {
    let date_time: XsdDateTime = date_time.parse()?;
    let exp = EvalResult::from(SparqlNumber::from(exp));
    assert!(eval_eq(Some(super::hours(&date_time)), Some(exp)));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", 34)]
#[test_case("2025-01-18T12:35:56Z", 35)]
#[test_case("2025-01-18T12:36:56+01:00", 36)]
#[test_case("-0002-01-18T12:01:56", 1)]
#[test_case("-0002-01-18T12:02:56Z", 2)]
#[test_case("-0002-01-18T12:03:56+01:00", 3)]
#[test_case("2024-12-31T24:00:00", 0)]
#[test_case("2024-12-31T24:00:00Z", 0)]
#[test_case("2024-12-31T24:00:00+01:00", 0)]
fn minutes(date_time: &str, exp: isize) -> TestResult {
    let date_time: XsdDateTime = date_time.parse()?;
    let exp = EvalResult::from(SparqlNumber::from(exp));
    assert!(eval_eq(Some(super::minutes(&date_time)), Some(exp)));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", "56")]
#[test_case("2025-01-18T12:34:57Z", "57.0")]
#[test_case("2025-01-18T12:34:58.9+01:00", "58.9")]
#[test_case("-0002-01-18T12:34:01.23", "1.23")]
#[test_case("-0002-01-18T12:34:02Z", "2")]
#[test_case("-0002-01-18T12:34:03+01:00", "3")]
#[test_case("2024-12-31T24:00:00", "0")]
#[test_case("2024-12-31T24:00:00Z", "0")]
#[test_case("2024-12-31T24:00:00+01:00", "0")]
fn seconds(date_time: &str, exp: &str) -> TestResult {
    let date_time: XsdDateTime = date_time.parse()?;
    let exp = EvalResult::from(SparqlNumber::from(
        bigdecimal::BigDecimal::from_str(exp).unwrap(),
    ));
    assert!(eval_eq(Some(super::seconds(&date_time)), Some(exp)));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", None)]
#[test_case("2025-01-18T12:34:57Z", Some("PT0S"))]
#[test_case("2025-01-18T12:34:58.9+01:00", Some("PT1H"))]
#[test_case("2025-01-18T12:34:58.9-00:02", Some("-PT2M"))]
#[test_case("2025-01-18T12:34:58.9+06:30", Some("PT6H30M"))]
#[test_case("2025-01-18T12:34:58.9-14:00", Some("-PT14H"))]
#[test_case("-0002-01-18T12:34:01.23", None)]
#[test_case("-0002-01-18T12:34:02Z", Some("PT0S"))]
#[test_case("-0002-01-18T12:34:03+01:00", Some("PT1H"))]
#[test_case("-0002-01-18T12:34:03-00:02", Some("-PT2M"))]
#[test_case("-0002-01-18T12:34:03+06:30", Some("PT6H30M"))]
#[test_case("-0002-01-18T12:34:03-14:00", Some("-PT14H"))]
#[test_case("2024-12-31T24:00:00", None)]
#[test_case("2024-12-31T24:00:00Z", Some("PT0S"))]
#[test_case("2024-12-31T24:00:00+01:00", Some("PT1H"))]
#[test_case("2024-12-31T24:00:00-00:02", Some("-PT2M"))]
#[test_case("2024-12-31T24:00:00+06:30", Some("PT6H30M"))]
#[test_case("2024-12-31T24:00:00-14:00", Some("-PT14H"))]
fn timezone(date_time: &str, exp: Option<&str>) -> TestResult {
    let date_time: XsdDateTime = date_time.parse()?;
    let exp = exp.map(|txt| {
        EvalResult::from(ResultTerm::from(ArcTerm::from_term(
            txt * xsd::dayTimeDuration,
        )))
    });
    assert!(eval_eq(super::timezone(&date_time), exp));
    Ok(())
}

#[test_case("2025-01-18T12:34:56", "")]
#[test_case("2025-01-18T12:34:57Z", "Z")]
#[test_case("2025-01-18T12:34:58.9+01:00", "+01:00")]
#[test_case("2025-01-18T12:34:58.9-00:02", "-00:02")]
#[test_case("2025-01-18T12:34:58.9+06:30", "+06:30")]
#[test_case("2025-01-18T12:34:58.9-14:00", "-14:00")]
#[test_case("-0002-01-18T12:34:01.23", "")]
#[test_case("-0002-01-18T12:34:02Z", "Z")]
#[test_case("-0002-01-18T12:34:03+01:00", "+01:00")]
#[test_case("-0002-01-18T12:34:03-00:02", "-00:02")]
#[test_case("-0002-01-18T12:34:03+06:30", "+06:30")]
#[test_case("-0002-01-18T12:34:03-14:00", "-14:00")]
#[test_case("2024-12-31T24:00:00", "")]
#[test_case("2024-12-31T24:00:00Z", "Z")]
#[test_case("2024-12-31T24:00:00+01:00", "+01:00")]
#[test_case("2024-12-31T24:00:00-00:02", "-00:02")]
#[test_case("2024-12-31T24:00:00+06:30", "+06:30")]
#[test_case("2024-12-31T24:00:00-14:00", "-14:00")]
fn tz(date_time: &str, exp: &str) -> TestResult {
    let date_time = Arc::from(date_time);
    let exp = EvalResult::from(Arc::from(exp));
    assert!(eval_eq(Some(super::tz(&date_time)), Some(exp)));
    Ok(())
}

#[test_case("abc", "900150983cd24fb0d6963f7d28e17f72")]
fn md5(arg: &str, exp: &str) -> TestResult {
    let exp = EvalResult::from(Arc::from(exp));
    assert!(eval_eq(Some(super::md5_(arg)), Some(exp)));
    Ok(())
}

#[test_case("abc", "a9993e364706816aba3e25717850c26c9cd0d89d")]
fn sha1(arg: &str, exp: &str) -> TestResult {
    let exp = EvalResult::from(Arc::from(exp));
    assert!(eval_eq(Some(super::sha1(arg)), Some(exp)));
    Ok(())
}

#[test_case(
    "abc",
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
)]
fn sha256(arg: &str, exp: &str) -> TestResult {
    let exp = EvalResult::from(Arc::from(exp));
    assert!(eval_eq(Some(super::sha256(arg)), Some(exp)));
    Ok(())
}

#[test_case(
    "abc",
    "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"
)]
fn sha384(arg: &str, exp: &str) -> TestResult {
    let exp = EvalResult::from(Arc::from(exp));
    assert!(eval_eq(Some(super::sha384(arg)), Some(exp)));
    Ok(())
}

#[test_case(
    "abc",
    "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
)]
fn sha512(arg: &str, exp: &str) -> TestResult {
    let exp = EvalResult::from(Arc::from(exp));
    assert!(eval_eq(Some(super::sha512(arg)), Some(exp)));
    Ok(())
}

#[test_case("chat", "fr", Some("chat@fr"))]
#[test_case("abc", "", None)]
fn str_lang(lex: &str, lang: &str, exp: Option<&str>) -> TestResult {
    let exp = exp.map(|exp| EvalResult::from(txt2pair(exp)));
    assert!(eval_eq(super::str_lang(lex, lang), exp));
    Ok(())
}

#[test_case("abc", "en", "ltr", Some("abc@en--ltr"))]
#[test_case("abc", "en", "LTR", None)]
#[test_case("قطة", "ar", "rtl", Some("قطة@ar--rtl"))]
#[test_case("abc", "en", "", None)]
#[test_case("abc", "", "ltr", None)]
fn str_lang_dir(lex: &str, lang: &str, dir: &str, exp: Option<&str>) -> TestResult {
    let exp = exp.map(|exp| EvalResult::from(txt2pair(exp)));
    assert!(eval_eq(super::str_lang_dir(lex, lang, dir), exp));
    Ok(())
}

#[test_case("123", "http://www.w3.org/2001/XMLSchema#integer", true)]
#[test_case("iii", "http://example/romanNumeral", true)]
#[test_case(
    "hello",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString",
    false
)]
#[test_case(
    "hello",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#dirLangString",
    false
)]
fn str_dt(lex: &str, dt: &str, exp: bool) -> TestResult {
    let dt = IriRef::<Arc<str>>::new_unchecked(dt.into());
    let exp = exp.then_some(GenericLiteral::Typed(lex.into(), dt.clone()).into());
    assert!(eval_eq(super::str_dt(lex, &dt), exp));
    Ok(())
}

#[test_case("abracadabra", "bra", None, Some(true))]
#[test_case("abracadabra", "^a.*a$", None, Some(true))]
#[test_case("abracadabra", "^bra", None, Some(false))]
#[test_case("$poem", "Kaum.*krähen", None, Some(false))]
#[test_case("$poem", "Kaum.*krähen", Some("s"), Some(true))]
#[test_case("$poem", "^Kaum.*gesehen,$", Some("m"), Some(true))]
#[test_case("$poem", "^Kaum.*gesehen,$", None, Some(false))]
#[test_case("$poem", "kiki", Some("i"), Some(true))]
#[test_case("helloworld", "hello world", Some("x"), Some(true))]
// the two tests below are provided in https://www.w3.org/TR/xpath-functions-31/#flags
// but they are actually not honnored by many SPARQL implementations
// so skipping them
// #[test_case("helloworld", "hello[ ]world", Some("x"), Some(false))]
// #[test_case("hello world", "hello\\ sworld", Some("x"), Some(true))]
#[test_case("hello world", "hello world", Some("x"), Some(false))]
#[test_case("Alice", "^ali", Some("i"), Some(true))]
#[test_case("Bob", "^ali", Some("i"), Some(false))]
#[test_case("invalid pattern", "[", None, None)]
#[test_case("invalid flag", ".", Some("iz"), None)]
fn regex(mut text: &str, pattern: &str, flags: Option<&str>, exp: Option<bool>) -> TestResult {
    if text == "$poem" {
        // # spelchecker: off
        text = r#"
<poem author="Wilhelm Busch">
Kaum hat dies der Hahn gesehen,
Fängt er auch schon an zu krähen:
Kikeriki! Kikikerikih!!
Tak, tak, tak! - da kommen sie.
</poem>
"#;
    }
    let flags = flags.map(Arc::<str>::from);
    let exp = exp.map(EvalResult::from);
    assert!(eval_eq(super::regex(text, pattern, flags.as_ref()), exp));
    Ok(())
}

#[test_case("<tag:s>", "<tag:p>", "<tag:o>", true)]
#[test_case("<tag:s>", "<tag:p>", "bnode()", true)]
#[test_case("<tag:s>", "<tag:p>", " \"o\" ", true)]
#[test_case("bnode()", "<tag:p>", "<tag:o>", true)]
#[test_case("bnode()", "<tag:p>", "bnode()", true)]
#[test_case("bnode()", "<tag:p>", " \"o\" ", true)]
#[test_case(" \"s\" ", "<tag:p>", "<tag:o>", false)]
#[test_case(" \"s\" ", "<tag:p>", "bnode()", false)]
#[test_case(" \"s\" ", "<tag:p>", " \"o\" ", false)]
#[test_case("<tag:s>", "bnode()", "<tag:o>", false)]
#[test_case("<tag:s>", " \"p\" ", "<tag:o>", false)]
fn triple(s: &str, p: &str, o: &str, ok: bool) -> TestResult {
    let s = eval_expr(s)?;
    let p = eval_expr(p)?;
    let o = eval_expr(o)?;
    let got = super::triple(&s, &p, &o);
    if ok {
        let t = got.unwrap().as_term();
        let t = t.triple().unwrap();
        assert!(Term::eq(&t[0], s.as_term()));
        assert!(Term::eq(&t[1], p.as_term()));
        assert!(Term::eq(&t[2], o.as_term()));
    } else {
        assert!(got.is_none());
    }
    Ok(())
}

#[test_case("<tag:s>", "")]
#[test_case("bnode()", "")]
#[test_case("<<( <tag:s> <tag:p> <tag:o> )>>", "")]
#[test_case("\"true\"@en", ""; "language string")] // the spec is not clear about this one
#[test_case("\"true\"^^<tag:dummy>", ""; "unrecognized datatype")]
#[test_case("true", "true"; "true boolean")]
#[test_case("false", "false"; "false boolean")]
#[test_case("\"bad\"^^xsd:boolean", ""; "bad boolean")]
#[test_case("\"1\"", "true"; "1 string")]
#[test_case("\"0 \"", "false"; "0 string")]
#[test_case("\"bad\"^^xsd:integer", ""; "bad integer")]
#[test_case("\" true\"", "true"; "true string")]
#[test_case("\" false \"", "false"; "false string")]
#[test_case("\"True\"", ""; "invalid string")]
#[test_case("1", "true"; "1 integer")]
#[test_case("0", "false"; "0 integer")]
#[test_case("1.0", "true"; "1 decimal")]
#[test_case("0.0", "false"; "0 decimal")]
#[test_case("1e0", "true"; "1 double")]
#[test_case("0.0e0", "false"; "0 double")]
#[test_case("-0.0e0", "false"; "negative 0 double")]
#[test_case("\"-INF\"^^xsd:double", "true"; "negative inf double")]
#[test_case("\"NaN\"^^xsd:double", "false"; "nan double")]
#[test_case("\"1e0\"^^xsd:float", "true"; "1 float")]
#[test_case("\"0.0e0\"^^xsd:float", "false"; "0 float")]
#[test_case("\"-0.0e0\"^^xsd:float", "false"; "negative 0 float")]
#[test_case("\"-INF\"^^xsd:float", "true"; "negative inf float")]
#[test_case("\"NaN\"^^xsd:float", "false"; "nan float")]
#[test_case("\"2025-05-20:01:02:03Z\"^^xsd:dateTime", ""; "dateTime")]
fn xsd_boolean(input: &str, exp: &str) -> TestResult {
    let input = eval_expr(input)?;
    let got = super::xsd_boolean(&input);
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?)
    };
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("<tag:s>", "")]
#[test_case("bnode()", "")]
#[test_case("<<( <tag:s> <tag:p> <tag:o> )>>", "")]
#[test_case("\"1.000\"@fr", ""; "language string")] // the spec is not clear about this one
#[test_case("\"1.0\"^^<tag:dummy>", ""; "unrecognized datatype")]
#[test_case("01.2e34", "01.2e34"; "double with leading 0")]
#[test_case("-0.0e0", "-0.0e0"; "negative 0 double")]
#[test_case("\"-INF\"^^xsd:double", "\"-INF\"^^xsd:double"; "negative inf double")]
#[test_case("\"NaN\"^^xsd:double", "\"NaN\"^^xsd:double"; "nan double")]
#[test_case("\"bad\"^^xsd:double", ""; "bad double")]
#[test_case("\"01.2e1\"^^xsd:float", "1.2e1"; "float with leading 0")]
#[test_case("\"-0.0e0\"^^xsd:float", "-0.0e0"; "negative 0 float")]
#[test_case("\"-INF\"^^xsd:float", "\"-INF\"^^xsd:double"; "negative inf float")]
#[test_case("\"NaN\"^^xsd:float", "\"NaN\"^^xsd:double"; "nan float")]
#[test_case("\" 01.2e34 \"", "1.2e34"; "string")]
#[test_case("\"bad\"", ""; "bad string")]
#[test_case("42", "4.2e1"; "integer")]
#[test_case("4.2", "4.2e0"; "decimal")]
#[test_case("true", "1e0"; "true boolean")]
#[test_case("false", "0e0"; "false boolean")]
#[test_case("\"2025-05-20:01:02:03Z\"^^xsd:dateTime", ""; "dateTime")]
fn xsd_double(input: &str, exp: &str) -> TestResult {
    let input = eval_expr(input)?;
    let got = super::xsd_double(&input);
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?)
    };
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("<tag:s>", "")]
#[test_case("bnode()", "")]
#[test_case("<<( <tag:s> <tag:p> <tag:o> )>>", "")]
#[test_case("\"1.000\"@fr", ""; "language string")] // the spec is not clear about this one
#[test_case("\"1.0\"^^<tag:dummy>", ""; "unrecognized datatype")]
#[test_case("01.2e34", "\"1.2e34\"^^xsd:float"; "double with leading 0")]
#[test_case("-0.0e0", "\"-0e0\"^^xsd:float"; "negative 0 double")]
#[test_case("\"-INF\"^^xsd:double", "\"-INF\"^^xsd:float"; "negative inf double")]
#[test_case("\"NaN\"^^xsd:double", "\"NaN\"^^xsd:float"; "nan double")]
#[test_case("\"01.2e1\"^^xsd:float", "\"01.2e1\"^^xsd:float"; "float with leading 0")]
#[test_case("\"-0.0e0\"^^xsd:float", "\"-0.0e0\"^^xsd:float"; "negative 0 float")]
#[test_case("\"-INF\"^^xsd:float", "\"-INF\"^^xsd:float"; "negative inf float")]
#[test_case("\"NaN\"^^xsd:float", "\"NaN\"^^xsd:float"; "nan float")]
#[test_case("\"bad\"^^xsd:float", ""; "bad float")]
#[test_case("\" 01.2e34 \"", "\"1.2e34\"^^xsd:float"; "string")]
#[test_case("\"bad\"", ""; "bad string")]
#[test_case("42", "\"4.2e1\"^^xsd:float"; "integer")]
#[test_case("4.2", "\"4.2e0\"^^xsd:float"; "decimal")]
#[test_case("true", "\"1e0\"^^xsd:float"; "true boolean")]
#[test_case("false", "\"0e0\"^^xsd:float"; "false boolean")]
#[test_case("\"2025-05-20:01:02:03Z\"^^xsd:dateTime", ""; "dateTime")]
fn xsd_float(input: &str, exp: &str) -> TestResult {
    let input = eval_expr(input)?;
    let got = super::xsd_float(&input);
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?)
    };
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("<tag:s>", "")]
#[test_case("bnode()", "")]
#[test_case("<<( <tag:s> <tag:p> <tag:o> )>>", "")]
#[test_case("\"1.000\"@fr", ""; "language string")] // the spec is not clear about this one
#[test_case("\"1.0\"^^<tag:dummy>", ""; "unrecognized datatype")]
#[test_case("01.2e34", "11999999999999999346902771844513792.0"; "double with leading 0")]
#[test_case("-0.0e0", "0.0"; "negative 0 double")]
#[test_case("\"-INF\"^^xsd:double", ""; "negative inf double")]
#[test_case("\"NaN\"^^xsd:double", ""; "nan double")]
#[test_case("\"01.2e1\"^^xsd:float", "12.0"; "float with leading 0")]
#[test_case("\"-0.0e0\"^^xsd:float", "0.0"; "negative 0 float")]
#[test_case("\"-INF\"^^xsd:float", ""; "negative inf float")]
#[test_case("\"NaN\"^^xsd:float", ""; "nan float")]
#[test_case("\" 01.2e34 \"", "12000000000000000000000000000000000.0"; "string")]
#[test_case("\"bad\"", ""; "bad string")]
#[test_case("42", "42.0"; "integer")]
#[test_case("4.2", "4.2"; "decimal")]
#[test_case("\"bad\"^^xsd:decimal", ""; "bad decimal")]
#[test_case("true", "1.0"; "true boolean")]
#[test_case("false", "0.0"; "false boolean")]
#[test_case("\"2025-05-20:01:02:03Z\"^^xsd:dateTime", ""; "dateTime")]
fn xsd_decimal(input: &str, exp: &str) -> TestResult {
    let input = eval_expr(input)?;
    let got = super::xsd_decimal(&input);
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?)
    };
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("<tag:s>", "")]
#[test_case("bnode()", "")]
#[test_case("<<( <tag:s> <tag:p> <tag:o> )>>", "")]
#[test_case("\"1.000\"@fr", ""; "language string")] // the spec is not clear about this one
#[test_case("\"1.0\"^^<tag:dummy>", ""; "unrecognized datatype")]
#[test_case("01.2e34", "11999999999999999346902771844513792"; "double with leading 0")]
#[test_case("-0.0e0", "0"; "negative 0 double")]
#[test_case("\"-INF\"^^xsd:double", ""; "negative inf double")]
#[test_case("\"NaN\"^^xsd:double", ""; "nan double")]
#[test_case("\"01.29e1\"^^xsd:float", "12"; "float with leading 0")]
#[test_case("\"-1.29e1\"^^xsd:float", "-12"; "negative float")]
#[test_case("\"-0.0e0\"^^xsd:float", "0"; "negative 0 float")]
#[test_case("\"-INF\"^^xsd:float", ""; "negative inf float")]
#[test_case("\"NaN\"^^xsd:float", ""; "nan float")]
#[test_case("\" 42 \"", "42"; "string")]
#[test_case("\"bad\"", ""; "bad string")]
#[test_case("42", "42"; "integer")]
#[test_case("123456789123456789123456789", "123456789123456789123456789"; "big integer")]
#[test_case("\"bad\"^^xsd:integer", ""; "bad integer")]
#[test_case("4.9", "4"; "decimal")]
#[test_case("-4.9", "-4"; "negative decimal")]
#[test_case("true", "1"; "true boolean")]
#[test_case("false", "0"; "false boolean")]
#[test_case("\"2025-05-20:01:02:03Z\"^^xsd:dateTime", ""; "dateTime")]
fn xsd_integer(input: &str, exp: &str) -> TestResult {
    let input = eval_expr(input)?;
    let got = super::xsd_integer(&input);
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?)
    };
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("<tag:s>", "")]
#[test_case("bnode()", "")]
#[test_case("<<( <tag:s> <tag:p> <tag:o> )>>", "")]
#[test_case("\"2025-01-02T03:04:05Z\"@fr", ""; "language string")] // the spec is not clear about this one
#[test_case("\"2025-01-02T03:04:05Z\"^^<tag:dummy>", ""; "unrecognized datatype")]
#[test_case("\" 2025-01-02T03:04:05+00:00 \"", "\"2025-01-02T03:04:05Z\"^^xsd:dateTime"; "string")]
#[test_case("\"bad\"", ""; "bad string")]
#[test_case("42", ""; "integer")]
#[test_case("4.2", ""; "decimal")]
#[test_case("true", ""; "true boolean")]
#[test_case("false", ""; "false boolean")]
#[test_case("\"2025-05-20:01:02:03Z\"^^xsd:dateTime", "\"2025-05-20:01:02:03Z\"^^xsd:dateTime"; "dateTime")]
#[test_case("\"bad\"^^xsd:dateTime", "\"bad\"^^xsd:dateTime"; "bad dateTime")]
fn xsd_date_time(input: &str, exp: &str) -> TestResult {
    let input = eval_expr(input)?;
    let got = super::xsd_date_time(&input);
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?)
    };
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("<tag:s>", "\"tag:s\"")]
#[test_case("bnode()", "")]
#[test_case("<<( <tag:s> <tag:p> <tag:o> )>>", "")]
#[test_case("\"1.000\"@fr", "\"1.000\""; "language string")] // the spec is not clear about this one
#[test_case("\"1.0\"^^<tag:dummy>", "\"1.0\""; "unrecognized datatype")]
#[test_case("01.2e34", "\"1.2E34\""; "big double with leading 0")]
#[test_case("1.2e-34", "\"1.2E-34\""; "small double")]
#[test_case("01.2e3", "\"1200\""; "medium-big double with leading 0")]
#[test_case("1.2e-3", "\"0.0012\""; "medium-small double")]
#[test_case("-0.0e0", "\"-0\""; "negative 0 double")]
#[test_case("\"-INF\"^^xsd:double", "\"-INF\""; "negative inf double")]
#[test_case("\"NaN\"^^xsd:double", "\"NaN\""; "nan double")]
#[test_case("\"01.2e34\"^^xsd:float", "\"1.2E34\""; "big float with leading 0")]
#[test_case("\"1.2e-34\"^^xsd:float", "\"1.2E-34\""; "small float")]
#[test_case("\"01.2e3\"^^xsd:float", "\"1200\""; "medium-big float with leading 0")]
#[test_case("\"1.2e-3\"^^xsd:float", "\"0.0012\""; "medium-small float")]
#[test_case("\"-0.0e0\"^^xsd:float", "\"-0\""; "negative 0 float")]
#[test_case("\"-INF\"^^xsd:float", "\"-INF\""; "negative inf float")]
#[test_case("\"NaN\"^^xsd:float", "\"NaN\""; "nan float")]
#[test_case("\" 42⛄ \"", "\" 42⛄ \""; "string")]
#[test_case("42", "\"42\""; "integer")]
#[test_case("123456789123456789123456789", "\"123456789123456789123456789\""; "big integer")]
#[test_case("\"bad\"^^xsd:integer", ""; "bad integer")]
#[test_case("4.2", "\"4.2\""; "decimal")]
#[test_case("4.000", "\"4\""; "decimal integer")]
#[test_case("true", "\"true\""; "true boolean")]
#[test_case("false", "\"false\""; "false boolean")]
#[test_case("\"1\"^^xsd:boolean", "\"true\""; "1 boolean")]
#[test_case("\"2025-05-20T01:02:03.45+00:00\"^^xsd:dateTime", "\"2025-05-20T01:02:03.450Z\""; "dateTime")]
#[test_case("\"10000-05-20T01:02:03.45\"^^xsd:dateTime", "\"10000-05-20T01:02:03.450\""; "dateTime big")]
fn xsd_string(input: &str, exp: &str) -> TestResult {
    let input = eval_expr(input)?;
    let got = super::xsd_string(&input);
    let exp = if exp.is_empty() {
        None
    } else {
        Some(eval_expr(exp)?)
    };
    assert!(eval_eq(got, exp));
    Ok(())
}

/// Evaluate the given SPARQL expression
fn eval_expr(expr: &str) -> TestResult<EvalResult> {
    eprintln!("eval_expr: {expr}");
    let dataset = LightDataset::default();
    let dataset = SparqlWrapper(&dataset);
    let query = SparqlQuery::parse(&format!(
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ({expr} as ?x) {{}}"
    ))?;
    let bindings = dataset.query(&query)?.into_bindings();
    assert_eq!(bindings.variables().len(), 1);
    let mut first_binding = bindings.into_iter().next().unwrap()?;
    assert_eq!(first_binding.len(), 1);
    Ok(first_binding.pop().unwrap().unwrap().into())
}

fn eval_eq(e1: Option<EvalResult>, e2: Option<EvalResult>) -> bool {
    match (dbg!(e1), dbg!(e2)) {
        (Some(e1), Some(e2)) => Term::eq(&dbg!(e1.into_term()), dbg!(e2.into_term())),
        (None, None) => true,
        _ => false,
    }
}

type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;
