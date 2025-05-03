#![allow(clippy::unnecessary_wraps)]
use std::{collections::HashSet, str::FromStr, sync::Arc};

use crate::{
    ResultTerm, SparqlQuery, SparqlWrapper,
    expression::EvalResult,
    value::{SparqlNumber, SparqlValue, XsdDateTime},
};

use sophia_api::{
    ns::{rdf, xsd},
    sparql::{Query, SparqlDataset},
    term::{FromTerm, IriRef, LanguageTag, Term},
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
fn str_literal(lex: &str, lang: &str, dt: &str) -> TestResult {
    let lit = if lang.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.into()))
    } else {
        GenericLiteral::LanguageString(lex.into(), LanguageTag::new_unchecked(lang.into()))
    };
    let got = Some(super::str_literal(lit));
    let exp = Some(EvalResult::from(Arc::<str>::from(lex)));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", "")]
fn lang(lex: &str, lang: &str, dt: &str) -> TestResult {
    let lit = if lang.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.into()))
    } else {
        GenericLiteral::LanguageString(lex.into(), LanguageTag::new_unchecked(lang.into()))
    };
    let got = Some(super::lang(lit));
    let exp = Some(EvalResult::from(Arc::<str>::from(lang)));
    assert!(eval_eq(got, exp));
    Ok(())
}

#[allow(clippy::needless_pass_by_value)]
#[test_case("chat", "", "tag:dt")]
#[test_case("chat", "en", rdf::langString)]
fn datatype<T: ToString>(lex: &str, lang: &str, dt: T) -> TestResult {
    let lit = if lang.is_empty() {
        GenericLiteral::Typed(lex.into(), IriRef::new_unchecked(dt.to_string().into()))
    } else {
        GenericLiteral::LanguageString(lex.into(), LanguageTag::new_unchecked(lang.into()))
    };
    let got = Some(super::datatype(lit));
    let exp = Some(EvalResult::from(IriRef::new_unchecked(Arc::<str>::from(
        dt.to_string(),
    ))));
    assert!(eval_eq(dbg!(got), dbg!(exp)));
    Ok(())
}

#[test_case("tag:x", true)]
#[test_case("../a", true)]
#[test_case("a b", false)]
fn iri(arg: &str, exp: bool) -> TestResult {
    let arg = Arc::<str>::from(arg);
    let got = super::iri(&arg);
    let exp = true
        .then_some(arg)
        .and_then(|arg| IriRef::new(arg.clone()).ok())
        .map(EvalResult::from);
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
        let EvalResult::Value(SparqlValue::Number(SparqlNumber::Double(val))) = super::rand()
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
#[test_case(vec!["foo@en", "bar"], "foobar")]
#[test_case(vec!["foo", "bar@en"], "foobar")]
#[test_case(vec!["foo@en", "bar@es"], "foobar")]
#[test_case(vec!["abc"], "abc")]
#[test_case(vec!["abc@en"], "abc@en")]
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

fn txt2pair(txt: &str) -> (Arc<str>, Option<LanguageTag<Arc<str>>>) {
    let (lex, tag) = txt.split_once('@').unwrap_or((txt, ""));
    (
        Arc::from(lex),
        if tag.is_empty() {
            None
        } else {
            Some(LanguageTag::new_unchecked(Arc::from(tag)))
        },
    )
}

fn pair2ref(
    pair: &(Arc<str>, Option<LanguageTag<Arc<str>>>),
) -> (&Arc<str>, Option<&LanguageTag<Arc<str>>>) {
    (&pair.0, pair.1.as_ref())
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
#[test_case("foobar", 4.0, Some(1.0), Some("b"))]
#[test_case("foobar@en", 4.0, Some(1.0), Some("b@en"))]
#[test_case("foobar", -2.0, Some(6.0), Some("foo"))]
#[test_case("foobar", -2.0, None, Some("foobar"))]
#[test_case("foobar", 4.0, Some(0.0), Some(""))]
#[test_case("foobar", 4.0, Some(-1.0), Some(""))]
#[test_case("foobar", 0.9, Some(1.1), Some("f"))]
#[test_case("foobar", 1.1, Some(0.9), Some("f"))]
fn sub_str(source: &str, start: f64, length: Option<f64>, exp: Option<&str>) -> TestResult {
    let pair = txt2pair(source);
    let source = pair2ref(&pair);
    let exp = exp.map(|txt| EvalResult::from(txt2pair(txt)));
    assert!(eval_eq(super::sub_str(source, start, length), exp));
    Ok(())
}

#[test_case("foobar", 6)]
#[test_case("foobar@en", 6)]
#[test_case("é", 1)]
#[test_case("é@fr", 1)]
#[test_case("⛄", 1; "snowman")]
#[test_case("⛄@en", 1; "snowman en")]
fn str_len(string: &str, exp: isize) -> TestResult {
    let pair = txt2pair(string);
    let string = &pair.0;
    let exp = EvalResult::from(SparqlNumber::from(exp));
    assert!(eval_eq(Some(super::str_len(string)), Some(exp)));
    Ok(())
}

#[test_case("abcd", "X", "Z", None, Some("abcd"))]
#[test_case("abcd@en", "X", "Z", None, Some("abcd@en"))]
#[test_case("abcd", "b", "Z", None, Some("aZcd"))]
#[test_case("abcd@en", "b", "Z", None, Some("aZcd@en"))]
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
#[test_case("FOO", "FOO"; "noop")]
#[test_case("FOO@en", "FOO@en"; "noop en")]
#[test_case("fooBAR 1!⛄xY", "FOOBAR 1!⛄XY")]
#[test_case("fooBAR 1!⛄xY@en", "FOOBAR 1!⛄XY@en")]
#[test_case("àéîôù", "ÀÉÎÔÙ"; "accents")]
#[test_case("àéîôù@fr", "ÀÉÎÔÙ@fr"; "accents fr")]
#[test_case("ﬀ ŉ", "FF ʼN"; "multichar")]
#[test_case("ﬀ ŉ@en", "FF ʼN@en"; "multichar en")]
fn u_case(string: &str, exp: &str) -> TestResult {
    let pair = txt2pair(string);
    let source = pair2ref(&pair);
    let exp = EvalResult::from(txt2pair(exp));
    assert!(eval_eq(Some(super::u_case(source)), Some(exp)));
    Ok(())
}

#[test_case("FOO", "foo")]
#[test_case("FOO@en", "foo@en")]
#[test_case("foo", "foo"; "noop")]
#[test_case("foo@en", "foo@en"; "noop en")]
#[test_case("fooBAR 1!⛄xY", "foobar 1!⛄xy")]
#[test_case("fooBAR 1!⛄xY@en", "foobar 1!⛄xy@en")]
#[test_case("ÀÉÎÔÙ", "àéîôù"; "accents")]
#[test_case("ÀÉÎÔÙ@fr", "àéîôù@fr"; "accents fr")]
fn l_case(string: &str, exp: &str) -> TestResult {
    let pair = txt2pair(string);
    let source = pair2ref(&pair);
    let exp = EvalResult::from(txt2pair(exp));
    assert!(eval_eq(Some(super::l_case(source)), Some(exp)));
    Ok(())
}

#[test_case("Los Angeles", "Los%20Angeles")]
#[test_case("Los Angeles@en", "Los%20Angeles")]
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
#[test_case("@en", "", Some(true))]
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
#[test_case("", "foo", Some(false))]
#[test_case("@en", "foo@en", Some(false))]
#[test_case("@en", "foo", Some(false))]
#[test_case("foobar", "BAR", Some(false))]
#[test_case("foobar", "baz", Some(false))]
#[test_case("foobar@en", "BAR@en", Some(false))]
#[test_case("foobar@en", "baz@en", Some(false))]
#[test_case("foobar@en", "BAR", Some(false))]
#[test_case("foobar@en", "baz", Some(false))]
#[test_case("", "@fr", None)]
#[test_case("foobar", "bar@fr", None)]
#[test_case("foobar", "baz@fr", None)]
#[test_case("@en", "@fr", None)]
#[test_case("foobar@en", "bar@fr", None)]
#[test_case("foobar@en", "baz@fr", None)]
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
#[test_case("@en", "", Some(true))]
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
#[test_case("", "foo", Some(false))]
#[test_case("@en", "foo@en", Some(false))]
#[test_case("@en", "foo", Some(false))]
#[test_case("foobar", "FOO", Some(false))]
#[test_case("foobar", "baz", Some(false))]
#[test_case("foobar@en", "FOO@en", Some(false))]
#[test_case("foobar@en", "baz@en", Some(false))]
#[test_case("foobar@en", "FOO", Some(false))]
#[test_case("foobar@en", "baz", Some(false))]
#[test_case("", "@fr", None)]
#[test_case("foobar", "bar@fr", None)]
#[test_case("foobar", "baz@fr", None)]
#[test_case("@en", "@fr", None)]
#[test_case("foobar@en", "foo@fr", None)]
#[test_case("foobar@en", "baz@fr", None)]
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
#[test_case("@en", "", Some(true))]
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
#[test_case("", "foo", Some(false))]
#[test_case("@en", "foo@en", Some(false))]
#[test_case("@en", "foo", Some(false))]
#[test_case("foobar", "BAR", Some(false))]
#[test_case("foobar", "baz", Some(false))]
#[test_case("foobar@en", "BAR@en", Some(false))]
#[test_case("foobar@en", "baz@en", Some(false))]
#[test_case("foobar@en", "BAR", Some(false))]
#[test_case("foobar@en", "baz", Some(false))]
#[test_case("", "@fr", None)]
#[test_case("foobar", "bar@fr", None)]
#[test_case("foobar", "baz@fr", None)]
#[test_case("@en", "@fr", None)]
#[test_case("foobar@en", "bar@fr", None)]
#[test_case("foobar@en", "baz@fr", None)]
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
#[test_case("abcbde", "b@fr", None)]
#[test_case("abcbde@en", "b@fr", None)]
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
#[test_case("abcbde", "b@fr", None)]
#[test_case("abcbde@en", "b@fr", None)]
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

/// Evaluate the given SPARQL expression,
/// returning one or two versions:
/// one `EvalResult::Term` and one `EvalResult::Value` if appropriate.
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
        (Some(e1), Some(e2)) => Term::eq(&e1.into_term(), e2.into_term()),
        (None, None) => true,
        _ => false,
    }
}

type TestResult<T = ()> = Result<T, Box<dyn std::error::Error>>;
