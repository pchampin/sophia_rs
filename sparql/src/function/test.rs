#![allow(clippy::unnecessary_wraps)]
use std::{collections::HashSet, sync::Arc};

use crate::{
    expression::EvalResult,
    value::{SparqlNumber, SparqlValue},
    SparqlQuery, SparqlWrapper,
};

use sophia_api::{
    ns::rdf,
    sparql::{Query, SparqlDataset},
    term::{IriRef, LanguageTag, Term},
};
use sophia_inmem::dataset::LightDataset;
use sophia_term::GenericLiteral;
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
    let query = SparqlQuery::parse(
        &format!("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT ({expr} as ?x) {{}}")
    )?;
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
