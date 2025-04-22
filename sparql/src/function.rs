use std::sync::{Arc, LazyLock};

use chrono::{Datelike, Timelike};
use rand::random;
use regex::{Regex, RegexBuilder};
use sophia_api::term::{BnodeId, IriRef, LanguageTag, Term};
use sophia_term::GenericLiteral;
use spargebra::algebra::Function::{self, *};

use crate::{
    ResultTerm,
    exec::ExecConfig,
    expression::EvalResult,
    ns::RDF_LANG_STRING,
    value::{SparqlNumber, SparqlValue, XsdDateTime},
};

#[allow(clippy::module_name_repetitions, clippy::too_many_lines)]
pub fn call_function<D: ?Sized>(
    function: &Function,
    mut arguments: Vec<EvalResult>,
    config: &ExecConfig<D>,
) -> Option<EvalResult> {
    match function {
        Str => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            #[expect(clippy::manual_map)]
            if let Some(iri) = arg.as_iri() {
                Some(str_iri(iri))
            } else if let Some(lit) = arg.as_literal() {
                Some(str_literal(lit))
            } else {
                None
            }
        }
        Lang => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(lang(arg.as_literal()?))
        }
        Datatype => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(datatype(arg.as_literal()?))
        }
        Iri => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            if let Some(iri) = arg.as_iri() {
                Some(iri.clone().into())
            } else if let Some(st) = arg.as_xsd_string() {
                Some(IriRef::new(st.clone()).ok()?.into())
            } else {
                None
            }
        }
        BNode => {
            let o: Option<Option<i32>> = Some(None);
            match arguments.pop() {
                None => Some(bnode0()),
                Some(arg) => Some(bnode1(arg.as_xsd_string()?)),
            }
        }
        Rand => Some(rand()),
        Abs => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number()?.abs().into())
        }
        Ceil => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number()?.ceil().into())
        }
        Floor => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number()?.floor().into())
        }
        Round => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number()?.round().into())
        }
        Concat => {
            let args = arguments
                .iter()
                .map(EvalResult::as_string_lit)
                .collect::<Option<Vec<_>>>()?;
            Some(concat(&args))
        }
        LangMatches => {
            let [tag, range] = &arguments[..] else {
                unreachable!()
            };
            lang_matches(tag.as_xsd_string()?, range.as_xsd_string()?)
        }
        SubStr => match &arguments[..] {
            [source, starting_loc] => sub_str(
                source.as_string_lit()?,
                starting_loc.as_number()?.coerce_to_double(),
                None,
            ),
            [source, starting_loc, length] => sub_str(
                source.as_string_lit()?,
                starting_loc.as_number()?.coerce_to_double(),
                Some(length.as_number()?.coerce_to_double()),
            ),
            _ => unreachable!(),
        },
        StrLen => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(str_len(string.as_string_lit()?.0))
        }
        Replace => todo("Replace"),
        UCase => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(u_case(string.as_string_lit()?))
        }
        LCase => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(l_case(string.as_string_lit()?))
        }
        EncodeForUri => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(encode_for_uri(string.as_string_lit()?.0))
        }
        Contains => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            contains(heystack.as_string_lit()?, needle.as_string_lit()?)
        }
        StrStarts => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strstarts(heystack.as_string_lit()?, needle.as_string_lit()?)
        }
        StrEnds => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strends(heystack.as_string_lit()?, needle.as_string_lit()?)
        }
        StrBefore => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strbefore(heystack.as_string_lit()?, needle.as_string_lit()?)
        }
        StrAfter => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strafter(heystack.as_string_lit()?, needle.as_string_lit()?)
        }
        Year => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(year(argument.as_xsd_date_time()?))
        }
        Month => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(month(argument.as_xsd_date_time()?))
        }
        Day => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(day(argument.as_xsd_date_time()?))
        }
        Hours => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(hours(argument.as_xsd_date_time()?))
        }
        Minutes => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(minutes(argument.as_xsd_date_time()?))
        }
        Seconds => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(seconds(argument.as_xsd_date_time()?))
        }
        Timezone => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            timezone(argument.as_xsd_date_time()?)
        }
        Tz => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            argument.as_xsd_date_time()?; // ensure it is an xsd:dateTime
            Some(tz(argument.as_literal()?.get_lexical_form()))
        }
        Now => {
            debug_assert!(arguments.is_empty());
            Some(EvalResult::from(SparqlValue::DateTime(Some(
                XsdDateTime::Timezoned(config.now),
            ))))
        }
        Uuid => todo("Uuid"),
        StrUuid => todo("StrUuid"),
        Md5 => todo("Md5"),
        Sha1 => todo("Sha1"),
        Sha256 => todo("Sha256"),
        Sha384 => todo("Sha384"),
        Sha512 => todo("Sha512"),
        StrLang => todo("StrLang"),
        StrDt => todo("StrDt"),
        IsIri => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(is_iri(arg))
        }
        IsBlank => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(is_blank(arg))
        }
        IsLiteral => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(is_literal(arg))
        }
        IsNumeric => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(is_numeric(arg))
        }
        Regex => {
            let (text, pattern, flags) = match &arguments[..] {
                [text, pattern] => (text, pattern, None),
                [text, pattern, flags] => (text, pattern, Some(flags.as_xsd_string()?)),
                _ => unreachable!(),
            };
            let text = text.as_string_lit()?;
            let pattern = pattern.as_xsd_string()?;
            sparql_regex(text.0, pattern, flags)
        }
        Triple => {
            let [s, p, o] = &arguments[..] else {
                unreachable!()
            };
            triple(s, p, o)
        }
        Subject => todo("Subject"),
        Predicate => todo("Predicate"),
        Object => todo("Object"),
        IsTriple => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(is_triple(arg))
        }
        Custom(iri) => todo(iri.to_string()),
    }
}

pub fn str_iri(iri: &IriRef<Arc<str>>) -> EvalResult {
    iri.clone().unwrap().into()
}

pub fn str_literal(lit: GenericLiteral<Arc<str>>) -> EvalResult {
    use GenericLiteral::*;
    match lit {
        Typed(lex, _) | LanguageString(lex, _) => lex.into(),
    }
}

pub fn lang(lit: GenericLiteral<Arc<str>>) -> EvalResult {
    use GenericLiteral::*;
    match lit {
        Typed(..) => Arc::<str>::from("").into(),
        LanguageString(_, tag) => tag.unwrap().into(),
    }
}

pub fn datatype(lit: GenericLiteral<Arc<str>>) -> EvalResult {
    use GenericLiteral::{LanguageString, Typed};
    match lit {
        Typed(_, dt) => dt.clone().into(),
        LanguageString(..) => RDF_LANG_STRING.clone().into(),
    }
}

pub fn is_iri(er: &EvalResult) -> EvalResult {
    match er {
        EvalResult::Term(t) => t.is_iri(),
        EvalResult::Value(_) => false,
    }
    .into()
}

pub fn is_blank(er: &EvalResult) -> EvalResult {
    match er {
        EvalResult::Term(t) => t.is_blank_node(),
        EvalResult::Value(_) => false,
    }
    .into()
}

pub fn is_literal(er: &EvalResult) -> EvalResult {
    match er {
        EvalResult::Term(t) => t.is_literal(),
        EvalResult::Value(_) => true,
    }
    .into()
}

pub fn is_numeric(er: &EvalResult) -> EvalResult {
    matches!(er.as_value(), Some(SparqlValue::Number(_))).into()
}

pub fn sparql_regex(text: &str, pattern: &str, flags: Option<&Arc<str>>) -> Option<EvalResult> {
    make_regex(pattern, flags).map(|re| re.is_match(text).into())
}

fn make_regex(pattern: &str, flags: Option<&Arc<str>>) -> Option<Regex> {
    if let Some(flags) = flags {
        let mut reb = RegexBuilder::new(pattern);
        for c in flags.bytes() {
            match c {
                b's' => {
                    reb.dot_matches_new_line(true);
                }
                b'm' => {
                    reb.multi_line(true);
                }
                b'i' => {
                    reb.case_insensitive(true);
                }
                b'x' => {
                    reb.ignore_whitespace(true);
                }
                b'q' => {
                    log::warn!("regex flag 'q' not implemented");
                    return None;
                }
                _ => {
                    log::error!("unrecognized regex flag in '{flags}' (should be one of smixq)");
                    return None;
                }
            }
        }
        reb.build()
    } else {
        Regex::new(pattern)
    }
    .inspect_err(|err| log::error!("invalid regex pattern: {err}"))
    .ok()
}

pub fn iri(st: &Arc<str>) -> Option<EvalResult> {
    IriRef::new(st.clone()).ok().map(EvalResult::from)
}

pub fn bnode0() -> EvalResult {
    let bnid = uuid::Uuid::now_v7().to_string();
    let bnid = BnodeId::<Arc<str>>::new_unchecked(bnid.into());
    bnid.into()
}

pub fn bnode1(arg: &Arc<str>) -> EvalResult {
    // mimic Jena for the moment: ignore the argument
    // because we don't know whether we are in the same result or not.
    // TODO improve compliance and generate same bnode for a given 'er' AND result number?
    bnode0()
}

pub fn rand() -> EvalResult {
    SparqlNumber::from(random::<f64>()).into()
}

pub fn concat(args: &[StringLiteral]) -> EvalResult {
    let lex = args
        .iter()
        .map(|x| x.0.as_ref())
        .collect::<Vec<_>>()
        .join("");
    let tag1 = args.first().and_then(|x| x.1);
    let tag = if args.len() < 2 || args[1..].iter().all(|x| x.1 == tag1) {
        tag1
    } else {
        None
    };
    EvalResult::from((Arc::from(lex), tag.cloned()))
}

pub fn lang_matches(tag: &Arc<str>, range: &Arc<str>) -> Option<EvalResult> {
    let tag = LanguageTag::new(tag.clone()).ok()?;
    if range.as_ref() == "*" {
        return Some(true.into());
    }
    let range = LanguageTag::new(range.clone()).ok()?;
    Some(
        (range.len() <= tag.len()
            && tag[..range.len()].eq_ignore_ascii_case(range.as_str())
            && (tag.len() == range.len() || tag[range.len()..].starts_with('-')))
        .into(),
    )
}

pub fn sub_str(
    source: StringLiteral,
    starting_loc: f64,
    length: Option<f64>,
) -> Option<EvalResult> {
    if starting_loc.is_nan() {
        return None;
    }
    let (lex, tag) = source;
    let (s, e) = match length {
        Some(l) if l.is_nan() => return None,
        None | Some(f64::INFINITY) => (
            ((starting_loc.round() - 1.0) as usize).min(lex.len()),
            lex.len(),
        ),
        Some(l) => {
            let s_signed = starting_loc.round() as isize - 1;
            let s = (s_signed.max(0) as usize).min(lex.len());
            let e = ((s_signed + l.round() as isize).max(0) as usize)
                .max(s)
                .min(lex.len());
            (s, e)
        }
    };
    Some(EvalResult::from((Arc::from(&lex[s..e]), tag.cloned())))
}

pub fn str_len(string: &Arc<str>) -> EvalResult {
    let l = string.chars().count();
    if l <= isize::MAX as usize {
        SparqlNumber::from(l as isize).into()
    } else {
        unreachable!()
        // slices in Rust can not be longer than isize::MAX
        // See https://doc.rust-lang.org/std/primitive.pointer.html#method.offset
        // "Allocated objects can never be larger than isize::MAX bytes"
    }
}

pub fn u_case(source: StringLiteral) -> EvalResult {
    let lex: String = source.0.chars().flat_map(char::to_uppercase).collect();
    EvalResult::from((Arc::from(lex), source.1.cloned()))
}

pub fn l_case(source: StringLiteral) -> EvalResult {
    let lex: String = source.0.chars().flat_map(char::to_lowercase).collect();
    EvalResult::from((Arc::from(lex), source.1.cloned()))
}

pub fn encode_for_uri(source: &Arc<str>) -> EvalResult {
    use encode_for_uri_utils::*;
    let ascii = source
        .as_bytes()
        .iter()
        .copied()
        .flat_map(EncodeIter::new)
        .collect::<Vec<_>>();
    let ret: String = unsafe {
        // the following is safe, because 'ascii' only contains ASCII codes
        String::from_utf8_unchecked(ascii)
    };
    EvalResult::from((Arc::from(ret), None))
}

pub fn contains(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle)?;
    Some(heystack.0.contains(needle.0.as_ref()).into())
}

pub fn strstarts(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle)?;
    Some(heystack.0.starts_with(needle.0.as_ref()).into())
}

pub fn strends(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle)?;
    Some(heystack.0.ends_with(needle.0.as_ref()).into())
}

pub fn strbefore(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle)?;
    let found = heystack.0.find(needle.0.as_ref());
    Some(EvalResult::from((
        Arc::from(&heystack.0[..found.unwrap_or(0)]),
        found.and_then(|_| heystack.1.cloned()),
    )))
}

pub fn strafter(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle)?;
    let txt = heystack.0.as_ref();
    let delim = needle.0.as_ref();
    let found = txt.find(delim);
    let substr = match found {
        Some(pos) => &txt[pos + delim.len()..],
        None => "",
    };
    Some(EvalResult::from((
        Arc::from(substr),
        found.and_then(|_| heystack.1.cloned()),
    )))
}

pub fn year(dt: &XsdDateTime) -> EvalResult {
    SparqlNumber::from(dt.year() as isize).into()
}

pub fn month(dt: &XsdDateTime) -> EvalResult {
    SparqlNumber::from(dt.month() as isize).into()
}

pub fn day(dt: &XsdDateTime) -> EvalResult {
    SparqlNumber::from(dt.day() as isize).into()
}

pub fn hours(dt: &XsdDateTime) -> EvalResult {
    SparqlNumber::from(dt.hour() as isize).into()
}

pub fn minutes(dt: &XsdDateTime) -> EvalResult {
    SparqlNumber::from(dt.minute() as isize).into()
}

pub fn seconds(dt: &XsdDateTime) -> EvalResult {
    let nano: u64 = dt.second() as u64 * 1_000_000_000 + dt.nanosecond() as u64;
    let seconds = bigdecimal::BigDecimal::from((nano, 9));
    SparqlNumber::from(seconds).into()
}

pub fn timezone(dt: &XsdDateTime) -> Option<EvalResult> {
    static DATATYPE: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| {
        IriRef::new_unchecked("http://www.w3.org/2001/XMLSchema#dayTimeDuration".into())
    });
    match dt {
        XsdDateTime::Naive(_) => None,
        XsdDateTime::Timezoned(dt) => {
            let offset = dt.offset().local_minus_utc();
            let s = if offset.signum() < 0 { "-" } else { "" };
            let offset = offset.abs();
            let h = offset / 3600;
            let m = offset / 60 % 60;
            let lex = if h > 0 && m > 0 {
                format!("{s}PT{h}H{m}M")
            } else if h > 0 {
                format!("{s}PT{h}H")
            } else if m > 0 {
                format!("{s}PT{m}M")
            } else {
                "PT0S".into()
            };
            Some(
                ResultTerm::from(sophia_term::ArcTerm::Literal(GenericLiteral::Typed(
                    lex.into(),
                    DATATYPE.clone(),
                )))
                .into(),
            )
        }
    }
}

pub fn tz(valid_xsd_date_time: &str) -> EvalResult {
    static TZ: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^.?+:..:..(?:\.[0-9]*)?(.*)").unwrap());
    let substr = match TZ.captures(valid_xsd_date_time) {
        None => unreachable!("argument should be a valid xsd:dateTime"),
        Some(cap) => cap.get(1).unwrap().as_str(),
    };
    EvalResult::from(Arc::from(substr))
}

pub fn triple(s: &EvalResult, p: &EvalResult, o: &EvalResult) -> Option<EvalResult> {
    let EvalResult::Term(s) = s else { return None };
    let EvalResult::Term(p) = p else { return None };
    if !s.is_iri() && !s.is_blank_node() {
        return None;
    }
    if !p.is_iri() {
        return None;
    };
    let o = ResultTerm::from(o.as_term());
    Some(ResultTerm::from([s.clone(), p.clone(), o.clone()]).into())
}

pub fn is_triple(er: &EvalResult) -> EvalResult {
    match er {
        EvalResult::Term(t) => t.is_triple(),
        EvalResult::Value(_) => false,
    }
    .into()
}

fn todo<T: std::fmt::Display>(function_name: T) -> Option<EvalResult> {
    eprintln!("Function not implemented: {function_name}");
    None
}

type StringLiteral<'a> = (&'a Arc<str>, Option<&'a LanguageTag<Arc<str>>>);

fn check_compatible(arg1: StringLiteral, arg2: StringLiteral) -> Option<()> {
    match (arg1.1, arg2.1) {
        (_, None) => true,
        (Some(tag1), Some(tag2)) if tag1 == tag2 => true,
        _ => false,
    }
    .then_some(())
}

mod encode_for_uri_utils {
    pub struct EncodeIter {
        value: u8,
        state: u8,
    }

    impl EncodeIter {
        pub fn new(value: u8) -> Self {
            match value {
                b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                    Self { value, state: 0 }
                }
                _ => Self { value, state: 2 },
            }
        }
    }

    impl Iterator for EncodeIter {
        type Item = u8;

        fn next(&mut self) -> Option<Self::Item> {
            self.state += 1;
            match self.state {
                1 => Some(self.value),
                3 => Some(b'%'),
                4 => Some(hex_digit(self.value / 16)),
                5 => Some(hex_digit(self.value % 16)),
                _ => None,
            }
        }
    }

    fn hex_digit(val: u8) -> u8 {
        debug_assert!(val < 16);
        if val < 10 {
            b'0' + val
        } else {
            b'A' + val - 10
        }
    }
}

#[cfg(test)]
mod test;
