use std::{
    borrow::Cow,
    sync::{Arc, LazyLock},
};

use bigdecimal::{BigDecimal, FromPrimitive, One, Zero};
use chrono::{Datelike, Timelike};
use rand::random;
use regex::{Captures, Regex, RegexBuilder};
use sha1::{Digest, Sha1};
use sha2::{Sha256, Sha384, Sha512};
use sophia_api::{
    ns::xsd,
    term::{BnodeId, IriRef, LanguageTag, Term},
};
use sophia_term::GenericLiteral;
use spargebra::algebra::Function::{self, *};
use uuid::{
    Uuid,
    fmt::{Hyphenated, Urn},
};

use crate::{
    ResultTerm,
    exec::ExecConfig,
    expression::EvalResult,
    ns::{RDF_DIR_LANG_STRING, RDF_LANG_STRING},
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
            if let Some(iri) = arg.as_iri("") {
                Some(str_iri(iri))
            } else if let Some(lit) = arg.as_literal("") {
                Some(str_literal(lit))
            } else {
                log::warn!("Str expects IRI or literal");
                None
            }
        }
        Lang => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(lang(arg.as_literal("Lang")?))
        }
        Datatype => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(datatype(arg.as_literal("Datatype")?))
        }
        Iri => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            if let Some(iri) = arg.as_iri("") {
                Some(iri.clone().into())
            } else if let Some(st) = arg.as_xsd_string("Iri") {
                Some(
                    IriRef::new(st.clone())
                        .inspect_err(|err| {
                            log::warn!("Iri#1 is an invalid IRI");
                            log::debug!(" {err}")
                        })
                        .ok()?
                        .into(),
                )
            } else {
                None
            }
        }
        BNode => {
            let o: Option<Option<i32>> = Some(None);
            match arguments.pop() {
                None => Some(bnode0()),
                Some(arg) => Some(bnode1(arg.as_xsd_string("Bnode")?)),
            }
        }
        Rand => Some(rand()),
        Abs => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number("Abs")?.abs().into())
        }
        Ceil => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number("Ceil")?.ceil().into())
        }
        Floor => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number("Floor")?.floor().into())
        }
        Round => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(arg.as_number("Round")?.round().into())
        }
        Concat => {
            let args = arguments
                .iter()
                .map(|e| e.as_string_lit("Concat arguments"))
                .collect::<Option<Vec<_>>>()?;
            Some(concat(&args))
        }
        LangMatches => {
            let [tag, range] = &arguments[..] else {
                unreachable!()
            };
            lang_matches(
                tag.as_xsd_string("LangMatch")?,
                range.as_xsd_string("LangMatch")?,
            )
        }
        SubStr => match &arguments[..] {
            [source, starting_loc] => sub_str(
                source.as_string_lit("SubStr#1")?,
                starting_loc.as_number("SubStr#2")?.coerce_to_double(),
                None,
            ),
            [source, starting_loc, length] => sub_str(
                source.as_string_lit("SubStr#1")?,
                starting_loc.as_number("SubStr#2")?.coerce_to_double(),
                Some(length.as_number("SubStr#3")?.coerce_to_double()),
            ),
            _ => unreachable!(),
        },
        StrLen => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(str_len(string.as_string_lit("StrLen")?.0))
        }
        Replace => {
            let (arg, pattern, replacement, flags) = match &arguments[..] {
                [a, p, r] => (a, p, r, None),
                [a, p, r, f] => (a, p, r, Some(f.as_xsd_string("Replace#4")?)),
                _ => unreachable!(),
            };
            let arg = arg.as_string_lit("Replace#1")?;
            let pattern = pattern.as_xsd_string("Replace#2")?;
            let replacement = replacement.as_xsd_string("Replace#3")?;
            replace(arg, pattern, replacement, flags)
        }
        UCase => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(u_case(string.as_string_lit("UCase")?))
        }
        LCase => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(l_case(string.as_string_lit("LCase")?))
        }
        EncodeForUri => {
            let [string] = &arguments[..] else {
                unreachable!();
            };
            Some(encode_for_uri(string.as_string_lit("EncodeForUri")?.0))
        }
        Contains => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            contains(
                heystack.as_string_lit("Contains#1")?,
                needle.as_string_lit("Contains#2")?,
            )
        }
        StrStarts => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strstarts(
                heystack.as_string_lit("StrStarts#1")?,
                needle.as_string_lit("StrStarts#2")?,
            )
        }
        StrEnds => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strends(
                heystack.as_string_lit("StrEnds#1")?,
                needle.as_string_lit("StrEnds#2")?,
            )
        }
        StrBefore => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strbefore(
                heystack.as_string_lit("StrBefore#1")?,
                needle.as_string_lit("StrBefore#2")?,
            )
        }
        StrAfter => {
            let [heystack, needle] = &arguments[..] else {
                unreachable!();
            };
            strafter(
                heystack.as_string_lit("StrAfter#1")?,
                needle.as_string_lit("StrAfter#2")?,
            )
        }
        Year => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(year(argument.as_xsd_date_time("Year")?))
        }
        Month => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(month(argument.as_xsd_date_time("Month")?))
        }
        Day => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(day(argument.as_xsd_date_time("Day")?))
        }
        Hours => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(hours(argument.as_xsd_date_time("Hours")?))
        }
        Minutes => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(minutes(argument.as_xsd_date_time("Minutes")?))
        }
        Seconds => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            Some(seconds(argument.as_xsd_date_time("Seconds")?))
        }
        Timezone => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            timezone(argument.as_xsd_date_time("Timezone")?)
        }
        Tz => {
            let [argument] = &arguments[..] else {
                unreachable!();
            };
            argument.as_xsd_date_time("Tz")?; // ensure it is an xsd:dateTime
            Some(tz(argument.as_literal("")?.get_lexical_form()))
        }
        Now => {
            debug_assert!(arguments.is_empty());
            Some(EvalResult::from(SparqlValue::DateTime(Some(
                XsdDateTime::Timezoned(config.now),
            ))))
        }
        Uuid => {
            debug_assert!(arguments.is_empty());
            Some(uuid())
        }
        StrUuid => {
            debug_assert!(arguments.is_empty());
            Some(str_uuid())
        }
        Md5 => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(md5_(arg.as_xsd_string("Md5")?))
        }
        Sha1 => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(sha1(arg.as_xsd_string("Sha1")?))
        }
        Sha256 => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(sha256(arg.as_xsd_string("Sha256")?))
        }
        Sha384 => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(sha384(arg.as_xsd_string("Sha384")?))
        }
        Sha512 => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(sha512(arg.as_xsd_string("Sha512")?))
        }
        StrLang => {
            let [lex, lang] = &arguments[..] else {
                unreachable!();
            };
            str_lang(
                lex.as_xsd_string("StrLang#1")?,
                lang.as_xsd_string("StrLang#2")?,
            )
        }
        StrDt => {
            let [lex, lang] = &arguments[..] else {
                unreachable!();
            };
            str_dt(lex.as_xsd_string("StrDr#1")?, lang.as_iri("StrDt#2")?)
        }
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
                [text, pattern, flags] => (text, pattern, Some(flags.as_xsd_string("Regex#3")?)),
                _ => unreachable!(),
            };
            let text = text.as_string_lit("Regex#1")?;
            let pattern = pattern.as_xsd_string("Regex#2")?;
            regex(text.0, pattern, flags)
        }
        Triple => {
            let [s, p, o] = &arguments[..] else {
                unreachable!()
            };
            triple(s, p, o)
        }
        Subject => {
            let [t] = &arguments[..] else { unreachable!() };
            t.as_triple("Subject")
                .map(|spo| ResultTerm::from(spo[0].clone()).into())
        }
        Predicate => {
            let [t] = &arguments[..] else { unreachable!() };
            t.as_triple("Predicate")
                .map(|spo| ResultTerm::from(spo[1].clone()).into())
        }
        Object => {
            let [t] = &arguments[..] else { unreachable!() };
            t.as_triple("Object")
                .map(|spo| ResultTerm::from(spo[2].clone()).into())
        }
        IsTriple => {
            let [arg] = &arguments[..] else {
                unreachable!()
            };
            Some(is_triple(arg))
        }
        Custom(iri) => {
            if iri.as_str().starts_with(*xsd::PREFIX) {
                let typ = &iri.as_str()[xsd::PREFIX.len()..];
                if arguments.len() != 1 {
                    log::warn!("Function xsd:{typ} expects 1 argument");
                    None
                } else {
                    match typ {
                        "boolean" => xsd_boolean(&arguments[0]),
                        "double" => xsd_double(&arguments[0]),
                        "float" => xsd_float(&arguments[0]),
                        "decimal" => xsd_decimal(&arguments[0]),
                        "integer" => xsd_integer(&arguments[0]),
                        "dateTime" => xsd_date_time(&arguments[0]),
                        "string" => xsd_string(&arguments[0]),
                        _ => {
                            log::warn!("Cast to xsd:{typ} not supported");
                            None
                        }
                    }
                }
            } else {
                log::warn!("Custom function not supported: {iri}");
                None
            }
        }
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

pub fn regex(text: &str, pattern: &str, flags: Option<&Arc<str>>) -> Option<EvalResult> {
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
                    log::warn!("unrecognized regex flag in '{flags}' (should be one of smixq)");
                    return None;
                }
            }
        }
        reb.build()
    } else {
        Regex::new(pattern)
    }
    .inspect_err(|err| log::warn!("invalid regex pattern: {err}"))
    .ok()
}

pub fn iri(st: &Arc<str>) -> Option<EvalResult> {
    IriRef::new(st.clone()).ok().map(EvalResult::from)
}

pub fn bnode0() -> EvalResult {
    let bnid = Uuid::now_v7().to_string();
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
    let tag = LanguageTag::new(tag.clone())
        .inspect_err(|err| {
            log::warn!("langMatch#1 is an invalid language tag");
            log::debug!(" {err}")
        })
        .ok()?;
    if range.as_ref() == "*" {
        return Some(true.into());
    }
    let range = LanguageTag::new(range.clone())
        .inspect_err(|err| {
            log::warn!("invalid language tag range: {err}");
            log::debug!(" {err}")
        })
        .ok()?;
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
        log::warn!("subStr#2 is NaN");
        return None;
    }
    let (lex, tag) = source;
    let lex_len = lex.chars().count();
    let (s, e) = match length {
        Some(l) if l.is_nan() => {
            log::warn!("subStr#3 is NaN");
            return None;
        }
        None | Some(f64::INFINITY) => (
            ((starting_loc.round() - 1.0) as usize).min(lex_len),
            lex_len,
        ),
        Some(l) => {
            let s_signed = starting_loc.round() as isize - 1;
            let s = (s_signed.max(0) as usize).min(lex_len);
            let e = ((s_signed + l.round() as isize).max(0) as usize)
                .max(s)
                .min(lex_len);
            (s, e)
        }
    };
    // s and e are "codepoint indexes", convert them to byte index
    let s = lex.char_indices().nth(s).map(|p| p.0).unwrap_or(lex.len());
    let e = lex.char_indices().nth(e).map(|p| p.0).unwrap_or(lex.len());
    Some(EvalResult::from((Arc::from(&lex[s..e]), tag.cloned())))
}

pub fn str_len(string: &str) -> EvalResult {
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

pub fn replace(
    arg: StringLiteral,
    pattern: &str,
    replacement: &str,
    flags: Option<&Arc<str>>,
) -> Option<EvalResult> {
    let re = make_regex(pattern, flags)?;
    if re.is_match("") {
        return None;
    }
    let repl = prepare_replacement(replacement);
    Some((re.replace_all(arg.0, repl).into(), arg.1.cloned()).into())
}

fn prepare_replacement(replacement: &str) -> Cow<str> {
    static REPL_ARG: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\$[0-9]+").unwrap());
    REPL_ARG.replace_all(replacement, |cap: &Captures| {
        format!("${{{}}}", &cap.get(0).unwrap().as_str()[1..])
    })
}

pub fn l_case(source: StringLiteral) -> EvalResult {
    let lex: String = source.0.chars().flat_map(char::to_lowercase).collect();
    EvalResult::from((Arc::from(lex), source.1.cloned()))
}

pub fn encode_for_uri(source: &str) -> EvalResult {
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
    check_compatible(heystack, needle, "arguments of Contains")?;
    Some(heystack.0.contains(needle.0.as_ref()).into())
}

pub fn strstarts(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle, "arguments of StrStarts")?;
    Some(heystack.0.starts_with(needle.0.as_ref()).into())
}

pub fn strends(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle, "arguments of StrEnds")?;
    Some(heystack.0.ends_with(needle.0.as_ref()).into())
}

pub fn strbefore(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle, "arguments of StrBefore")?;
    let found = heystack.0.find(needle.0.as_ref());
    Some(EvalResult::from((
        Arc::from(&heystack.0[..found.unwrap_or(0)]),
        found.and_then(|_| heystack.1.cloned()),
    )))
}

pub fn strafter(heystack: StringLiteral, needle: StringLiteral) -> Option<EvalResult> {
    check_compatible(heystack, needle, "arguments of StrAfter")?;
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
            Some(GenericLiteral::Typed(lex.into(), DATATYPE.clone()).into())
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

pub fn uuid() -> EvalResult {
    let mut buf = vec![b' '; Urn::LENGTH];
    let urn = Uuid::new_v4().urn().encode_lower(&mut buf[..]);
    let str = unsafe {
        // SAFETY: we now that buf contain only ASCII characters
        String::from_utf8_unchecked(buf)
    };
    IriRef::new_unchecked(Arc::<str>::from(str)).into()
}

pub fn str_uuid() -> EvalResult {
    let mut buf = vec![b' '; Hyphenated::LENGTH];
    let urn = Uuid::new_v4().hyphenated().encode_lower(&mut buf[..]);
    let str = unsafe {
        // SAFETY: we now that buf contain only ASCII characters
        String::from_utf8_unchecked(buf)
    };
    Arc::<str>::from(str).into()
}

pub fn md5_(arg: &str) -> EvalResult {
    Arc::<str>::from(format!("{:x}", md5::compute(arg.as_bytes()))).into()
}

pub fn sha1(arg: &str) -> EvalResult {
    let mut hasher = Sha1::new();
    hasher.update(arg.as_bytes());
    Arc::<str>::from(format!("{:x}", hasher.finalize())).into()
}

pub fn sha256(arg: &str) -> EvalResult {
    Arc::<str>::from(format!("{:x}", Sha256::digest(arg))).into()
}

pub fn sha384(arg: &str) -> EvalResult {
    Arc::<str>::from(format!("{:x}", Sha384::digest(arg))).into()
}

pub fn sha512(arg: &str) -> EvalResult {
    Arc::<str>::from(format!("{:x}", Sha512::digest(arg))).into()
}

pub fn str_lang(lex: &str, lang: &str) -> Option<EvalResult> {
    let lang = LanguageTag::new(lang.into())
        .inspect_err(|err| {
            log::warn!("StrLang#1 is an invalid language tag");
            log::debug!(" {err}")
        })
        .ok()?;
    Some((lex.into(), Some(lang)).into())
}

pub fn str_dt(lex: &str, dt: &IriRef<Arc<str>>) -> Option<EvalResult> {
    if &*RDF_LANG_STRING == dt || &*RDF_DIR_LANG_STRING == dt {
        None
    } else {
        Some(GenericLiteral::Typed(lex.into(), dt.clone()).into())
    }
}

pub fn triple(s: &EvalResult, p: &EvalResult, o: &EvalResult) -> Option<EvalResult> {
    let EvalResult::Term(s) = s else {
        log::warn!("not a valid subject");
        log::debug!("  {s:?}");
        return None;
    };
    let EvalResult::Term(p) = p else {
        log::warn!("not a valid predicate");
        log::debug!("  {p:?}");
        return None;
    };
    if !s.is_iri() && !s.is_blank_node() {
        log::warn!("not a valid subject");
        log::debug!("  {s:?}");
        return None;
    }
    if !p.is_iri() {
        log::warn!("not a valid predicate");
        log::debug!("  {p:?}");
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

pub fn xsd_boolean(arg: &EvalResult) -> Option<EvalResult> {
    // See https://www.w3.org/TR/sparql12-query/#FunctionMapping
    // and https://www.w3.org/TR/xpath-functions-31/#casting-boolean
    match arg.as_value() {
        Some(SparqlValue::Boolean(opt)) => opt.map(|_| arg.clone()),
        Some(SparqlValue::Number(n)) => Some(SparqlValue::from(n.is_truthy()).into()),
        Some(SparqlValue::String(lex, None)) => match lex.trim() {
            "1" | "true" => Some(SparqlValue::from(true).into()),
            "0" | "false" => Some(SparqlValue::from(false).into()),
            _ => None,
        },
        Some(SparqlValue::String(_, Some(_))) => None,
        Some(SparqlValue::DateTime(_)) => None,
        None => None,
    }
}

pub fn xsd_double(arg: &EvalResult) -> Option<EvalResult> {
    // See https://www.w3.org/TR/sparql12-query/#FunctionMapping
    // and https://www.w3.org/TR/xpath-functions-31/#casting-to-double
    match arg.as_value() {
        Some(SparqlValue::Number(SparqlNumber::Double(_))) => Some(arg.clone()),
        Some(SparqlValue::Number(n)) => Some(SparqlNumber::from(n.coerce_to_double()).into()),
        Some(SparqlValue::Boolean(opt)) => {
            opt.map(|b| SparqlNumber::from(if b { 1.0 } else { 0.0 }).into())
        }
        Some(SparqlValue::String(lex, None)) => lex
            .trim()
            .parse::<f64>()
            .ok()
            .map(|f| SparqlNumber::from(f).into()),
        Some(SparqlValue::String(_, Some(_))) => None,
        Some(SparqlValue::DateTime(_)) => None,
        None => None,
    }
}

pub fn xsd_float(arg: &EvalResult) -> Option<EvalResult> {
    // See https://www.w3.org/TR/sparql12-query/#FunctionMapping
    // and https://www.w3.org/TR/xpath-functions-31/#casting-to-float
    match arg.as_value() {
        Some(SparqlValue::Number(SparqlNumber::Float(_))) => Some(arg.clone()),
        Some(SparqlValue::Number(n)) => Some(SparqlNumber::from(n.coerce_to_float()).into()),
        Some(SparqlValue::Boolean(opt)) => {
            opt.map(|b| SparqlNumber::from(if b { 1.0_f32 } else { 0.0_f32 }).into())
        }
        Some(SparqlValue::String(lex, None)) => lex
            .trim()
            .parse::<f32>()
            .ok()
            .map(|f| SparqlNumber::from(f).into()),
        Some(SparqlValue::String(_, Some(_))) => None,
        Some(SparqlValue::DateTime(_)) => None,
        None => None,
    }
}

pub fn xsd_decimal(arg: &EvalResult) -> Option<EvalResult> {
    // See https://www.w3.org/TR/sparql12-query/#FunctionMapping
    // and https://www.w3.org/TR/xpath-functions-31/#casting-to-decimal
    match arg.as_value() {
        Some(SparqlValue::Number(SparqlNumber::Decimal(_))) => Some(arg.clone()),
        Some(SparqlValue::Number(SparqlNumber::Float(n))) => {
            Some(SparqlValue::Number(BigDecimal::from_f32(*n)?.into()).into())
        }
        Some(SparqlValue::Number(SparqlNumber::Double(n))) => {
            Some(SparqlValue::Number(BigDecimal::from_f64(*n)?.into()).into())
        }
        Some(SparqlValue::Number(n)) => Some(SparqlNumber::from(n.coerce_to_decimal()).into()),
        Some(SparqlValue::Boolean(opt)) => opt.map(|b| {
            SparqlNumber::from(if b {
                BigDecimal::one()
            } else {
                BigDecimal::zero()
            })
            .into()
        }),
        Some(SparqlValue::String(lex, None)) => lex
            .trim()
            .parse::<BigDecimal>()
            .ok()
            .map(|f| SparqlNumber::from(f).into()),
        Some(SparqlValue::String(_, Some(_))) => None,
        Some(SparqlValue::DateTime(_)) => None,
        None => None,
    }
}

pub fn xsd_integer(arg: &EvalResult) -> Option<EvalResult> {
    // See https://www.w3.org/TR/sparql12-query/#FunctionMapping
    // and https://www.w3.org/TR/xpath-functions-31/#casting-to-integer
    //
    // Do no forget to trim whitespaces when converting from string
    todo!()
}

pub fn xsd_date_time(arg: &EvalResult) -> Option<EvalResult> {
    // See https://www.w3.org/TR/sparql12-query/#FunctionMapping
    // and https://www.w3.org/TR/xpath-functions-31/#casting-to-datetimes
    //
    // Do no forget to trim whitespaces when converting from string
    todo!()
}

pub fn xsd_string(arg: &EvalResult) -> Option<EvalResult> {
    // The following is not right
    // See https://www.w3.org/TR/xpath-functions-31/#casting-to-string
    todo!()
}

type StringLiteral<'a> = (&'a Arc<str>, Option<&'a LanguageTag<Arc<str>>>);

fn check_compatible(arg1: StringLiteral, arg2: StringLiteral, diag: &str) -> Option<()> {
    match (arg1.1, arg2.1) {
        (_, None) => true,
        (Some(tag1), Some(tag2)) if tag1 == tag2 => true,
        _ => {
            log::warn!("{diag} are not compatible");
            log::debug!("  {arg1:?} {arg2:?}");
            false
        }
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
