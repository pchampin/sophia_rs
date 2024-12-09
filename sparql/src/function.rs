use std::sync::Arc;

use rand::random;
use sophia_api::term::{BnodeId, IriRef, LanguageTag, Term};
use sophia_term::GenericLiteral;
use spargebra::algebra::Function::{self, *};

use crate::{
    expression::EvalResult,
    ns::RDF_LANG_STRING,
    value::{SparqlNumber, SparqlValue},
    ResultTerm,
};

#[allow(clippy::module_name_repetitions, clippy::too_many_lines)]
pub fn call_function(function: &Function, mut arguments: Vec<EvalResult>) -> Option<EvalResult> {
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
        EncodeForUri => todo("EncodeForUri"),
        Contains => todo("Contains"),
        StrStarts => todo("StrStarts"),
        StrEnds => todo("StrEnds"),
        StrBefore => todo("StrBefore"),
        StrAfter => todo("StrAfter"),
        Year => todo("Year"),
        Month => todo("Month"),
        Day => todo("Day"),
        Hours => todo("Hours"),
        Minutes => todo("Minutes"),
        Seconds => todo("Seconds"),
        Timezone => todo("Timezone"),
        Tz => todo("Tz"),
        Now => todo("Now"),
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
        Regex => todo("Regex"),
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

#[expect(clippy::type_complexity)]
pub fn concat(args: &[(&Arc<str>, Option<&LanguageTag<Arc<str>>>)]) -> EvalResult {
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
    source: (&Arc<str>, Option<&LanguageTag<Arc<str>>>),
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
    let l = string.len();
    if l <= isize::MAX as usize {
        SparqlNumber::from(l as isize).into()
    } else {
        todo!()
    }
}

pub fn u_case(source: (&Arc<str>, Option<&LanguageTag<Arc<str>>>)) -> EvalResult {
    let lex: String = source.0.chars().flat_map(char::to_uppercase).collect();
    EvalResult::from((Arc::from(lex), source.1.cloned()))
}

pub fn l_case(source: (&Arc<str>, Option<&LanguageTag<Arc<str>>>)) -> EvalResult {
    let lex: String = source.0.chars().flat_map(char::to_lowercase).collect();
    EvalResult::from((Arc::from(lex), source.1.cloned()))
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

#[cfg(test)]
mod test;
