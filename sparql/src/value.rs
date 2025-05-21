#![allow(clippy::module_name_repetitions)]

use std::{cmp::Ordering, sync::Arc};

use bigdecimal::{BigDecimal, Signed};
use sophia_api::{
    ns::xsd,
    term::{IriRef, LanguageTag},
};
use sophia_term::{ArcTerm, GenericLiteral};

mod _xsd_date_time;
pub use _xsd_date_time::XsdDateTime;
mod _number;
pub use _number::SparqlNumber;

use crate::ns::{
    RDF_LANG_STRING, XSD_BOOLEAN, XSD_DATE_TIME, XSD_DECIMAL, XSD_DOUBLE, XSD_FLOAT, XSD_INTEGER,
    XSD_STRING,
};

#[derive(Clone, Debug)]
pub enum SparqlValue {
    Number(SparqlNumber),
    String(Arc<str>, Option<LanguageTag<Arc<str>>>),
    Boolean(Option<bool>),
    DateTime(Option<XsdDateTime>),
}

impl SparqlValue {
    pub fn try_from_term(term: &ArcTerm) -> Option<Self> {
        if let ArcTerm::Literal(genlit) = term {
            Self::try_from_literal(genlit)
        } else {
            None
        }
    }

    pub fn try_from_literal(genlit: &GenericLiteral<Arc<str>>) -> Option<Self> {
        match genlit {
            GenericLiteral::LanguageString(lex, tag) => {
                Some(Self::String(lex.clone(), Some(tag.clone())))
            }
            GenericLiteral::Typed(lex, dt) => {
                let dt = dt.as_str();
                if !dt.starts_with(xsd::PREFIX.as_str()) {
                    return None;
                }
                match &dt[xsd::PREFIX.len()..] {
                    "integer" => Some(Self::Number(SparqlNumber::try_parse_integer(lex)?)),
                    "decimal" => Some(Self::Number(SparqlNumber::try_parse::<BigDecimal>(lex)?)),
                    "float" => Some(Self::Number(SparqlNumber::try_parse::<f32>(lex)?)),
                    "double" => Some(Self::Number(SparqlNumber::try_parse::<f64>(lex)?)),
                    "string" => Some(Self::String(lex.clone(), None)),
                    "boolean" => Some(Self::Boolean(match lex.as_ref() {
                        "true" | "1" => Some(true),
                        "false" | "0" => Some(false),
                        _ => None,
                    })),
                    "dateTime" => Some(Self::DateTime(lex.parse().ok())),
                    "nonPositiveInteger" => Some(Self::Number(
                        SparqlNumber::try_parse_integer(lex)?.check(|n| !n.is_positive())?,
                    )),
                    "negativeInteger" => Some(Self::Number(
                        SparqlNumber::try_parse_integer(lex)?
                            .check(_number::SparqlNumber::is_negative)?,
                    )),
                    "long" => Some(Self::Number(SparqlNumber::try_parse::<i64>(lex)?)),
                    "int" => Some(Self::Number(SparqlNumber::try_parse::<i32>(lex)?)),
                    "short" => Some(Self::Number(SparqlNumber::try_parse::<i16>(lex)?)),
                    "byte" => Some(Self::Number(SparqlNumber::try_parse::<i8>(lex)?)),
                    "nonNegativeInteger" => Some(Self::Number(
                        SparqlNumber::try_parse_integer(lex)?.check(|n| !n.is_negative())?,
                    )),
                    "unsignedLong" => Some(Self::Number(SparqlNumber::try_parse::<u64>(lex)?)),
                    "unsignedInt" => Some(Self::Number(SparqlNumber::try_parse::<u32>(lex)?)),
                    "unsignedShort" => Some(Self::Number(SparqlNumber::try_parse::<u16>(lex)?)),
                    "unsignedByte" => Some(Self::Number(SparqlNumber::try_parse::<u8>(lex)?)),
                    "positiveInteger" => Some(Self::Number(
                        SparqlNumber::try_parse_integer(lex)?
                            .check(_number::SparqlNumber::is_positive)?,
                    )),
                    _ => None,
                }
            }
        }
    }

    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            SparqlValue::Number(n) => Some(n.is_truthy()),
            SparqlValue::String(val, _) => Some(!val.is_empty()),
            SparqlValue::Boolean(opt) => opt.or(Some(false)),
            SparqlValue::DateTime(_) => None,
        }
    }

    pub fn sparql_eq(&self, other: &Self) -> Option<bool> {
        use SparqlValue::*;
        match (self, other) {
            (Number(n1), Number(n2)) => Some(n1 == n2),
            (String(s1, None), String(s2, None)) => Some(s1 == s2),
            (String(s1, Some(t1)), String(s2, Some(t2))) => Some(t1 == t2 && s1 == s2),
            (Boolean(b1), Boolean(b2)) => Some(b1 == b2),
            (DateTime(d1), DateTime(d2)) => d1.partial_cmp(d2).map(|o| o == Ordering::Equal),
            _ => None,
        }
    }

    pub fn lexical_form<F>(&self, mut factory: F) -> Arc<str>
    where
        F: FnMut(&str) -> Arc<str>,
    {
        use SparqlNumber::*;
        use SparqlValue::*;
        match self {
            Number(NativeInt(i)) => factory(&i.to_string()),
            Number(BigInt(i)) => factory(&i.to_string()),
            Number(Decimal(d)) => factory(&dec2string(d)),
            Number(Float(f)) if f.is_infinite() && f.is_positive() => factory("INF"),
            Number(Float(f)) if f.is_infinite() && f.is_negative() => factory("-INF"),
            Number(Float(f)) => factory(&format!("{f:e}")),
            Number(Double(d)) if d.is_infinite() && d.is_positive() => factory("INF"),
            Number(Double(d)) if d.is_infinite() && d.is_negative() => factory("-INF"),
            Number(Double(d)) => factory(&format!("{d:e}")),
            Boolean(Some(b)) => factory(if *b { "true" } else { "false" }),
            DateTime(Some(d)) => factory(&d.to_string()),
            Boolean(None) | DateTime(None) => factory("ill-formed"),
            String(lex, _) => lex.clone(),
        }
    }

    pub fn datatype(&self) -> IriRef<Arc<str>> {
        use SparqlNumber::*;
        use SparqlValue::*;
        match self {
            Number(NativeInt(_) | BigInt(_)) => XSD_INTEGER.clone(),
            Number(Decimal(_)) => XSD_DECIMAL.clone(),
            Number(Float(_)) => XSD_FLOAT.clone(),
            Number(Double(_)) => XSD_DOUBLE.clone(),
            Boolean(_) => XSD_BOOLEAN.clone(),
            DateTime(_) => XSD_DATE_TIME.clone(),
            String(_, None) => XSD_STRING.clone(),
            String(_, Some(_)) => RDF_LANG_STRING.clone(),
        }
    }
}

impl From<bool> for SparqlValue {
    fn from(value: bool) -> Self {
        SparqlValue::Boolean(Some(value))
    }
}

impl From<SparqlNumber> for SparqlValue {
    fn from(value: SparqlNumber) -> Self {
        SparqlValue::Number(value)
    }
}

impl PartialEq for SparqlValue {
    fn eq(&self, other: &Self) -> bool {
        self.sparql_eq(other).unwrap_or(false)
    }
}

impl PartialOrd for SparqlValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use SparqlValue::*;
        match (self, other) {
            (Number(n1), Number(n2)) => n1.partial_cmp(&n2),
            (String(s1, None), String(s2, None)) => Some(s1.cmp(s2)),
            (String(s1, Some(t1)), String(s2, Some(t2))) => {
                Some(t1.cmp(t2).then_with(|| s1.cmp(s2)))
            }
            (Boolean(Some(b1)), Boolean(Some(b2))) => b1.partial_cmp(b2),
            (DateTime(Some(d1)), DateTime(Some(d2))) => d1.partial_cmp(d2),
            _ => None,
        }
    }
}

pub fn dec2string(d: &BigDecimal) -> String {
    let d = d.normalized();
    if d.fractional_digit_count() <= 0 {
        format!("{}.0", d.with_scale(0))
    } else {
        d.to_string()
    }
}
