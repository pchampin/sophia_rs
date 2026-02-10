#![allow(clippy::module_name_repetitions)]

use std::{cmp::Ordering, sync::Arc};

use bigdecimal::{BigDecimal, Signed};
use sophia_api::{MownStr, ns::xsd, term::IriRef};
use sophia_term::{ArcTerm, GenericLiteral};

mod _xsd_date_time;
pub use _xsd_date_time::XsdDateTime;
mod _number;
pub use _number::SparqlNumber;

use crate::{
    expression::CompoundTag,
    ns::{
        RDF_LANG_STRING, XSD_BOOLEAN, XSD_DATE_TIME, XSD_DECIMAL, XSD_DOUBLE, XSD_FLOAT,
        XSD_INTEGER, XSD_STRING,
    },
};

/// A value of one of the recognized datatypes.
#[derive(Clone, Debug)]
pub enum SparqlValue {
    /// A value of type xsd:boolean, or None if the lexical value is invalid
    Boolean(Option<bool>),
    /// A value of type xsd:dateTime, or None if the lexical value is invalid
    DateTime(Option<XsdDateTime>),
    /// A value of a recognized numeric datatype, or None of the lexical value is invalid
    Number(Option<SparqlNumber>),
    /// A value of type xsd:string or rdf:langString (if the language tag is not None)
    String(Arc<str>, Option<CompoundTag>),
}

impl SparqlValue {
    pub fn try_from_term(term: &ArcTerm) -> Option<Self> {
        if let ArcTerm::Literal(genlit) = term {
            Self::try_from_literal(genlit)
        } else {
            None
        }
    }

    pub fn try_from_parts<T: LexicalPart>(lex: T, datatype: IriRef<&str>) -> Option<Self> {
        let dt = datatype.as_str();
        if !dt.starts_with(xsd::PREFIX.as_str()) {
            return None;
        }
        match &dt[xsd::PREFIX.len()..] {
            "integer" => Some(Self::Number(SparqlNumber::try_parse_integer(lex.as_str()))),
            "decimal" => Some(Self::Number(SparqlNumber::try_parse::<BigDecimal>(
                lex.as_str(),
            ))),
            "float" => Some(Self::Number(SparqlNumber::try_parse::<f32>(lex.as_str()))),
            "double" => Some(Self::Number(SparqlNumber::try_parse::<f64>(lex.as_str()))),
            "string" => Some(Self::String(lex.to_arc(), None)),
            "boolean" => Some(Self::Boolean(match lex.as_str() {
                "true" | "1" => Some(true),
                "false" | "0" => Some(false),
                _ => None,
            })),
            "dateTime" => Some(Self::DateTime(lex.as_str().parse().ok())),
            "nonPositiveInteger" => Some(Self::Number(
                SparqlNumber::try_parse_integer(lex.as_str()).filter(|n| !n.is_positive()),
            )),
            "negativeInteger" => Some(Self::Number(
                SparqlNumber::try_parse_integer(lex.as_str())
                    .filter(_number::SparqlNumber::is_negative),
            )),
            "long" => Some(Self::Number(SparqlNumber::try_parse::<i64>(lex.as_str()))),
            "int" => Some(Self::Number(SparqlNumber::try_parse::<i32>(lex.as_str()))),
            "short" => Some(Self::Number(SparqlNumber::try_parse::<i16>(lex.as_str()))),
            "byte" => Some(Self::Number(SparqlNumber::try_parse::<i8>(lex.as_str()))),
            "nonNegativeInteger" => Some(Self::Number(
                SparqlNumber::try_parse_integer(lex.as_str()).filter(|n| !n.is_negative()),
            )),
            "unsignedLong" => Some(Self::Number(SparqlNumber::try_parse::<u64>(lex.as_str()))),
            "unsignedInt" => Some(Self::Number(SparqlNumber::try_parse::<u32>(lex.as_str()))),
            "unsignedShort" => Some(Self::Number(SparqlNumber::try_parse::<u16>(lex.as_str()))),
            "unsignedByte" => Some(Self::Number(SparqlNumber::try_parse::<u8>(lex.as_str()))),
            "positiveInteger" => Some(Self::Number(
                SparqlNumber::try_parse_integer(lex.as_str())
                    .filter(_number::SparqlNumber::is_positive),
            )),
            _ => None,
        }
    }

    pub fn try_from_literal(genlit: &GenericLiteral<Arc<str>>) -> Option<Self> {
        match genlit {
            GenericLiteral::LanguageString(lex, tag, dir) => {
                Some(Self::String(lex.clone(), Some((tag.clone(), *dir))))
            }
            GenericLiteral::Typed(lex, dt) => Self::try_from_parts(lex, dt.as_ref()),
        }
    }

    pub fn is_ill_formed(&self) -> bool {
        match self {
            SparqlValue::Boolean(opt) => opt.is_none(),
            SparqlValue::DateTime(opt) => opt.is_none(),
            SparqlValue::Number(opt) => opt.is_none(),
            SparqlValue::String(_, _) => false,
        }
    }

    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            SparqlValue::Boolean(opt) => opt.or(Some(false)),
            SparqlValue::DateTime(_) => None,
            SparqlValue::Number(opt) => opt.as_ref().map(SparqlNumber::is_truthy),
            SparqlValue::String(val, _) => Some(!val.is_empty()),
        }
    }

    pub fn sparql_eq(&self, other: &Self) -> Option<bool> {
        use SparqlValue::*;
        match (self, other) {
            (Boolean(Some(b1)), Boolean(Some(b2))) => Some(b1 == b2),
            (DateTime(Some(d1)), DateTime(Some(d2))) => {
                d1.partial_cmp(d2).map(|o| o == Ordering::Equal)
            }
            (Number(Some(n1)), Number(Some(n2))) => Some(n1 == n2),
            (String(s1, None), String(s2, None)) => Some(s1 == s2),
            (String(s1, Some(t1)), String(s2, Some(t2))) => Some(t1 == t2 && s1 == s2),
            _ => None,
        }
    }

    pub fn lexical_form_arc<F>(&self, mut factory: F) -> Arc<str>
    where
        F: FnMut(&str) -> Arc<str>,
    {
        use SparqlNumber::*;
        use SparqlValue::*;
        match self {
            Boolean(Some(b)) => factory(if *b { "true" } else { "false" }),
            DateTime(Some(d)) => factory(&d.to_string()),
            Number(Some(NativeInt(i))) => factory(&i.to_string()),
            Number(Some(BigInt(i))) => factory(&i.to_string()),
            Number(Some(Decimal(d))) => factory(&dec2string(d)),
            Number(Some(Float(f))) if f.is_infinite() && f.is_positive() => factory("INF"),
            Number(Some(Float(f))) if f.is_infinite() && f.is_negative() => factory("-INF"),
            Number(Some(Float(f))) => factory(&format!("{f:e}")),
            Number(Some(Double(d))) if d.is_infinite() && d.is_positive() => factory("INF"),
            Number(Some(Double(d))) if d.is_infinite() && d.is_negative() => factory("-INF"),
            Number(Some(Double(d))) => factory(&format!("{d:e}")),
            String(lex, _) => lex.clone(),
            Boolean(None) | DateTime(None) | Number(None) => factory("ill-typed"),
        }
    }

    pub fn lexical_form(&self) -> MownStr<'_> {
        use SparqlNumber::*;
        use SparqlValue::*;
        match self {
            Boolean(Some(b)) => (if *b { "true" } else { "false" }).into(),
            DateTime(Some(d)) => d.to_string().into(),
            Number(Some(NativeInt(i))) => i.to_string().into(),
            Number(Some(BigInt(i))) => i.to_string().into(),
            Number(Some(Decimal(d))) => dec2string(d).into(),
            Number(Some(Float(f))) if f.is_infinite() && f.is_positive() => ("INF").into(),
            Number(Some(Float(f))) if f.is_infinite() && f.is_negative() => ("-INF").into(),
            Number(Some(Float(f))) => format!("{f:e}").into(),
            Number(Some(Double(d))) if d.is_infinite() && d.is_positive() => ("INF").into(),
            Number(Some(Double(d))) if d.is_infinite() && d.is_negative() => ("-INF").into(),
            Number(Some(Double(d))) => format!("{d:e}").into(),
            String(lex, _) => lex.as_ref().into(),
            Boolean(None) | DateTime(None) | Number(None) => ("ill-typed").into(),
        }
    }

    pub fn datatype(&self) -> IriRef<Arc<str>> {
        use SparqlNumber::*;
        use SparqlValue::*;
        match self {
            Boolean(_) => XSD_BOOLEAN.clone(),
            DateTime(_) => XSD_DATE_TIME.clone(),
            Number(Some(NativeInt(_) | BigInt(_))) => XSD_INTEGER.clone(),
            Number(Some(Decimal(_))) => XSD_DECIMAL.clone(),
            Number(Some(Float(_))) => XSD_FLOAT.clone(),
            Number(Some(Double(_)) | None) => XSD_DOUBLE.clone(),
            String(_, None) => XSD_STRING.clone(),
            String(_, Some(_)) => RDF_LANG_STRING.clone(),
        }
    }

    pub fn compound_tag(&self) -> Option<&CompoundTag> {
        use SparqlValue::*;
        if let String(_, r) = self {
            r.as_ref()
        } else {
            None
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
        SparqlValue::Number(Some(value))
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
            (Boolean(Some(b1)), Boolean(Some(b2))) => b1.partial_cmp(b2),
            (DateTime(Some(d1)), DateTime(Some(d2))) => d1.partial_cmp(d2),
            (Number(Some(n1)), Number(Some(n2))) => n1.partial_cmp(&n2),
            (String(s1, None), String(s2, None)) => Some(s1.cmp(s2)),
            (String(s1, Some(t1)), String(s2, Some(t2))) => {
                Some(t1.cmp(t2).then_with(|| s1.cmp(s2)))
            }
            _ => None,
        }
    }
}

fn dec2string(d: &BigDecimal) -> String {
    let d = d.normalized();
    if d.fractional_digit_count() <= 0 {
        format!("{}.0", d.with_scale(0))
    } else {
        d.to_string()
    }
}

/// Trait use for an argument of [`SparqlValue::try_from_parts`]
pub trait LexicalPart {
    fn as_str(&self) -> &str;
    fn to_arc(self) -> Arc<str>;
}

impl LexicalPart for &'_ str {
    fn as_str(&self) -> &str {
        self
    }

    fn to_arc(self) -> Arc<str> {
        Arc::from(self)
    }
}

impl LexicalPart for &'_ Arc<str> {
    fn as_str(&self) -> &str {
        self.as_ref()
    }

    fn to_arc(self) -> Arc<str> {
        self.clone()
    }
}
