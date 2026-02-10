use std::{borrow::Borrow, sync::LazyLock};

use smallvec::SmallVec;
use sophia_api::{
    MownStr,
    ns::{NsTerm, xsd},
    term::IriRef,
};
use sophia_sparql::value::{SparqlNumber, SparqlValue};

use crate::d_entailment::{IllTypedLiteral, LexDt, Recognized};

/// A [`Recognized`] recognizing all the datatype required by SPARQL.
///
/// # Normalized form
///
/// For all literals of type `xsd:decimal` (or any of its subtype),
/// the normalized form (returned by [[Sparql::try_normalize]]
/// and [[Sparql::normalize_or_fallback]]) will be
/// * `xsd:integer` if the value is an integer,
/// * `xsd:decimal` otherwise.
#[derive(Clone, Copy, Default)]
pub struct Sparql;

impl Recognized for Sparql {
    fn try_normalize<T: Borrow<str>>(
        (lex, datatype): LexDt<T>,
    ) -> Result<Result<LexDt<Box<str>>, IllTypedLiteral>, LexDt<T>> {
        match sophia_sparql::value::SparqlValue::try_from_parts(lex.borrow(), datatype.as_ref()) {
            Some(v) => Ok({
                if v.is_ill_formed() {
                    Err(IllTypedLiteral::new(lex.borrow(), datatype.as_ref()))
                } else if let SparqlValue::Number(Some(n)) = &v {
                    let mut lex: String = v.lexical_form().to_string();
                    let dt = match n {
                        SparqlNumber::NativeInt(_) | SparqlNumber::BigInt(_) => {
                            XSD_INTEGER_BOX.clone()
                        }
                        SparqlNumber::Decimal(_) => {
                            if lex.ends_with(".0") {
                                lex.truncate(lex.len() - 2);
                                XSD_INTEGER_BOX.clone()
                            } else {
                                XSD_DECIMAL_BOX.clone()
                            }
                        }
                        SparqlNumber::Float(_) | SparqlNumber::Double(_) => datatype,
                    };
                    Ok((lex.into(), dt))
                } else {
                    Ok((v.lexical_form().into(), datatype))
                }
            }),
            None => Err((lex, datatype)),
        }
    }

    fn datatypes() -> impl Iterator<Item = IriRef<MownStr<'static>>> + Send {
        [
            xsd::integer,
            xsd::decimal,
            xsd::float,
            xsd::double,
            xsd::string,
            xsd::boolean,
            xsd::dateTime,
            xsd::nonPositiveInteger,
            xsd::negativeInteger,
            xsd::long,
            xsd::int,
            xsd::short,
            xsd::byte,
            xsd::nonNegativeInteger,
            xsd::unsignedLong,
            xsd::unsignedInt,
            xsd::unsignedShort,
            xsd::unsignedByte,
            xsd::positiveInteger,
        ]
        .map(NsTerm::to_iriref)
        .into_iter()
    }

    fn witnesses() -> impl Iterator<Item = (MownStr<'static>, IriRef<MownStr<'static>>)> + Send {
        [
            ("0".into(), xsd::integer.to_iriref()),
            ("1".into(), xsd::integer.to_iriref()),
            ("-1".into(), xsd::integer.to_iriref()),
            ("0e0".into(), xsd::float.to_iriref()),
            ("0e0".into(), xsd::double.to_iriref()),
            ("true".into(), xsd::boolean.to_iriref()),
            ("1970-01-01T00:00:00".into(), xsd::dateTime.to_iriref()),
        ]
        .into_iter()
    }

    fn datatypes_for(
        lex: &str,
        datatype: IriRef<&str>,
    ) -> Option<SmallVec<[IriRef<MownStr<'static>>; 16]>> {
        debug_assert!({
            let dt = datatype.map_unchecked(MownStr::from_ref);
            if let Ok((l2, d2)) = Self::normalize_or_fallback((Box::from(lex), dt))
                && l2.as_ref() == lex
                && d2.as_str() == datatype.as_str()
            {
                true
            } else {
                false
            }
        });
        if !datatype.as_str().starts_with(xsd::PREFIX.as_str()) {
            return None;
        }
        match &datatype.as_str()[xsd::PREFIX.len()..] {
            "boolean" | "double" | "dateTime" | "float" => Some(SmallVec::new()),
            "decimal" => {
                if lex.ends_with(".0") {
                    Some(SmallVec::new())
                } else {
                    Some(datatypes_integer(&lex[..lex.len() - 2], true))
                }
            }
            "integer" => Some(datatypes_integer(lex, false)),
            "nonPositiveInteger" | "negativeInteger" | "long" | "int" | "short" | "byte"
            | "nonNegativeInteger" | "unsignedLong" | "unsignedInt" | "unsignedShort"
            | "unsignedByte" | "positiveInteger"
                if cfg!(debug_assertions) =>
            {
                unreachable!("integers should be normalized to xsd:integer")
            }
            _ => None,
        }
    }
}

fn datatypes_integer(lex: &str, decimal: bool) -> SmallVec<[IriRef<MownStr<'static>>; 16]> {
    let val: Option<i128> = lex.parse().ok();
    let mut v = SmallVec::<[NsTerm; 16]>::new();
    if decimal {
        v.push(xsd::integer);
        // NB: this is NOT an error:
        // the literal is *known* to be an xsd:decimal,
        // so we add the unknown type xsd:integer
        // (and vice-versa)
    } else {
        v.push(xsd::decimal);
    }
    if let Some(val) = val {
        if ((i64::MIN as i128)..=(i64::MAX as i128)).contains(&val) {
            v.push(xsd::long);
            if ((i32::MIN as i128)..=(i32::MAX as i128)).contains(&val) {
                v.push(xsd::int);
                if ((i16::MIN as i128)..=(i16::MAX as i128)).contains(&val) {
                    v.push(xsd::short);
                    if ((i8::MIN as i128)..=(i8::MAX as i128)).contains(&val) {
                        v.push(xsd::byte);
                    }
                }
            }
        }
        if (0_i128..=(u64::MAX as i128)).contains(&val) {
            v.push(xsd::unsignedLong);
            if (0_i128..=(u32::MAX as i128)).contains(&val) {
                v.push(xsd::unsignedInt);
                if (0_i128..=(u16::MAX as i128)).contains(&val) {
                    v.push(xsd::unsignedShort);
                    if (0_i128..=(u8::MAX as i128)).contains(&val) {
                        v.push(xsd::unsignedByte);
                    }
                }
            }
        }
    }
    if lex.starts_with("-") {
        v.push(xsd::negativeInteger);
        v.push(xsd::nonPositiveInteger);
    } else if lex == "0" {
        v.push(xsd::nonNegativeInteger);
        v.push(xsd::nonPositiveInteger);
    } else {
        v.push(xsd::nonNegativeInteger);
        v.push(xsd::positiveInteger);
    }
    v.into_iter()
        .map(|i| i.to_iriref().map_unchecked(MownStr::from))
        .collect()
}

static XSD_DECIMAL_BOX: LazyLock<IriRef<MownStr<'static>>> =
    LazyLock::new(|| xsd::decimal.iriref());
static XSD_INTEGER_BOX: LazyLock<IriRef<MownStr<'static>>> =
    LazyLock::new(|| xsd::integer.iriref());
