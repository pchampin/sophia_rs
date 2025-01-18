//! An `XsdDateTime` is a dateTime with or without a timezone.
//! See <https://www.w3.org/TR/xmlschema-2/#dt-dateTime>
//!
//! # Comparing dateTimes
//!
//! According to [Section 3.2.7.4 Order relation on dateTime of XML Schema Part 2](https://www.w3.org/TR/xmlschema-2/#dt-dateTime)
//! dateTimes are incomparable in some circumstances.
//! However, the `XPath` functions [`op:date-equal`](https://www.w3.org/TR/xpath-functions/#func-date-equal)
//! and [`op:date-less-than`](https://www.w3.org/TR/xpath-functions/#func-date-less-than),
//! which the SPARQL specification refers to,
//! solve this ambiguity with an [implicit timezone],
//! which is [controversial](https://github.com/w3c/sparql-query/issues/116).
//!
//! This implementation uses no implicit timezone.
use std::{cmp::Ordering, fmt::Display, str::FromStr};

use chrono::{format::ParseErrorKind, DateTime, Datelike, FixedOffset, NaiveDateTime};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum XsdDateTime {
    Naive(NaiveDateTime),
    Timezoned(DateTime<FixedOffset>),
}

impl FromStr for XsdDateTime {
    type Err = chrono::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        DateTime::parse_from_str(s, "%+")
            .map(Self::Timezoned)
            .or_else(|e| {
                if e.kind() == ParseErrorKind::TooShort {
                    s.parse().map(Self::Naive)
                } else {
                    Err(e)
                }
            })
    }
}

impl Display for XsdDateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                XsdDateTime::Naive(d) => {
                    let mut out = d.and_utc().to_rfc3339();
                    out.truncate(out.len() - 6); // truncate timezone
                    out
                }
                XsdDateTime::Timezoned(d) => {
                    d.to_rfc3339()
                }
            }
        )
    }
}

impl PartialOrd for XsdDateTime {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (XsdDateTime::Naive(d1), XsdDateTime::Naive(d2)) => d1.partial_cmp(d2),
            (XsdDateTime::Naive(d1), XsdDateTime::Timezoned(d2)) => {
                heterogeneous_cmp(d2, d1).map(Ordering::reverse)
            }
            (XsdDateTime::Timezoned(d1), XsdDateTime::Naive(d2)) => heterogeneous_cmp(d1, d2),
            (XsdDateTime::Timezoned(d1), XsdDateTime::Timezoned(d2)) => d1.partial_cmp(d2),
        }
    }
}

impl Datelike for XsdDateTime {
    fn year(&self) -> i32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.year(),
            XsdDateTime::Timezoned(date_time) => date_time.year(),
        }
    }

    fn month(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.month(),
            XsdDateTime::Timezoned(date_time) => date_time.month(),
        }
    }

    fn month0(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.month0(),
            XsdDateTime::Timezoned(date_time) => date_time.month0(),
        }
    }

    fn day(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.day(),
            XsdDateTime::Timezoned(date_time) => date_time.day(),
        }
    }

    fn day0(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.day0(),
            XsdDateTime::Timezoned(date_time) => date_time.day0(),
        }
    }

    fn ordinal(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.ordinal(),
            XsdDateTime::Timezoned(date_time) => date_time.ordinal(),
        }
    }

    fn ordinal0(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.ordinal0(),
            XsdDateTime::Timezoned(date_time) => date_time.ordinal0(),
        }
    }

    fn weekday(&self) -> chrono::Weekday {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.weekday(),
            XsdDateTime::Timezoned(date_time) => date_time.weekday(),
        }
    }

    fn iso_week(&self) -> chrono::IsoWeek {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.iso_week(),
            XsdDateTime::Timezoned(date_time) => date_time.iso_week(),
        }
    }

    fn with_year(&self, year: i32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_year(year)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_year(year)?))
            }
        }
    }

    fn with_month(&self, month: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_month(month)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_month(month)?))
            }
        }
    }

    fn with_month0(&self, month0: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_month0(month0)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_month0(month0)?))
            }
        }
    }

    fn with_day(&self, day: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_day(day)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_day(day)?))
            }
        }
    }

    fn with_day0(&self, day0: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_day0(day0)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_day0(day0)?))
            }
        }
    }

    fn with_ordinal(&self, ordinal: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_ordinal(ordinal)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_ordinal(ordinal)?))
            }
        }
    }

    fn with_ordinal0(&self, ordinal0: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_ordinal0(ordinal0)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_ordinal0(ordinal0)?))
            }
        }
    }
}

/// Implements <https://www.w3.org/TR/xmlschema-2/#dateTime-order>
fn heterogeneous_cmp(d1: &DateTime<FixedOffset>, d2: &NaiveDateTime) -> Option<Ordering> {
    if d1 < &naive_to_fixed(d2, 14) {
        Some(Ordering::Less)
    } else if d1 > &naive_to_fixed(d2, -14) {
        Some(Ordering::Greater)
    } else {
        None
    }
}

fn naive_to_fixed(d: &NaiveDateTime, offset: i8) -> DateTime<FixedOffset> {
    debug_assert!((-14..=14).contains(&offset));
    let fixed_offset = FixedOffset::east_opt(i32::from(offset) * 3600).unwrap();
    match d.and_local_timezone(fixed_offset) {
        chrono::offset::LocalResult::Single(r) => r,
        _ => unreachable!(), // FixedOffset has no fold or gap, so there is always a single result
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test_case("2024-09-17T12:00:00"; "no timezone")]
    fn naive(d: &str) {
        assert!(matches!(
            d.parse::<XsdDateTime>().unwrap(),
            XsdDateTime::Naive(_)
        ));
    }

    #[test_case("2024-09-17T12:00:00Z"; "z")]
    #[test_case("2024-09-17T12:00:00+00:00"; "plus 0")]
    #[test_case("2024-09-17T12:00:00-00:00"; "minus 0")]
    #[test_case("2024-09-17T12:00:00+01:59"; "plus 1:59")]
    #[test_case("2024-09-17T12:00:00-01:59"; "minus 1:59")]
    #[test_case("2024-09-17T12:00:00+14:00"; "plus 14")]
    #[test_case("2024-09-17T12:00:00-14:00"; "minus 14")]
    fn timezoned(d: &str) {
        assert!(matches!(
            d.parse::<XsdDateTime>().unwrap(),
            XsdDateTime::Timezoned(_)
        ));
    }

    #[test_case("2024-09-17T12:00:00"; "no timezone")]
    #[test_case("2024-09-17T12:00:00+00:00"; "plus 0")]
    #[test_case("2024-09-17T12:00:00+01:59"; "plus 1:59")]
    #[test_case("2024-09-17T12:00:00-01:59"; "minus 1:59")]
    #[test_case("2024-09-17T12:00:00+14:00"; "plus 14")]
    #[test_case("2024-09-17T12:00:00-14:00"; "minus 14")]
    #[test_case("2024-09-17T12:00:00.123-14:00"; "with subsec")]
    fn to_string(d: &str) {
        assert_eq!(d.parse::<XsdDateTime>().unwrap().to_string(), d);
    }

    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T12:00:00Z" => true; "12z 12z")]
    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T12:00:00+00:00" => true; "12z 12p0")]
    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T13:00:00+01:00" => true; "12z 13p1")]
    #[test_case("2024-09-17T12:00:00", "2024-09-17T12:00:00Z" => false; "12 12z")]
    #[test_case("2024-09-17T12:00:00", "2024-09-17T13:00:00+01:00" => false; "12 13p1")]
    fn equal(d1: &str, d2: &str) -> bool {
        d1.parse::<XsdDateTime>().unwrap() == d2.parse::<XsdDateTime>().unwrap()
    }

    use Ordering::*;

    #[test_case("2024-09-17T12:00:00", "2024-09-17T12:00:00", Some(Equal); "12 12")]
    #[test_case("2024-09-17T12:00:00", "2024-09-17T12:00:00Z", None; "12 12z")]
    #[test_case("2024-09-17T12:00:00", "2024-09-17T13:00:00+01:00", None; "12 13p1")]
    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T12:00:00Z", Some(Equal); "12z 12z")]
    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T12:00:00+00:00", Some(Equal); "12z 12p0")]
    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T13:00:00+01:00", Some(Equal); "12z 13p1")]
    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T13:00:00+02:00", Some(Greater); "12z 13p2")]
    #[test_case("2024-09-17T12:00:00Z", "2024-09-17T11:00:00-02:00", Some(Less); "12z 11m2")]
    #[test_case("2024-09-17T06:00:00", "2024-09-17T19:59:00Z", None; "6 19z")]
    #[test_case("2024-09-17T06:00:00", "2024-09-17T20:00:00Z", None; "6 20z")]
    #[test_case("2024-09-17T06:00:00", "2024-09-17T20:01:00Z", Some(Less); "6 20:01z")]
    #[test_case("2024-09-17T06:00:00Z", "2024-09-17T19:59:00", None; "6z 19")]
    #[test_case("2024-09-17T06:00:00Z", "2024-09-17T20:00:00", None; "6z 20")]
    #[test_case("2024-09-17T06:00:00Z", "2024-09-17T20:01:00", Some(Less); "6z 20:01")]
    fn partial_cmp(d1: &str, d2: &str, exp: Option<Ordering>) {
        let d1 = d1.parse::<XsdDateTime>().unwrap();
        let d2 = d2.parse::<XsdDateTime>().unwrap();
        assert_eq!(d1.partial_cmp(&d2), exp);
        assert_eq!(d2.partial_cmp(&d1), exp.map(Ordering::reverse));
    }
}
