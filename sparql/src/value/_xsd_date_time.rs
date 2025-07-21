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
use std::{cmp::Ordering, fmt::Display, str::FromStr, sync::LazyLock};

use chrono::{
    DateTime, Datelike, Days, FixedOffset, NaiveDate, NaiveDateTime, SecondsFormat, Timelike,
};
use regex::Regex;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum XsdDateTime {
    Naive(NaiveDateTime),
    Timezoned(DateTime<FixedOffset>),
}

impl XsdDateTime {
    fn new(s: &str) -> Option<Self> {
        static RE: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(r"(?x)
            ^ (-)?(\d{4,}) - (\d{2} - \d{2} T \d{2} : \d{2} : \d{2}) (?:\.(\d+))? ( Z | [-+]\d{2}:\d{2} )? $
        ").unwrap()
        });

        let c = RE.captures(s)?;
        let sign: i32 = c.get(1).map(|_| -1).unwrap_or(1);
        let year: i32 = c.get(2).unwrap().as_str().parse().unwrap();
        let mdhms = c.get(3).unwrap().as_str();
        let month: u32 = mdhms[..2].parse().unwrap();
        let day: u32 = mdhms[3..5].parse().unwrap();
        let hour: u32 = mdhms[6..8].parse().unwrap();
        let minute: u32 = mdhms[9..11].parse().unwrap();
        let second: u32 = mdhms[12..14].parse().unwrap();
        let nano: u32 = c
            .get(4)
            .map(|fraction| {
                let fraction = fraction.as_str();
                if fraction.len() >= 9 {
                    fraction[..9].parse().unwrap()
                } else {
                    fraction.parse::<u32>().unwrap() * 10_u32.pow(9 - fraction.len() as u32)
                }
            })
            .unwrap_or(0);

        let ymd = NaiveDate::from_ymd_opt(sign * year, month, day)?;
        let naive = if (hour, minute, second, nano) != (24, 0, 0, 0) {
            ymd.and_hms_nano_opt(hour, minute, second, nano)?
        } else {
            ymd.and_hms_nano_opt(0, 0, 0, 0)?
                .checked_add_days(Days::new(1))?
        };

        match c.get(5) {
            None => Some(Self::Naive(naive)),
            Some(tz) => {
                let tz = tz.as_str();
                let offset: i32 = if tz == "Z" {
                    0
                } else {
                    let sign = if &tz[..1] == "-" { -1 } else { 1 };
                    let hh: i32 = tz[1..3].parse().unwrap();
                    let mm: i32 = tz[4..6].parse().unwrap();
                    sign * (hh * 3600 + mm * 60)
                };
                let timezoned = naive
                    .and_local_timezone(FixedOffset::east_opt(offset)?)
                    .single()?;
                Some(Self::Timezoned(timezoned))
            }
        }
    }
}

impl FromStr for XsdDateTime {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s).ok_or("Invalid lexical value for xsd:dateTime")
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
                    d.to_rfc3339_opts(SecondsFormat::AutoSi, true)
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

impl Timelike for XsdDateTime {
    fn hour(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.hour(),
            XsdDateTime::Timezoned(date_time) => date_time.hour(),
        }
    }

    fn minute(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.minute(),
            XsdDateTime::Timezoned(date_time) => date_time.minute(),
        }
    }

    fn second(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.second(),
            XsdDateTime::Timezoned(date_time) => date_time.second(),
        }
    }

    fn nanosecond(&self) -> u32 {
        match self {
            XsdDateTime::Naive(naive_date_time) => naive_date_time.nanosecond(),
            XsdDateTime::Timezoned(date_time) => date_time.nanosecond(),
        }
    }

    fn with_hour(&self, hour: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_hour(hour)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_hour(hour)?))
            }
        }
    }

    fn with_minute(&self, min: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_minute(min)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_minute(min)?))
            }
        }
    }

    fn with_second(&self, sec: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_second(sec)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_second(sec)?))
            }
        }
    }

    fn with_nanosecond(&self, nano: u32) -> Option<Self> {
        match self {
            XsdDateTime::Naive(naive_date_time) => {
                Some(XsdDateTime::Naive(naive_date_time.with_nanosecond(nano)?))
            }
            XsdDateTime::Timezoned(date_time) => {
                Some(XsdDateTime::Timezoned(date_time.with_nanosecond(nano)?))
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
    use chrono::Timelike;
    use test_case::test_case;

    #[test_case("2024-09-17T12:34:56" => (2024, 9, 17, 12, 34, 56, 0); "no fraction")]
    #[test_case("2024-09-17T12:34:56.78" => (2024, 9, 17, 12, 34, 56, 780000000); "fraction")]
    #[test_case("2024-09-17T12:34:56.0123456789" => (2024, 9, 17, 12, 34, 56, 12345678); "big fraction")]
    #[test_case("2024-09-17T24:00:00" => (2024, 9, 18, 0, 0, 0, 0); "24:00:00")]
    #[test_case("2024-12-31T24:00:00" => (2025, 1, 1, 0, 0, 0, 0); "24:00:00 new year")]
    #[test_case("-0002-09-17T12:34:56" => (-2, 9, 17, 12, 34, 56, 0); "negative year")]
    #[test_case("12345-09-17T12:34:56" => (12345, 9, 17, 12, 34, 56, 0); "big year")]
    fn naive(d: &str) -> (i32, u32, u32, u32, u32, u32, u32) {
        let got: XsdDateTime = d.parse().unwrap();
        assert!(matches!(got, XsdDateTime::Naive(_)));
        dt2tuple(&got)
    }

    #[test_case("2024-09-17T12:34:56Z" => ((2024, 9, 17, 12, 34, 56, 0), 0); "z no fraction")]
    #[test_case("2024-09-17T12:34:56.78Z" => ((2024, 9, 17, 12, 34, 56, 780000000), 0); "z fraction")]
    #[test_case("2024-09-17T12:34:56.0123456789Z" => ((2024, 9, 17, 12, 34, 56, 12345678), 0); "z big fraction")]
    #[test_case("2024-09-17T24:00:00Z" => ((2024, 9, 18, 0, 0, 0, 0), 0); "z 24:00:00")]
    #[test_case("2024-12-31T24:00:00Z" => ((2025, 1, 1, 0, 0, 0, 0), 0); "z 24:00:00 new year")]
    #[test_case("-0002-09-17T12:34:56Z" => ((-2, 9, 17, 12, 34, 56, 0), 0); "z negative year")]
    #[test_case("12345-09-17T12:34:56Z" => ((12345, 9, 17, 12, 34, 56, 0), 0); "z big year")]
    #[test_case("2024-09-17T12:34:56+00:00" => ((2024, 9, 17, 12, 34, 56, 0), 0); "plus 0 no fraction")]
    #[test_case("2024-09-17T12:34:56.78+00:00" => ((2024, 9, 17, 12, 34, 56, 780000000), 0); "plus 0 fraction")]
    #[test_case("2024-09-17T12:34:56.0123456789+00:00" => ((2024, 9, 17, 12, 34, 56, 12345678), 0); "plus 0 big fraction")]
    #[test_case("2024-09-17T24:00:00+00:00" => ((2024, 9, 18, 0, 0, 0, 0), 0); "plus 0 24:00:00")]
    #[test_case("2024-12-31T24:00:00+00:00" => ((2025, 1, 1, 0, 0, 0, 0), 0); "plus 0 24:00:00 new year")]
    #[test_case("-0002-09-17T12:34:56+00:00" => ((-2, 9, 17, 12, 34, 56, 0), 0); "plus 0 negative year")]
    #[test_case("12345-09-17T12:34:56+00:00" => ((12345, 9, 17, 12, 34, 56, 0), 0); "plus 0 big year")]
    #[test_case("2024-09-17T12:34:56-00:00" => ((2024, 9, 17, 12, 34, 56, 0), 0); "minus 0")]
    #[test_case("2024-09-17T12:34:56+01:00" => ((2024, 9, 17, 12, 34, 56, 0), 60); "plus 1")]
    #[test_case("2024-09-17T12:34:56-01:00" => ((2024, 9, 17, 12, 34, 56, 0), -60); "minus 1")]
    #[test_case("2024-09-17T12:34:56+01:59" => ((2024, 9, 17, 12, 34, 56, 0), 119); "plus 1:59")]
    #[test_case("2024-09-17T12:34:56-01:59" => ((2024, 9, 17, 12, 34, 56, 0), -119); "minus 1:59")]
    #[test_case("2024-09-17T12:34:56+14:00" => ((2024, 9, 17, 12, 34, 56, 0), 60*14); "plus 14")]
    #[test_case("2024-09-17T12:34:56-14:00" => ((2024, 9, 17, 12, 34, 56, 0), -60*14); "minus 14")]
    #[allow(clippy::type_complexity)]
    fn timezoned(d: &str) -> ((i32, u32, u32, u32, u32, u32, u32), i32) {
        let got: XsdDateTime = d.parse().unwrap();
        assert!(matches!(got, XsdDateTime::Timezoned(_)));
        let XsdDateTime::Timezoned(dt) = got else {
            unreachable!();
        };
        (dt2tuple(&dt), dt.timezone().local_minus_utc() / 60)
    }

    #[test_case("2024-09-17T12:34:0"; "too short")]
    #[test_case("2024-09-17T12:34:0Z"; "seconds too short")]
    #[test_case("24-09-17T12:34:56"; "short year")]
    #[test_case("2023-02-29T12:34:56"; "non-existing day")]
    #[test_case("2024-13-17T12:34:56"; "month too large")]
    #[test_case("2024-09-327T12:34:56"; "day too large")]
    #[test_case("2024-09-17T24:34:56"; "hour too large")]
    #[test_case("2024-09-17T12:60:56"; "minute too large")]
    #[test_case("2024-09-17T12:34:60"; "seconds too large")]
    #[test_case("2024-00-17T12:34:56"; "month too small")]
    #[test_case("2024-09-00T12:34:56"; "day too small")]
    #[test_case("2024-09-17:34:56+15:00"; "timezone too big")]
    #[test_case("2024-09-17:34:56+0100"; "timezone badly formatted")]
    fn invalid(d: &str) {
        assert!(d.parse::<XsdDateTime>().is_err())
    }

    #[test_case("2024-09-17T12:34:56"; "no timezone")]
    #[test_case("2024-09-17T12:34:56Z"; "z")]
    #[test_case("2024-09-17T12:34:56+01:00"; "plus 1")]
    #[test_case("2024-09-17T12:34:56+01:59"; "plus 1:59")]
    #[test_case("2024-09-17T12:34:56-01:59"; "minus 1:59")]
    #[test_case("2024-09-17T12:34:56+14:00"; "plus 14")]
    #[test_case("2024-09-17T12:34:56-14:00"; "minus 14")]
    #[test_case("2024-09-17T12:34:56.123-14:00"; "with subsec")]
    fn to_string(d: &str) {
        assert_eq!(d.parse::<XsdDateTime>().unwrap().to_string(), d);
    }

    #[test_case("2024-09-17T12:34:56", "2024-09-17T12:34:56" => true; "12 12")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T12:34:56Z" => true; "12z 12z")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T12:34:56+00:00" => true; "12z 12p0")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T13:34:56+01:00" => true; "12z 13p1")]
    #[test_case("2024-09-17T12:34:56", "2024-09-17T12:34:56Z" => false; "12 12z")]
    #[test_case("2024-09-17T12:34:56", "2024-09-17T13:34:56+01:00" => false; "12 13p1")]
    #[test_case("2024-09-17T24:00:00", "2024-09-18T00:00:00" => true; "24 00")]
    #[test_case("2024-09-17T24:00:00Z", "2024-09-18T00:00:00Z" => true; "24z 00z")]
    #[test_case("2024-09-17T12:34:56.7", "2024-09-17T12:34:56.70" => true; "equivalent fractions")]
    fn equal(d1: &str, d2: &str) -> bool {
        d1.parse::<XsdDateTime>().unwrap() == d2.parse::<XsdDateTime>().unwrap()
    }

    use Ordering::*;

    #[test_case("2024-09-17T12:34:56", "2024-09-17T12:34:56", Some(Equal); "12 12")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T12:34:56Z", Some(Equal); "12z 12z")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T12:34:56+00:00", Some(Equal); "12z 12p0")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T13:34:56+01:00", Some(Equal); "12z 13p1")]
    #[test_case("2024-09-17T12:34:56", "2024-09-17T12:34:56Z", None; "12 12z")]
    #[test_case("2024-09-17T12:34:56", "2024-09-17T13:34:56+01:00", None; "12 13p1")]
    #[test_case("2024-09-17T24:00:00", "2024-09-18T00:00:00", Some(Equal); "24 00")]
    #[test_case("2024-09-17T24:00:00Z", "2024-09-18T00:00:00Z", Some(Equal); "24z 00z")]
    #[test_case("2024-09-17T12:34:56.7", "2024-09-17T12:34:56.70", Some(Equal); "equivalent fractions")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T13:34:56+02:00", Some(Greater); "12z 13p2")]
    #[test_case("2024-09-17T12:34:56Z", "2024-09-17T11:00:00-02:00", Some(Less); "12z 11m2")]
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

    fn dt2tuple<D: Datelike + Timelike>(dt: &D) -> (i32, u32, u32, u32, u32, u32, u32) {
        (
            dt.year(),
            dt.month(),
            dt.day(),
            dt.hour(),
            dt.minute(),
            dt.second(),
            dt.nanosecond(),
        )
    }
}
