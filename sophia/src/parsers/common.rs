//! Reusable types, functions and macros for implementing parsers.

use std;
use std::borrow::Cow;
use std::iter::once;

use pest::{RuleType, iterators::Pair};
use pest::error::{Error as PestError, ErrorVariant};

use ::error::{Error, ErrorKind};
use ::term::Term;

/// This macro provides a straightforward implementation of the default functions
/// of a serializer module.
#[macro_export]
macro_rules! def_default_parser_api {
    ($bufread_parser: ty, $read_parser: ty, $str_parser: ty) => {
        /// Shortcut for `Config::default().parse_bufread(bufread)`
        #[inline]
        pub fn parse_bufread<B: ::std::io::BufRead>(bufread: B) -> $bufread_parser {
            Config::default().parse_bufread(bufread)
        }
        /// Shortcut for `Config::default().parse_read(read)`
        #[inline]
        pub fn parse_read<R: ::std::io::Read>(read: R) -> $read_parser {
            Config::default().parse_read(read)
        }
        /// Shortcut for `Config::default().parse_str(txt)`
        #[inline]
        pub fn parse_str<'a>(txt: &'a str) -> $str_parser {
            Config::default().parse_str(txt)
        }
    };
    ($io_parser: ident, $str_parser: ident) => {
        def_default_parser_api!(
            $io_parser<B>,
            $io_parser<BufReader<R>>,
            $str_parser<'a>
        );
    };
    ($generic_parser: ident) => {
        def_default_parser_api!(
            $generic_parser<B>,
            $generic_parser<BufReader<R>>,
            $generic_parser<BufReader<Cursor<&'a str>>>
        );
    };
}


pub(crate) type CowTerm<'a> = Term<Cow<'a, str>>;

/// Return the unescaped version of `pair.to_str()`,
/// assuming that `pair`'s inner pairs are only ECHAR or UCHAR
/// (as defined in N-Triples, Turtle, SPARQL, etc...).
///
/// `delim` is the size of the delimiter,
/// which will be trimmed off the start and end of the result.
pub(crate) fn unescape_str<'a, R> (pair: Pair<'a,R>, delim: usize) -> Result<Cow<'a, str>, PestError<R>> where
    R: RuleType,
{
    let txt = pair.as_str();
    let mut inner_pairs = pair.clone().into_inner();
    let first_inner = inner_pairs.next();
    if first_inner.is_none() {
        return Ok(Cow::Borrowed(&txt[delim..txt.len()-delim]));
    }
    // else we have escape sequences to unescape
    let mut dst = String::with_capacity(txt.len()-2*delim);
    let span = pair.as_span();
    let offset = -(span.start() as isize);

    // rebuild src containing the original text on which inner pairs are indexed
    let slice = unsafe {
        std::slice::from_raw_parts(txt.as_ptr().offset(offset),
                                   txt.len()+span.start())
    };
    let src = std::str::from_utf8(slice).unwrap();

    let mut i = span.start() + delim;
    for pair in once(first_inner.unwrap()).chain(inner_pairs) {
        let escape_seq = pair.as_str();
        let span = pair.as_span();
        dst.push_str(&src[i..span.start()]);
        dst.push(unescape_char(escape_seq)
            .map_err(|message| PestError::new_from_span(
                ErrorVariant::CustomError{message},
                pair.as_span(),
            ))?
        );
        i = span.end();
    }
    dst.push_str(&src[i..(src.len()-delim)]);
    Ok(Cow::Owned(dst))
}

/// Transform the match of ECHAR or UCHAR into the corresponding character.
pub(crate) fn unescape_char(txt: &str) -> Result<char, String> {
    let bytes = txt.as_bytes();
    debug_assert_eq!(bytes[0] as char, '\\');
    match bytes[1] as char {
        // ECHAR
        't'  => Ok('\t'),
        'b'  => Ok('\x08'),
        'n'  => Ok('\n'),
        'r'  => Ok('\r'),
        'f'  => Ok('\x0c'),
        '"'  => Ok('"'),
        '\\' => Ok('\\'),
        '\'' => Ok('\''),

        // UCHAR
        'u' | 'U'  => {
            match u32::from_str_radix(&txt[2..], 16) {
                Ok(code) => {
                    std::char::from_u32(code).ok_or_else(||
                        format!("Invalid codepoint {:x}", code)
                    )
                },
                Err(err) => Err(format!("{}", err)),
            }
        }

        // other ??
        _    => {
            Err(format!("Invalid ECHAR: {}", txt))
        }
    }
}

/// Utility function for converting [Pest] errors into [Sophia errors](../../error/index.html).
/// 
/// [Pest]: https://docs.rs/crate/pest/
/// 
pub fn convert_pest_err<R: pest::RuleType> (err: PestError<R>, lineoffset: usize) -> Error {
    let message = match err.variant {
        ErrorVariant::ParsingError{positives, negatives} => {
            format!("expected: {:?}\nunexpected: {:?}", positives, negatives)
        }
        ErrorVariant::CustomError{message} => message
    };
    let location = err.location.clone();
    use ::pest::error::LineColLocation::*;
    let line_col = match err.line_col.clone() {
        Pos((l, c)) =>
            Pos((l+lineoffset, c)),
        Span((l1, c1), (l2, c2)) =>
            Span((l1+lineoffset, c1), (l2+lineoffset, c2)),
    };
    ErrorKind::Parsing(message, location, line_col).into()
}


// ---------------------------------------------------------------------------------
//                               utility test function
// ---------------------------------------------------------------------------------

#[cfg(test)]
use pest::iterators::Pairs;

#[cfg(test)]
pub(crate) fn test_rule<P, R, T> (parse: &P, rule: R, values: &[T]) where
    P: Fn(R, &str) -> Result<Pairs<R>, PestError<R>>,
    R: RuleType,
    T: std::borrow::Borrow<str>,
{
    for val in values.iter() {
        let val = val.borrow();
        match parse(rule, val) {
            Ok(pairs) => {
                let v: Vec<_> = pairs.collect();
                assert_eq!(v.len(), 1, "expected exactly 1 match");
                let span = v[0].as_span();
                assert_eq!(span.start(), 0, "expected match at start");
                assert_eq!(span.end(), val.len(), "expected total match");
            }
            Err(err) => {
                assert!(false, format!("{:?} -> {}", val, err))
            }
        }
    }
}

#[cfg(test)]
pub(crate) fn test_rule_partial<P, R, T> (parse: &P, rule: R, values: &[(T, usize)]) where
    P: Fn(R, &str) -> Result<Pairs<R>, PestError<R>>,
    R: RuleType,
    T: std::borrow::Borrow<str>,
{
    for (val, match_length) in values.iter() {
        let val = val.borrow();
        match parse(rule, val) {
            Ok(pairs) => {
                let v: Vec<_> = pairs.collect();
                assert_eq!(v.len(), 1, "expected exactly 1 match");
                let span = v[0].as_span();
                assert_eq!(span.start(), 0, "expected match at start");
                assert_eq!(span.end(), *match_length, "expected match of given length");
            }
            Err(err) => {
                assert!(false, format!("{:?} -> {}", val, err))
            }
        }
    }
}

#[cfg(test)]
pub(crate) fn test_rule_negative<P, R, T> (parse: &P, rule: R, values: &[T]) where
    P: Fn(R, &str) -> Result<Pairs<R>, PestError<R>>,
    R: RuleType,
    T: std::borrow::Borrow<str>,
{
    for val in values.iter() {
        let val = val.borrow();
        match parse(rule, val) {
            Ok(pairs) => {
                let v: Vec<_> = pairs.collect();
                if v.len() != 1 { continue }
                let span = v[0].as_span();
                if span.start() != 0 || span.end() != val.len() { continue }
                assert!(false, format!("unexpected match {:?}", val))
            }
            Err(_) => {} // pass
        }
    }
}


// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn unescape_char_() {
        // ECHAR
        assert_eq!(unescape_char(r"\t"), Ok('\t'));
        assert_eq!(unescape_char(r"\b"), Ok('\x08'));
        assert_eq!(unescape_char(r"\n"), Ok('\n'));
        assert_eq!(unescape_char(r"\r"), Ok('\r'));
        assert_eq!(unescape_char(r"\f"), Ok('\x0c'));
        assert_eq!(unescape_char(r"\'"), Ok('\''));
        assert_eq!(unescape_char(r"\\"), Ok('\\'));
        assert_eq!(unescape_char(r#"\""#), Ok('"'));
        // pseudo-ECHAR with wront character
        assert!(unescape_char(r"\x").is_err());
        assert!(unescape_char(r"\u").is_err());
        assert!(unescape_char(r"\U").is_err());

        // UCHAR
        assert_eq!(unescape_char(r"\u1234"), Ok('\u{1234}'));
        assert_eq!(unescape_char(r"\uabCD"), Ok('\u{ABCD}'));
        assert_eq!(unescape_char(r"\U0010abCD"), Ok('\u{10ABCD}'));
        // UCHAR with invalid codepoint
        assert!(unescape_char(r"\UFFFFFFFF").is_err());
        // pseudo-UCHAR with non hex-digit
        assert!(unescape_char(r"\u123x").is_err());
        assert!(unescape_char(r"\U1234567x").is_err());
    }

    #[test]
    fn unescape_str_() {
        use pest::Parser;
        use super::super::nt::{PestNtParser,Rule};

        fn test<'a> (txt: &'a str) -> Result<String, String> {
            // parsing a triple just to test that unescape_str works with an offset > 0.
            let triple = format!("<> <> {}.", txt);
            let mut pairs = PestNtParser::parse(Rule::triple, &triple[..]).unwrap();
            let pairs = pairs.next().unwrap().into_inner(); // into 'triple'
            let object = pairs.skip(2).next().unwrap();
            let pair =
                if object.as_rule() == Rule::literal {
                    object.into_inner().next().unwrap() // into 'literal'
                } else {
                    object
                };
            unescape_str(pair, 1)
            .map(|cow| cow.into_owned())
            .map_err(|err| format!("{:?}", err))
        }

        assert_eq!(test(&r#""hello world""#).unwrap(),
                            "hello world");
        assert_eq!(test(&r#""hello\nworld""#).unwrap(),
                            "hello\nworld");
        assert_eq!(test(&r#""hell\u006f\nworld""#).unwrap(),
                            "hello\nworld");
        assert_eq!(test(&r#""hell\u006f\nw\U0000006Frld""#).unwrap(),
                            "hello\nworld");

        assert!(test(r#""hello\UFFFFFFFFworld""#).is_err());

        assert_eq!(test(&r"<hello>").unwrap(),
                           "hello");
        assert_eq!(test(&r"<hell\u006F>").unwrap(),
                           "hello");
        assert_eq!(test(&r"<he\U0000006cl\u006F>").unwrap(),
                           "hello");
        assert_eq!(test(&r"<hel\U0000006c\u006F>").unwrap(),
                           "hello");

        assert!(test(r"<hello\UFFFFFFFFworld>").is_err());
    }
}
