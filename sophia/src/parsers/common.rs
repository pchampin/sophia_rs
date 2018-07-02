use std;

use pest::{Error, RuleType, iterators::Pair};

use ::term::Term;

#[derive(Clone, Debug)]
pub enum BooStr<'a> {
    B(&'a str),
    O(Box<str>),
}

impl<'a> std::borrow::Borrow<str> for BooStr<'a> {
    fn borrow(&self) -> &str {
        match self {
            BooStr::B(borrowed) => borrowed,
            BooStr::O(owned) => &owned[..],
        }
    }
}

impl<'a> Into<Box<str>> for BooStr<'a> {
    fn into(self) -> Box<str> {
        match self {
            BooStr::B(borrowed) => Box::from(borrowed),
            BooStr::O(owned) => owned,
        }
    }
}

impl<'a, 'b:'a> From<&'b str> for BooStr<'a> {
    fn from(other: &'b str) -> BooStr<'a> {
        BooStr::B(other)
    }
}

pub(crate) type BooTerm<'a> = Term<BooStr<'a>>;


/// Return the unescaped version of `pair.to_str()`,
/// assuming that `pair`'s inner pairs are only ECHAR or UCHAR
/// (as defined in N-Triples, Turtle, SPARQL, etc...).
///
/// `delim` is the size of the delimiter,
/// which will be trimmed off the start and end of the result.
pub(crate) fn unescape_str<'a, R> (pair: Pair<'a,R>, delim: usize) -> Result<BooStr<'a>, Error<'a,R>> where
    R: RuleType,
{
    let txt = pair.as_str();
    let inner:Vec<_> = pair.clone().into_inner().collect();
    if inner.len() == 0 {
        return Ok(BooStr::B(&txt[delim..txt.len()-delim]));
    }
    // else we have escape sequences to unescape
    let mut dst = String::new();
    let span = pair.clone().into_span();
    let offset = -(span.start() as isize);

    let slice = unsafe {
        std::slice::from_raw_parts(txt.as_ptr().offset(offset),
                                   txt.len()+span.start())
    };
    let src = std::str::from_utf8(slice).unwrap();

    let mut i = span.start();
    for pair in inner.into_iter() {
        let escape_seq = pair.as_str();
        let span = pair.clone().into_span();
        dst.push_str(&src[i..span.start()]);
        dst.push(unescape_char(escape_seq)
            .map_err(|message| Error::CustomErrorSpan{
                message, span: pair.into_span(),
            })?
        );
        i = span.end();
    }
    dst.push_str(&src[i..]);
    let len = dst.len();
    Ok(BooStr::O(dst[delim..(len - delim)].into()))
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



// ---------------------------------------------------------------------------------
//                               test utility function
// ---------------------------------------------------------------------------------

#[cfg(test)]
use pest::iterators::Pairs;

#[cfg(test)]
pub(crate) fn test_rule<P, R, T> (parse: &P, rule: R, values: &[T]) where
    P: Fn(R, &str) -> Result<Pairs<R>, Error<R>>,
    R: RuleType,
    T: std::borrow::Borrow<str>,
{
    for val in values.iter() {
        let val = val.borrow();
        match parse(rule, val) {
            Ok(pairs) => {
                let v: Vec<_> = pairs.collect();
                assert_eq!(v.len(), 1, "expected exactly 1 match");
                let span = v[0].clone().into_span();
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
    P: Fn(R, &str) -> Result<Pairs<R>, Error<R>>,
    R: RuleType,
    T: std::borrow::Borrow<str>,
{
    for (val, match_length) in values.iter() {
        let val = val.borrow();
        match parse(rule, val) {
            Ok(pairs) => {
                let v: Vec<_> = pairs.collect();
                assert_eq!(v.len(), 1, "expected exactly 1 match");
                let span = v[0].clone().into_span();
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
    P: Fn(R, &str) -> Result<Pairs<R>, Error<R>>,
    R: RuleType,
    T: std::borrow::Borrow<str>,
{
    for val in values.iter() {
        let val = val.borrow();
        match parse(rule, val) {
            Ok(pairs) => {
                let v: Vec<_> = pairs.collect();
                if v.len() != 1 { continue }
                let span = v[0].clone().into_span();
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
        use super::super::nt::{NtParser,Rule};

        fn test<'a> (txt: &'a str) -> Result<Box<str>, String> {
            // parsing a triple just to test that unescape_str works with an offset > 0.
            let triple = format!("<> <> {}.", txt);
            let mut pairs = NtParser::parse(Rule::triple, &triple[..]).unwrap();
            let pairs = pairs.next().unwrap().into_inner(); // into 'triple'
            let object = pairs.skip(2).next().unwrap();
            let pair =
                if object.as_rule() == Rule::literal {
                    object.into_inner().next().unwrap() // into 'literal'
                } else {
                    object
                };
            unescape_str(pair, 1)
            .map(|ref_or_owned| ref_or_owned.into())
            .map_err(|err| format!("{:?}", err))
        }

        assert_eq!(test(r#""hello world""#).unwrap().as_ref(),
                          "hello world");
        assert_eq!(test(r#""hello\nworld""#).unwrap().as_ref(),
                          "hello\nworld");
        assert_eq!(test(r#""hell\u006f\nworld""#).unwrap().as_ref(),
                          "hello\nworld");
        assert_eq!(test(r#""hell\u006f\nw\U0000006Frld""#).unwrap().as_ref(),
                          "hello\nworld");

        assert!(test(r#""hello\UFFFFFFFFworld""#).is_err());

        assert_eq!(test(r"<hello>").unwrap().as_ref(),
                         "hello");
        assert_eq!(test(r"<hell\u006F>").unwrap().as_ref(),
                         "hello");
        assert_eq!(test(r"<he\U0000006cl\u006F>").unwrap().as_ref(),
                         "hello");
        assert_eq!(test(r"<hel\U0000006c\u006F>").unwrap().as_ref(),
                         "hello");

        assert!(test(r"<hello\UFFFFFFFFworld>").is_err());
    }
}
