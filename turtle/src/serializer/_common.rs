//! Common functions and statics for T-syntaxes
use std::io::{self, Write};

use sophia_api::{
    MownStr,
    ns::xsd,
    prefix::{PrefixMap, PrefixMapPair},
    term::{BaseDirection, IriRef, LanguageTag},
};
use sophia_iri::Iri;

use crate::lazy_regex;

pub fn write_iri<W: io::Write>(
    iri: &IriRef<MownStr>,
    w: &mut W,
    prefix_map: &[PrefixMapPair],
) -> io::Result<()> {
    let Some(iri) = Iri::new(iri.as_str()).ok() else {
        return write!(w, "<{}>", iri.as_str());
    };
    match prefix_map.get_checked_prefixed_pair(iri, |txt| PN_LOCAL.is_match(txt)) {
        Some((pre, suf)) => {
            write!(w, "{}:{}", pre.as_str(), suf)
        }
        None => {
            write!(w, "<{}>", iri.as_str())
        }
    }
}

pub fn write_typed_literal<W: io::Write>(
    lex: &MownStr,
    dt: &IriRef<MownStr>,
    w: &mut W,
    prefix_map: &[PrefixMapPair],
) -> io::Result<()> {
    if dt.starts_with(xsd::PREFIX.as_str()) {
        let suffix = &dt[xsd::PREFIX.len()..];
        if suffix == "integer" && INTEGER.is_match(lex)
            || suffix == "decimal" && DECIMAL.is_match(lex)
            || suffix == "double" && DOUBLE.is_match(lex)
            || suffix == "boolean" && BOOLEAN.is_match(lex)
        {
            return w.write_all(lex.as_bytes());
        }
        if suffix == "string" {
            w.write_all(b"\"")?;
            quoted_string(w, lex)?;
            return w.write_all(b"\"");
        }
    }
    w.write_all(b"\"")?;
    quoted_string(w, lex)?;
    w.write_all(b"\"^^")?;
    write_iri(dt, w, prefix_map)
}

pub fn write_language_string<W: io::Write>(
    lex: &MownStr,
    tag: &LanguageTag<MownStr>,
    dir: Option<BaseDirection>,
    w: &mut W,
) -> io::Result<()> {
    w.write_all(b"\"")?;
    quoted_string(w, lex)?;
    w.write_all(b"\"@")?;
    w.write_all(tag.as_bytes())?;
    if let Some(dir) = dir {
        write!(w, "--{dir}")?;
    }
    Ok(())
}

pub fn quoted_string<W: std::io::Write>(w: &mut W, txt: &str) -> std::io::Result<()> {
    lazy_regex!(ESCAPED = r#"[\u0000-\u001f\u007f"\\\ufffe\uffff]"#);

    if let Some(m) = ESCAPED.find(txt) {
        let head = unsafe {
            // SAFETY m is a match inside txt
            txt.as_bytes().get_unchecked(..m.start())
        };
        w.write_all(head)?;
        let first = unsafe {
            // SAFETY m is a match inside txt
            txt.as_bytes().get_unchecked(m.start())
        };
        let mut escape_buf = [0_u8; 6];
        let escaped = match first {
            0x08..=0x0D => {
                let offset = (*first - 0x08) as usize;
                ["\\b", "\\t", "\\n", "\\u000B", "\\f", "\\r"][offset].as_bytes()
            }
            b'\\' => b"\\\\",
            b'"' => b"\\\"",
            0xef => {
                debug_assert!(matches!(m.as_str(), "\u{fffe}" | "\u{ffff}"));
                let last = unsafe {
                    // SAFETY m is a match inside txt
                    *txt.as_bytes().get_unchecked(m.end() - 1)
                };
                if last == 0xbe { b"\\uFFFE" } else { b"\\uFFFF" }
            }
            _ => {
                debug_assert!(matches!(first, 0..=0x07 | 0x0e..=0x1f | 0x7f));
                write!(&mut escape_buf[..], "\\u00{first:02X}")?;
                &escape_buf[..]
            }
        };
        w.write_all(escaped)?;
        let tail = unsafe {
            // SAFETY m is a match inside txt
            &txt.get_unchecked(m.end()..)
        };
        quoted_string(w, tail)
    } else {
        w.write_all(txt.as_bytes())
    }
}

lazy_regex!(
    pub(crate) PN_LOCAL = r"(?x)^
    #(PN_CHARS_U | ':' | [0-9] | PLX)
    (
        [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_:0-9]
        # | PLX
        | \\ [_~.!$&'()*+,;=/?\#@%-]
        | % [0-9A-Fa-f]{2}
    )
    # ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
    (
        (
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}.:-]
            | \\ [_~.!$&'()*+,;=/?\#@%-]
            | % [0-9A-Fa-f]{2}
        )*
        (
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}:-]
            | \\ [_~.!$&'()*+,;=/?\#@%-]
            | % [0-9A-Fa-f]{2}
        )
    )?
$"
);
lazy_regex!(pub(crate) INTEGER = r"^[+-]?[0-9]+$");
lazy_regex!(pub(crate) DECIMAL = r"^[+-]?[0-9]*\.[0-9]+$");
lazy_regex!(
    pub(crate) DOUBLE = r"(?x)^
  [+-]? ( [0-9]+ ( \. [0-9]* )? | \. [0-9]+ ) [eE] [+-]? [0-9]+
$"
);
lazy_regex!(pub(crate) BOOLEAN = r"^(true|false)$");

// ---------------------------------------------------------------------------------
//                                      tests
// ---------------------------------------------------------------------------------

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn pn_local() {
        for positive in [
            "a",
            "aBc",
            "éàïsophia_api::graph::",
            ":::",
            "123",
            "%20%21%22",
            "\\%\\?\\&",
        ] {
            assert!(PN_LOCAL.is_match(positive), "{}", positive);
        }
        for negative in [" ", ".a", "a."] {
            assert!(!PN_LOCAL.is_match(negative), "{}", negative);
        }
    }

    #[test]
    fn double() {
        for positive in [
            "3.14e0",
            "+3.14e0",
            "-3.14e0",
            "3.14e+0",
            "3.14e-0",
            "0000e0000",
            ".1E0",
            "1.e+3",
            "1E-3",
        ] {
            assert!(DOUBLE.is_match(positive), "{}", positive);
        }
    }
}
