//! I define [`GenericSource`], [`NxSource`], and [`TxSource`],
//! which implement all the parsing logic common to multiple syntaxes of the Turtle-family.

use crate::parser::_error::ErrorKind;

mod _extra;
pub(crate) use _extra::*;
mod _inner;
pub(crate) use _inner::*;
mod _state;
pub(crate) use _state::*;

mod _generic_source;
pub(crate) use _generic_source::*;
mod _nx_source;
pub(crate) use _nx_source::*;
mod _tx_source;
pub(crate) use _tx_source::*;

fn unescape_iri(
    txt: &str,
    pos: &mut usize,
    buf: &mut String,
    offset: usize,
) -> Result<(), ErrorKind> {
    let (chr, len) = unescape_numeric(txt)?;
    buf.push(chr);
    *pos = offset + len;
    Ok(())
}

fn unescape_literal(
    txt: &str,
    pos: &mut usize,
    buf: &mut String,
    offset: usize,
) -> Result<(), ErrorKind> {
    let (chr, len) = match txt.as_bytes()[0] {
        b'b' => ('\x08', 1),
        b'f' => ('\x0C', 1),
        b'n' => ('\n', 1),
        b'r' => ('\r', 1),
        b't' => ('\t', 1),
        b'"' => ('"', 1),
        b'\'' => ('\'', 1),
        b'\\' => ('\\', 1),
        _ => unescape_numeric(txt)?,
    };
    buf.push(chr);
    *pos = offset + len;
    Ok(())
}

fn unescape_numeric(txt: &str) -> Result<(char, usize), ErrorKind> {
    const ERR: ErrorKind = ErrorKind::InvalidEscape;
    match txt.as_bytes()[0] {
        b'u' if txt.len() > 4 => Ok((
            char::try_from(u32::from_str_radix(&txt[1..=4], 16).map_err(|_| ERR)?)
                .map_err(|_| ERR)?,
            5,
        )),
        b'U' if txt.len() > 8 => Ok((
            char::try_from(u32::from_str_radix(&txt[1..=8], 16).map_err(|_| ERR)?)
                .map_err(|_| ERR)?,
            9,
        )),
        _ => Err(ERR),
    }
}
