//! RDF literals like specified in
//! [RDF](https://www.w3.org/TR/rdf11-primer/#section-literal).
//!

use crate::ns::{rdf, xsd};
use crate::term::iri::Normalization;
use crate::term::{Iri, Result, Term, TermData, TermError};
use language_tag::LangTag;
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io;

mod _convert;
pub use self::_convert::*;

/// An RDF literal.
///
/// Each literals has a lexical value, i.e. a text, and a datatype.
#[derive(Clone, Copy, Debug, Eq)]
pub enum Literal<TD: TermData> {
    /// Simple literals are of type `xsd:string` and have shorthands in many
    /// serialization formats.
    Simple(TD),
    /// Language-tagged literals have the type `rdf:langString` and an
    /// additional language-tag.
    ///
    /// The tags conform to [BCP47](https://tools.ietf.org/html/bcp47).
    Lang {
        /// The text of the tagged literal.
        txt: TD,
        /// The language tag.
        tag: TD,
    },
    /// Typed literals have a dedicated datatype.
    ///
    /// Datatypes in RDF have a lexical scope and a value scope. Transformation
    /// between them is done by the lexical-to-value mapping of a datatype. If
    /// the text of a typed literal is not in the value space of its datatype,
    /// the literal is called ill-typed or malformed. However,
    /// [RDF specification](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-Graph-Literal)
    /// explicitly requires implementations to accept those literals. In the
    /// end such malformed literals lead to logical inconsistency in a graph.
    Typed {
        /// The text of the typed literal.
        txt: TD,
        /// The IRI referencing the datatype.
        dt: Iri<TD>,
    },
}

use self::Literal::*;

impl<TD> Literal<TD>
where
    TD: TermData,
{
    /// Return a new literal with type `xsd:string`.
    pub fn new<U>(txt: U) -> Self
    where
        TD: From<U>,
    {
        Self::Simple(txt.into())
    }

    /// Return a new language-tagged literal.
    ///
    /// # Error
    ///
    /// If `tag` is not a valid language-tag according to
    /// [BCP47](https://tools.ietf.org/html/bcp47) an error is raised.
    pub fn new_lang<U, V>(txt: U, tag: V) -> Result<Self>
    where
        V: AsRef<str>,
        TD: From<U> + From<V>,
    {
        if let Err(err) = tag.as_ref().parse::<LangTag>() {
            return Err(TermError::InvalidLanguageTag {
                tag: tag.as_ref().to_string(),
                err,
            });
        }

        Ok(Self::Lang {
            txt: txt.into(),
            tag: tag.into(),
        })
    }

    /// Return a new typed literal.
    ///
    /// Neither is checked if `dt` refers to a datatype nor if `txt` is
    /// ill-type and not in the lexical space of `dt`. This is intended as the
    /// [RDF specification](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-Graph-Literal)
    /// requires implementations to accept ill-typed literals.
    pub fn new_dt<U, V>(txt: U, dt: V) -> Self
    where
        TD: From<U>,
        Iri<TD>: From<V>,
    {
        let dt = dt.into();

        if xsd::string == dt {
            Self::Simple(txt.into())
        } else {
            Self::Typed {
                txt: txt.into(),
                dt,
            }
        }
    }

    /// Return a new language-tagged literal.
    ///
    /// # Pre-condition
    ///
    /// `tag` must be a valid language-tag according to
    /// [BCP47](https://tools.ietf.org/html/bcp47).
    /// In debug mode this is asserted.
    pub fn new_lang_unchecked<U, V>(txt: U, tag: V) -> Self
    where
        V: AsRef<str>,
        TD: From<U> + From<V>,
    {
        debug_assert!(tag.as_ref().parse::<LangTag>().is_ok());

        Self::Lang {
            txt: txt.into(),
            tag: tag.into(),
        }
    }

    /// Clone self while transforming the inner `TermData` with the given
    /// factory.
    ///
    /// Clone as this might allocate new `TermData`. However there is also
    /// `TermData` that is cheap to clone, i.e. `Copy`.
    pub fn clone_with<'a, U, F>(&'a self, mut factory: F) -> Literal<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        match self {
            Simple(txt) => Simple(factory(txt.as_ref())),
            Lang { txt, tag } => Lang {
                txt: factory(txt.as_ref()),
                tag: factory(tag.as_ref()),
            },
            Typed { txt, dt } => Typed {
                txt: factory(txt.as_ref()),
                dt: dt.clone_with(factory),
            },
        }
    }

    /// If the literal is typed transform the IRI according to the given
    /// policy.
    ///
    /// If the policy already applies or it is language tagged the literal is
    /// returned unchanged.
    pub fn clone_normalized_with<F, U>(&self, policy: Normalization, factory: F) -> Literal<U>
    where
        F: FnMut(&str) -> U,
        U: TermData,
    {
        let mut factory = factory;
        match self {
            Typed { txt, dt } => Typed {
                txt: factory(txt.as_ref()),
                dt: dt.clone_normalized_with(policy, factory),
            },
            lit => lit.clone_with(factory),
        }
    }

    /// Writes the IRI to the `fmt::Write` using the NTriples syntax.
    ///
    /// This means the IRI is in angled brackets and no prefix is used.
    pub fn write_fmt<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        match self {
            Simple(txt) => {
                w.write_char('"')?;
                fmt_quoted_string(w, txt.as_ref())?;
                w.write_char('"')
            }
            Lang { txt, tag } => {
                w.write_char('"')?;
                fmt_quoted_string(w, txt.as_ref())?;
                w.write_str("\"@")?;
                w.write_str(tag.as_ref())
            }
            Typed { txt, dt } => {
                w.write_char('"')?;
                fmt_quoted_string(w, txt.as_ref())?;
                if &xsd::string != dt {
                    w.write_str("\"^^")?;
                    dt.write_fmt(w)
                } else {
                    w.write_char('"')
                }
            }
        }
    }

    /// Writes the blank node to the `io::Write` using the N3 syntax.
    pub fn write_io<W>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self {
            Simple(txt) => {
                w.write_all(b"\"")?;
                io_quoted_string(w, txt.as_ref().as_bytes())?;
                w.write_all(b"\"")
            }
            Lang { txt, tag } => {
                w.write_all(b"\"")?;
                io_quoted_string(w, txt.as_ref().as_bytes())?;
                w.write_all(b"\"@")?;
                w.write_all(tag.as_ref().as_bytes())
            }
            Typed { txt, dt } => {
                w.write_all(b"\"")?;
                io_quoted_string(w, txt.as_ref().as_bytes())?;
                if &xsd::string != dt {
                    w.write_all(b"\"^^")?;
                    dt.write_io(w)
                } else {
                    w.write_all(b"\"")
                }
            }
        }
    }

    /// Return a copy of the literal's lexical value.
    pub fn value(&self) -> String {
        self.txt().as_ref().to_string()
    }

    /// Returns the literal's lexical value.
    pub fn txt(&self) -> &TD {
        match self {
            Simple(txt) | Lang { txt, .. } | Typed { txt, .. } => txt,
        }
    }

    /// Return a copy of the IRI of the literals datatype.
    ///
    /// _Note:_ A language-tagged literal has always the type `rdf:langString`.
    pub fn dt<'a>(&self) -> Iri<TD>
    where
        TD: From<&'a str>,
    {
        match self {
            Simple(_) => Iri::try_from(&xsd::string).expect("ensured"),
            Lang { .. } => Iri::try_from(&rdf::langString).expect("ensured"),
            Typed { dt, .. } => dt.clone(),
        }
    }

    /// Return the language-tag of the literal if it has one.
    pub fn lang(&self) -> Option<&TD> {
        if let Lang { tag, .. } = self {
            Some(tag)
        } else {
            None
        }
    }

    /// Check if the datatype IRI is absolute.
    pub fn is_absolute(&self) -> bool {
        if let Literal::Typed { dt, .. } = self {
            dt.is_absolute()
        } else {
            // other datatypes `xsd:string` and `rdf:langString` are absolute IRIs
            true
        }
    }

    /// Check if both literals have the same lexical value.
    pub fn eq_txt<U>(&self, other: Literal<U>) -> bool
    where
        U: TermData,
    {
        self.txt().as_ref() == other.txt().as_ref()
    }
}

impl<TD> fmt::Display for Literal<TD>
where
    TD: TermData,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_fmt(f)
    }
}

impl<TD> Hash for Literal<TD>
where
    TD: TermData,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Simple(txt) => {
                // Same hash as Typed { .., dt: xsd::string }
                state.write(txt.as_ref().as_bytes());
                xsd::string.hash(state);
            }
            Lang { txt, tag } => {
                state.write(txt.as_ref().as_bytes());
                tag.as_ref().to_ascii_lowercase().hash(state);
            }
            Typed { txt, dt } => {
                state.write(txt.as_ref().as_bytes());
                dt.hash(state);
            }
        };
    }
}

impl<'a, T, U> From<&'a Literal<U>> for Literal<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    fn from(other: &'a Literal<U>) -> Self {
        other.clone_with(T::from)
    }
}

impl<TD> TryFrom<Term<TD>> for Literal<TD>
where
    TD: TermData,
{
    type Error = TermError;

    fn try_from(term: Term<TD>) -> Result<Self, Self::Error> {
        match term {
            Term::Literal(lit) => Ok(lit),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "literal".to_owned(),
            }),
        }
    }
}

impl<'a, T, U> TryFrom<&'a Term<U>> for Literal<T>
where
    T: TermData + From<&'a str>,
    U: TermData,
{
    type Error = TermError;

    fn try_from(term: &'a Term<U>) -> Result<Self, Self::Error> {
        match term {
            Term::Literal(lit) => Ok(lit.into()),
            _ => Err(TermError::UnexpectedKindOfTerm {
                term: term.to_string(),
                expect: "literal".to_owned(),
            }),
        }
    }
}

impl<T, U> PartialEq<Literal<U>> for Literal<T>
where
    T: TermData,
    U: TermData,
{
    fn eq(&self, other: &Literal<U>) -> bool {
        match (self, other) {
            (Simple(st), Simple(ot)) => st.as_ref() == ot.as_ref(),
            (Lang { txt: st, tag: stag }, Lang { txt: ot, tag: otag }) => {
                st.as_ref() == ot.as_ref() && stag.as_ref().eq_ignore_ascii_case(otag.as_ref())
            }
            (Typed { txt: st, dt: sdt }, Typed { txt: ot, dt: odt }) => {
                st.as_ref() == ot.as_ref() && sdt == odt
            }
            (Simple(st), Typed { txt: ot, dt }) if dt == &xsd::string => st.as_ref() == ot.as_ref(),
            (Typed { txt: st, dt }, Simple(ot)) if dt == &xsd::string => st.as_ref() == ot.as_ref(),
            _ => false,
        }
    }
}

impl<TD> PartialEq<str> for Literal<TD>
where
    TD: TermData,
{
    fn eq(&self, other: &str) -> bool {
        match self {
            Simple(txt) => txt.as_ref() == other,
            _ => false,
        }
    }
}

fn fmt_quoted_string<W: fmt::Write>(w: &mut W, txt: &str) -> fmt::Result {
    let mut cut = txt.len();
    let mut cutchar = '\0';
    for (pos, chr) in txt.char_indices() {
        if chr <= '\\' && (chr == '\n' || chr == '\r' || chr == '\\' || chr == '"') {
            cut = pos;
            cutchar = chr;
            break;
        }
    }
    w.write_str(&txt[..cut])?;
    if cut < txt.len() {
        match cutchar {
            '\n' => {
                w.write_str("\\n")?;
            }
            '\r' => {
                w.write_str("\\r")?;
            }
            '"' => {
                w.write_str("\\\"")?;
            }
            '\\' => {
                w.write_str("\\\\")?;
            }
            _ => unreachable!(),
        }
    };
    if cut + 1 >= txt.len() {
        Ok(())
    } else {
        fmt_quoted_string(w, &txt[cut + 1..])
    }
}

fn io_quoted_string<W: io::Write>(w: &mut W, txt: &[u8]) -> io::Result<()> {
    let mut cut = txt.len();
    let mut cutchar = b'\0';
    for (pos, chr) in txt.iter().enumerate() {
        let chr = *chr;
        if chr <= b'\\' && (chr == b'\n' || chr == b'\r' || chr == b'\\' || chr == b'"') {
            cut = pos;
            cutchar = chr;
            break;
        }
    }
    w.write_all(&txt[..cut])?;
    if cut < txt.len() {
        match cutchar {
            b'\n' => {
                w.write_all(b"\\n")?;
            }
            b'\r' => {
                w.write_all(b"\\r")?;
            }
            b'"' => {
                w.write_all(b"\\\"")?;
            }
            b'\\' => {
                w.write_all(b"\\\\")?;
            }
            _ => unreachable!(),
        }
    };
    if cut + 1 >= txt.len() {
        Ok(())
    } else {
        io_quoted_string(w, &txt[cut + 1..])
    }
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially the ::term::test module).
}