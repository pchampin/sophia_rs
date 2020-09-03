//! RDF literals like specified in
//! [RDF](https://www.w3.org/TR/rdf11-primer/#section-literal).
//!

use crate::iri::Normalization;
use crate::literal::convert::{DataType, NativeLiteral};
use crate::*;
use mownstr::MownStr;
use oxilangtag::LanguageTag;
use sophia_api::ns::{rdf, xsd};
use std::convert::TryFrom;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io;

pub mod convert;

/// Internal distinction of literals.
///
/// Opaque to users.
#[derive(Clone, Copy, Debug)]
enum Kind<TD: TermData> {
    /// Something representing a language tag.
    ///
    /// The tags conform to [BCP47](https://tools.ietf.org/html/bcp47).
    Lang(TD),
    /// The IRI referencing the datatype.
    Dt(Iri<TD>),
}

use self::Kind::*;

/// An RDF literal.
///
/// Each literals has a lexical value, i.e. a text, and a datatype.
///
/// # Language tagged literals
///
/// Language-tagged literals have the type `rdf:langString` and an additional
/// language-tag.
///
/// The tags conform to [BCP47](https://tools.ietf.org/html/bcp47).
///
/// # Datatypes
///
/// Datatypes in RDF have a lexical scope and a value scope. Transformation
/// between them is done by the lexical-to-value mapping of a datatype. If the
/// text of a typed literal is not in the value space of its datatype, literal
/// is called ill-typed or malformed. However,
/// [RDF specification](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-Graph-Literal)
/// explicitly requires implementations to accept those literals. In the end
/// such malformed literals lead to logical inconsistency in a graph.
#[derive(Clone, Copy, Debug)]
pub struct Literal<TD: TermData> {
    txt: TD,
    kind: Kind<TD>,
}

impl<TD> Literal<TD>
where
    TD: TermData,
{
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
        if let Err(err) = LanguageTag::parse(tag.as_ref()) {
            return Err(TermError::InvalidLanguageTag {
                tag: tag.as_ref().to_string(),
                err: err.to_string(),
            });
        }

        Ok(Self {
            txt: txt.into(),
            kind: Lang(tag.into()),
        })
    }

    /// Return a new literal with an arbitrary datatype.
    ///
    /// Neither is checked if `dt` refers to a known datatype nor if `txt` is
    /// ill-type and not in the lexical space of `dt`. This is intended as the
    /// [RDF specification](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-Graph-Literal)
    /// requires implementations to accept ill-typed literals.
    pub fn new_dt<U, V>(txt: U, dt: Iri<V>) -> Self
    where
        TD: From<U> + From<V>,
        V: TermData,
    {
        Self {
            txt: txt.into(),
            kind: Dt(dt.map_into()),
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
        debug_assert!(
            LanguageTag::parse(tag.as_ref()).is_ok(),
            "invalid language tag {:?}",
            tag.as_ref()
        );

        Self {
            txt: txt.into(),
            kind: Lang(tag.into()),
        }
    }

    /// Borrow the inner contents of the literal.
    pub fn as_ref(&self) -> Literal<&TD> {
        let txt = &self.txt;
        let kind = match &self.kind {
            Lang(tag) => Lang(tag),
            Dt(dt) => Dt(dt.as_ref()),
        };
        Literal { txt, kind }
    }

    /// Borrow the inner contents of the literal as `&str`.
    pub fn as_ref_str(&self) -> Literal<&str> {
        let txt = self.txt.as_ref();
        let kind = match &self.kind {
            Lang(tag) => Lang(tag.as_ref()),
            Dt(dt) => Dt(dt.as_ref_str()),
        };
        Literal { txt, kind }
    }

    /// Create a new literal by applying `f` to the `TermData` of `self`.
    pub fn map<F, TD2>(self, f: F) -> Literal<TD2>
    where
        F: FnMut(TD) -> TD2,
        TD2: TermData,
    {
        let mut f = f;
        let txt = f(self.txt);
        let kind = match self.kind {
            Lang(tag) => Lang(f(tag)),
            Dt(dt) => Dt(dt.map(f)),
        };
        Literal { txt, kind }
    }

    /// Maps the literal using the `Into` trait.
    pub fn map_into<TD2>(self) -> Literal<TD2>
    where
        TD: Into<TD2>,
        TD2: TermData,
    {
        self.map(Into::into)
    }

    /// Clone self while transforming the inner `TermData` with the given
    /// factory.
    ///
    /// This is done in one step in contrast to calling `clone().map(factory)`.
    pub fn clone_map<'a, U, F>(&'a self, factory: F) -> Literal<U>
    where
        U: TermData,
        F: FnMut(&'a str) -> U,
    {
        let mut factory = factory;
        let txt = factory(self.txt.as_ref());
        let kind = match &self.kind {
            Lang(tag) => Lang(factory(tag.as_ref())),
            Dt(iri) => Dt(iri.clone_map(factory)),
        };

        Literal { txt, kind }
    }

    /// Apply `clone_map()` using the `Into` trait.
    pub fn clone_into<'src, U>(&'src self) -> Literal<U>
    where
        U: TermData + From<&'src str>,
    {
        self.clone_map(Into::into)
    }

    /// Return a literal equivalent to this one,
    /// with its datatype (if any)
    /// is internally represented with all its data in `ns`, and an empty `suffix`.
    ///
    /// # Performances
    /// The returned literal will borrow data from this one as much as possible,
    /// but strings may be allocated in case a concatenation is required.
    pub fn normalized(&self, policy: Normalization) -> Literal<MownStr> {
        let txt = MownStr::from(self.txt.as_ref());
        let kind = match &self.kind {
            Lang(tag) => Lang(MownStr::from(tag.as_ref())),
            Dt(iri) => Dt(iri.normalized(policy)),
        };
        Literal { txt, kind }
    }

    /// Writes the literal to the `fmt::Write` using the NTriples syntax.
    pub fn write_fmt<W>(&self, w: &mut W) -> fmt::Result
    where
        W: fmt::Write,
    {
        w.write_char('"')?;
        fmt_quoted_string(w, self.txt.as_ref())?;

        match &self.kind {
            Lang(tag) => {
                w.write_str("\"@")?;
                w.write_str(tag.as_ref())
            }
            Dt(dt) => {
                if &xsd::string != dt {
                    w.write_str("\"^^")?;
                    dt.write_fmt(w)
                } else {
                    w.write_char('"')
                }
            }
        }
    }

    /// Writes the literal to the `io::Write` using the NTriples syntax.
    pub fn write_io<W>(&self, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        w.write_all(b"\"")?;
        io_quoted_string(w, self.txt.as_ref().as_bytes())?;

        match &self.kind {
            Lang(tag) => {
                w.write_all(b"\"@")?;
                w.write_all(tag.as_ref().as_bytes())
            }
            Dt(dt) => {
                if &xsd::string != dt {
                    w.write_all(b"\"^^")?;
                    dt.write_io(w)
                } else {
                    w.write_all(b"\"")
                }
            }
        }
    }

    /// Returns the literal's lexical value.
    pub fn txt(&self) -> &TD {
        &self.txt
    }

    /// Return an IRI borrowing the literals datatype.
    ///
    /// _Note:_ A language-tagged literal has always the type `rdf:langString`.
    pub fn dt(&self) -> Iri<&str> {
        match &self.kind {
            Lang(_) => rdf::langString.into(),
            Dt(dt) => dt.as_ref_str(),
        }
    }

    /// Return the language-tag of the literal if it has one.
    pub fn lang(&self) -> Option<&TD> {
        if let Lang(tag) = &self.kind {
            Some(tag)
        } else {
            None
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

impl<TD: TermData> TTerm for Literal<TD> {
    fn kind(&self) -> TermKind {
        TermKind::Literal
    }
    fn value_raw(&self) -> RawValue {
        self.txt.as_ref().into()
    }
    fn datatype(&self) -> Option<SimpleIri> {
        Some(self.dt().into())
    }
    fn language(&self) -> Option<&str> {
        self.lang().map(|td| td.as_ref())
    }
    fn as_dyn(&self) -> &dyn TTerm {
        self
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

impl<T, TD> From<NativeLiteral<T>> for Literal<TD>
where
    T: DataType + ?Sized,
    TD: TermData + From<Box<str>> + From<&'static str>,
{
    fn from(other: NativeLiteral<T>) -> Literal<TD> {
        Literal::new_dt(other.lexval, Iri::<&'static str>::from(T::iri()))
    }
}

impl<'a, T> From<NativeLiteral<T, &'a str>> for Literal<&'a str>
where
    T: DataType + ?Sized,
{
    fn from(other: NativeLiteral<T, &'a str>) -> Literal<&'a str> {
        Literal::new_dt(other.lexval, Iri::<&'static str>::from(T::iri()))
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
            _ => Err(TermError::UnsupportedKind(term.to_string())),
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
            Term::Literal(lit) => Ok(lit.clone_into()),
            _ => Err(TermError::UnsupportedKind(term.to_string())),
        }
    }
}

impl<TD> TryCopyTerm for Literal<TD>
where
    TD: TermData + for<'x> From<&'x str>,
{
    type Error = TermError;

    fn try_copy<T>(term: &T) -> Result<Self, Self::Error>
    where
        T: TTerm + ?Sized,
    {
        if term.kind() == TermKind::Literal {
            let txt = term.value_raw().0;
            Ok(match term.language() {
                None => Self::new_dt(txt, term.datatype().unwrap().into()),
                Some(tag) => Self::new_lang_unchecked(txt, tag),
            })
        } else {
            Err(TermError::UnsupportedKind(term_to_string(term)))
        }
    }
}

impl<TD, TE> PartialEq<TE> for Literal<TD>
where
    TD: TermData,
    TE: TTerm + ?Sized,
{
    fn eq(&self, other: &TE) -> bool {
        term_eq(self, other)
    }
}

impl<T: TermData> Eq for Literal<T> {}

impl<TD, TE> PartialOrd<TE> for Literal<TD>
where
    TD: TermData,
    TE: TTerm + ?Sized,
{
    fn partial_cmp(&self, other: &TE) -> Option<std::cmp::Ordering> {
        Some(term_cmp(self, other))
    }
}

impl<TD: TermData> Ord for Literal<TD> {
    fn cmp(&self, other: &Literal<TD>) -> std::cmp::Ordering {
        term_cmp(self, other)
    }
}

impl<TD> Hash for Literal<TD>
where
    TD: TermData,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        term_hash(self, state)
    }
}

impl<'a, TD: TermData + 'a> std::borrow::Borrow<dyn TTerm + 'a> for Literal<TD> {
    fn borrow(&self) -> &(dyn TTerm + 'a) {
        self
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
    // Most of the code from this module is tested through its use in other modules
    // (especially the ::term::test module).

    use super::*;

    #[test]
    fn convert_to_mown_does_not_allocate() {
        let dt = Iri::<&'static str>::from(xsd::string);
        let lit1 = Literal::<Box<str>>::new_dt("hello", dt);
        let lit2: Literal<MownStr> = lit1.clone_into();
        let Literal { txt, .. } = lit2;
        assert!(txt.is_borrowed(), "txt has been allocated");
    }

    #[test]
    fn resolve_to_mown_does_not_allocate_txt() {
        use sophia_iri::resolve::{IriParsed, Resolve};
        let dt1 = Iri::<Box<str>>::new("").unwrap();
        let lit1 = Literal::<Box<str>>::new_dt("hello", dt1);
        let xsd_string = &xsd::string.value();
        let base = IriParsed::new(&xsd_string).unwrap();
        let lit2: Literal<MownStr> = base.resolve(&lit1);
        let Literal { txt, .. } = lit2;
        assert!(txt.is_borrowed(), "txt has been allocated");
    }

    #[test]
    fn map() {
        let dt = Iri::<&str>::new_suffixed("some/iri/", "example").unwrap();
        let input = Literal::new_dt("test", dt);
        let dt2 = Iri::<&str>::new("SOME/IRI/EXAMPLE").unwrap();
        let expect = Literal::<&str>::new_dt("TEST", dt2);

        let mut cnt = 0;
        let mut invoked = 0;

        let cl = input.clone_map(|s: &str| {
            cnt += s.len();
            invoked += 1;
            s.to_ascii_uppercase()
        });
        assert_eq!(cl, expect);
        assert_eq!(cnt, "some/iri/exampletest".len());
        assert_eq!(invoked, 3);

        cnt = 0;
        invoked = 0;
        let mapped = input.map(|s: &str| {
            cnt += s.len();
            invoked += 1;
            s.to_ascii_uppercase()
        });
        assert_eq!(mapped, expect);
        assert_eq!(cnt, "some/iri/exampletest".len());
        assert_eq!(invoked, 3);

        assert_eq!(
            cl.map_into::<Box<str>>(),
            mapped.clone_into::<std::sync::Arc<str>>()
        );
    }
}
