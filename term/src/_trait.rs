//! This module is transparently re-exported by its parent `lib`.
use mownstr::MownStr;
use std::cmp::Ordering;
use std::error::Error;
use std::hash::{Hash, Hasher};

/// Trait for all RDF terms.
///
/// Sophia supports 4 kinds of terms: IRI references (absolute or relative),
/// literals, blank nodes and variables.
/// Note that strict RDF does not support relative IRI references nor variables.
///
/// Types representing terms, of one or more of the kinds above,
/// can implement this trait and be used with the rest of the Sophia API.
///
/// # Design considerations
///
/// The design of this trait is not as "pure" as it could have been:
///
/// * it merges into a single trait four "kinds"
///   which could arguably be considered as four different abstract types;
///
/// * it is rather opinionated on how implementation should store their data internally,
///   and has a very constrained contract (see below);
///
/// * it relies on the "semi-abstract" methods `value_raw`,
///   which mostly makes sense for the default implementation of other methods.
///
/// These choices were made to allow for efficient implementations of the overall API.
///
/// # Contract
///
/// In addition to the specific contract of each method,
/// any type implementing this trait must uphold the following guarantees:
///
/// * if it implements [`Hash`](https://doc.rust-lang.org/std/hash/trait.Hash.html),
///   it must be consistent with (or, even better, based on)
///   [`term_hash`](./function.term_hash.html);
///
/// * if it implements [`PartialEq`](https://doc.rust-lang.org/std/cmp/trait.PartialEq.html),
///   it must be consistent with (or, even better, based on)
///   [`term_eq`](./function.term_eq.html);
///
/// * if it implements [`PartialCmp`](https://doc.rust-lang.org/std/cmp/trait.PartialCmp.html)
///   it must be consistent with (or, even better, based on)
///   [`term_cmp`](./function.term_cmp.html);
pub trait TTerm {
    /// Returns the kind of this term (IRI, literal, blank node, variable).
    fn kind(&self) -> TermKind;

    /// Return the "value" of this term, which depends on its kind:
    /// * for an IRI reference, its value;
    /// * for a literal, its lexical value;
    /// * for a blank node, its local identifier;
    /// * for a variable, its name.
    ///
    /// # Performance
    /// The returned `MownStr` is always borrowed (equivalent to a `&str`),
    /// **except** for IRI references where this method *may* allocate a new string
    /// (depending on implementations).
    /// If this allocation is undesirable, use [`value_raw`] instead.
    ///
    /// # Note to implementors
    /// Should not be overridden; must be consistent with [`value_raw`].
    ///
    /// [`value_raw`]: #tymethod.value_raw
    fn value(&self) -> MownStr {
        raw_to_mownstr(self.value_raw())
    }

    /// Return the datatype IRI of this term if it is a literal.
    ///
    /// # Note to implementors
    /// Should not be overridden; must be consistent with [`datatype_raw`].
    fn datatype(&self) -> Option<crate::SimpleIri> {
        None
    }

    /// Return the language tag of this term if it is a language-tagged literal.
    ///
    /// # Note to implementors
    /// The default implementation always return `None`,
    /// so unless your type may represent a language-tagged literal,
    /// you do not need to override it.
    fn language(&self) -> Option<&str> {
        None
    }

    /// Return the "value" of this term, possibly split in two substrings.
    /// The second part might only be non-empty if this term is an IRI reference.
    ///
    /// See also [`value`](#method.value).
    ///
    /// # Note to implementors
    /// The second part of the raw value is intended for some implementations
    /// of IRIs, storing both a "namepsace" and a "suffix".
    /// For other kinds of term, the second part must always be None.
    fn value_raw(&self) -> (&str, Option<&str>);

    /// All terms are absolute, except for:
    /// * relative IRI references,
    /// * literals whose datatype is a relative IRI reference.
    fn is_absolute(&self) -> bool {
        match self.kind() {
            Iri => raw_absolute(self.value_raw()),
            Literal => match self.language() {
                None => raw_absolute(self.datatype().unwrap().value_raw()),
                Some(_) => true,
            },
            _ => true,
        }
    }

    /// This method ensures that all implementations of `TTerm`
    /// can be turned into a trait object.
    ///
    /// # Why is this required?
    /// After all, in most cases, passing `&t` instead of `t.as_dyn()`
    /// will work just as well.
    ///
    /// The reason is that most methods of the API will accept *references*
    /// to terms, as `&T` where `T: TTerm + ?Sized`,
    /// and such references can *not* be cast to `dyn TTerm`
    /// (see https://stackoverflow.com/a/57432042/1235487 for more details).
    fn as_dyn(&self) -> &dyn TTerm;
}

/// Any [`TTerm`](./trait.TTerm.html) belongs to one those kinds.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum TermKind {
    /// RDF [IRI](https://www.w3.org/TR/rdf11-concepts/#section-IRIs),
    /// although in Sophia they can also be
    /// [relative IRI references](https://www.ietf.org/rfc/rfc3987.html#section-6.5)
    Iri,
    /// RDF [literal](https://www.w3.org/TR/rdf11-concepts/#section-Graph-Literal)
    Literal,
    /// RDF [blank node](https://www.w3.org/TR/rdf11-concepts/#section-blank-nodes)
    BlankNode,
    /// [variable](https://www.w3.org/TR/sparql11-query/#QSynVariables)
    Variable,
}
use TermKind::*;

/// A term type that can copy any other term.
pub trait CopyTerm: TTerm + Sized {
    /// Copy `term` into an instance of this type.
    fn copy<T>(term: &T) -> Self
    where
        T: TTerm + ?Sized;
}

/// A term type that can copy some other terms.
pub trait TryCopyTerm: TTerm + Sized {
    /// The error type produced when failing to copy a given term
    type Error: 'static + Error;
    /// Try to copy `term` into an instance of this type.
    fn try_copy<T>(term: &T) -> Result<Self, Self::Error>
    where
        T: TTerm + ?Sized;
}

fn raw_to_mownstr<'a>(raw: (&'a str, Option<&'a str>)) -> MownStr<'a> {
    match raw {
        (val, None) => MownStr::from(val),
        (ns, Some(suffix)) => {
            let mut s = String::with_capacity(ns.len() + suffix.len());
            s.push_str(ns);
            s.push_str(suffix);
            s.into()
        }
    }
}

#[inline]
pub(crate) fn raw_absolute(raw: (&str, Option<&str>)) -> bool {
    match str_absolute(raw.0) {
        0 => raw.1.map(|txt| str_absolute(txt) > 0).unwrap_or(false),
        i => i > 0,
    }
}

#[inline]
fn raw_to_bytes<'a>(raw: &'a (&'a str, Option<&'a str>)) -> impl Iterator<Item = u8> + 'a {
    raw.0.bytes().chain(raw.1.unwrap_or("").bytes())
}

#[inline]
fn raw_to_len(raw: &(&str, Option<&str>)) -> usize {
    raw.0.len() + raw.1.map(str::len).unwrap_or(0)
}

#[inline]
fn raw_eq(raw1: &(&str, Option<&str>), raw2: &(&str, Option<&str>)) -> bool {
    raw_to_len(&raw1) == raw_to_len(&raw2)
        && raw_to_bytes(&raw1)
            .zip(raw_to_bytes(&raw2))
            .all(|(b1, b2)| b1 == b2)
}

#[inline]
fn raw_hash<H: Hasher>(raw: &(&str, Option<&str>), state: &mut H) {
    state.write(raw.0.as_bytes());
    if let Some(txt) = raw.1 {
        state.write(txt.as_bytes());
    }
    state.write_u8(0xff); // this is what <str as Hash>::hash() does
}

/// return value: -1 means no, 0 means maybe, 1 means yes
#[inline]
fn str_absolute(txt: &str) -> i8 {
    if txt.is_empty() {
        return 0;
    }
    for b in txt.bytes() {
        if b == b':' {
            return 1;
        }
        if !(b'A' <= b && b <= b'Z'
            || b'a' <= b && b <= b'z'
            || b'0' <= b && b <= b'9'
            || b == b'.'
            || b == b'+'
            || b == b'-')
        {
            return -1;
        }
    }
    0
}

/// Hash a term
pub fn term_hash<T, H>(term: &T, state: &mut H)
where
    H: Hasher,
    T: TTerm + ?Sized,
{
    let k = term.kind();
    let v = term.value_raw();
    k.hash(state);
    match k {
        Iri => raw_hash(&v, state),
        Literal => {
            match term.language() {
                None => raw_hash(&term.datatype().unwrap().value_raw(), state),
                Some(tag) => {
                    for b in tag.bytes() {
                        state.write_u8(b.to_ascii_uppercase());
                    }
                }
            };
            v.0.hash(state);
        }
        _ => v.0.hash(state),
    }
}

/// Compare two terms for syntactic equality.
///
/// NB: this does not take into account semantics, not even for literals.
/// For example, `"42"^^xsd:integer`, `"042"^^xsd:integer` and `"42.0"^^xsd::decimal`
/// are considered all different from each other.
pub fn term_eq<T1, T2>(t1: &T1, t2: &T2) -> bool
where
    T1: TTerm + ?Sized,
    T2: TTerm + ?Sized,
{
    let k1 = t1.kind();
    let k2 = t2.kind();
    k1 == k2 && {
        let v1 = t1.value_raw();
        let v2 = t2.value_raw();
        if matches!(k1, Iri) {
            raw_eq(&v1, &v2)
        } else {
            v1.0 == v2.0 && {
                if matches!(k1, Literal) {
                    match (t1.language(), t2.language()) {
                        (Some(tag1), Some(tag2)) => tag1.eq_ignore_ascii_case(tag2),
                        (None, None) => {
                            let dt1 = t1.datatype().unwrap();
                            let dt2 = t2.datatype().unwrap();
                            raw_eq(&dt1.value_raw(), &dt2.value_raw())
                        }
                        _ => false,
                    }
                } else {
                    true
                }
            }
        }
    }
}

/// Compare two terms:
/// * IRIs < literals < blank nodes < variables
/// * IRIs, blank nodes and variables are ordered by their value
/// * Literals are ordered by their datatype, then their language (if any),
///   then their lexical value
///
/// NB: literals are ordered by their *lexical* value,
/// so for example, `"10"^^xsd:integer` come `*before* "2"^^xsd:integer`.
pub fn term_cmp<T1, T2>(t1: &T1, t2: &T2) -> Ordering
where
    T1: TTerm + ?Sized,
    T2: TTerm + ?Sized,
{
    let k1 = t1.kind();
    let k2 = t2.kind();
    k1.cmp(&k2).then_with(|| {
        let v1 = t1.value_raw();
        let v2 = t2.value_raw();
        match k1 {
            Iri => raw_to_bytes(&v1).cmp(raw_to_bytes(&v2)),
            Literal => {
                let tag1 = t1.language();
                let tag2 = t2.language();
                //if tag1.is_some() && tag2.is_some() {
                if let (Some(tag1), Some(tag2)) = (tag1, tag2) {
                    tag1.to_uppercase()
                        .cmp(&tag2.to_uppercase())
                        .then_with(|| v1.0.cmp(&v2.0))
                } else {
                    let dt1 = t1.datatype().unwrap();
                    let dt2 = t2.datatype().unwrap();
                    raw_to_bytes(&dt1.value_raw())
                        .cmp(raw_to_bytes(&dt2.value_raw()))
                        .then_with(|| v1.0.cmp(&v2.0))
                }
            }
            _ => v1.0.cmp(&v2.0),
        }
    })
}

/// Format the given term in a Turtle-like format.
pub fn term_format<T, W>(term: &T, w: &mut W) -> std::fmt::Result
where
    T: TTerm + ?Sized,
    W: std::fmt::Write,
{
    let v = term.value_raw();
    match term.kind() {
        Iri => {
            w.write_char('<')?;
            w.write_str(&v.0)?;
            if let Some(suffix) = v.1 {
                w.write_str(suffix)?;
            }
            w.write_char('>')
        }
        Literal => {
            write!(w, "{:?}", v.0)?;
            if let Some(tag) = term.language() {
                write!(w, "@{}", tag)
            } else {
                let dt = term.datatype().unwrap();
                if !term_eq(&dt, &crate::ns::xsd::string) {
                    w.write_str("^^")?;
                    term_format(&term.datatype().unwrap(), w)?;
                }
                Ok(())
            }
        }
        BlankNode => write!(w, "_:{}", v.0),
        Variable => write!(w, "_?{}", v.0),
    }
}

/// Formats the given term in to a string.
pub fn term_to_string<T>(term: &T) -> String
where
    T: TTerm + ?Sized,
{
    format!("{}", TermFormater(term))
}

struct TermFormater<'a, T: ?Sized>(&'a T);

impl<'a, T> std::fmt::Display for TermFormater<'a, T>
where
    T: TTerm + ?Sized,
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        term_format(self.0, fmt)
    }
}
