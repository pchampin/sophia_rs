//! A [`TermIndex`] is a bidirectional assocuation of [terms](Term) with short numeric [indices](Index).
use sophia_api::ns::{IriRef, rdf};
use sophia_api::term::{
    BaseDirection, BnodeId, GraphName, LanguageTag, SimpleTerm, Term, TermKind, VarName,
};

use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::convert::Infallible;
use std::error::Error;

/// Abstraction of the short numeric indices representing [terms](Term) in a [`TermIndex`].
pub trait Index: Copy + std::fmt::Debug + Eq + std::hash::Hash + Ord + std::ops::Shr {
    /// The minimal value of this index type
    const ZERO: Self;
    /// The maximal value of this index type
    const MAX: Self;
    /// Convert from usize
    fn from_usize(other: usize) -> Self;
    /// Convert to usize
    fn into_usize(self) -> usize;
}

impl Index for usize {
    const ZERO: Self = 0;
    const MAX: Self = usize::MAX;
    fn from_usize(other: usize) -> Self {
        other
    }
    fn into_usize(self) -> usize {
        self
    }
}

impl Index for u32 {
    const ZERO: Self = 0;
    const MAX: Self = u32::MAX;
    fn from_usize(other: usize) -> Self {
        other
            .try_into()
            .map_err(|_| ())
            .expect("usize too big to be converted to u32")
    }
    fn into_usize(self) -> usize {
        self as usize
    }
}

impl Index for u16 {
    const ZERO: Self = 0;
    const MAX: Self = u16::MAX;
    fn from_usize(other: usize) -> Self {
        other
            .try_into()
            .map_err(|_| ())
            .expect("usize too big to be converted to u16")
    }
    fn into_usize(self) -> usize {
        self as usize
    }
}

//

/// A [`TermIndex`] is a bidirectional association of [terms](Term) with short numeric [indices](Index).
pub trait TermIndex {
    /// The type of [`Term`]s contained in this [`TermIndex`]
    type Term<'x>: Term + Copy
    where
        Self: 'x;
    /// The type of [indices](Index) used by this [`TermIndex`]
    type Index: Index;
    /// The type of error that this [`TermIndex`] may raise
    type Error: Error + Send + Sync + 'static;

    /// Get the index corresponding to term `t`, if it exists.
    ///
    /// Return `None` if this term-index does not contain a term equivalent to `t`.
    fn get_index<T: Term>(&self, t: T) -> Option<Self::Index>;
    /// Get the index corresponding to term `t`, adding it in the term-index if necessary.
    ///
    /// Returns an error if `t` can not be added in this term-index
    /// (some term-index may support only a subset of RDF terms).
    fn ensure_index<T: Term>(&mut self, t: T) -> Result<Self::Index, Self::Error>;
    /// Get the term corresponding to index `i`.
    ///
    /// # Precondition
    /// `i` must have been returned previously by [`get_index`](TermIndex::get_index) or [`ensure_index`](TermIndex::ensure_index),
    /// otherwise this method may panic.
    fn get_term(&self, i: Self::Index) -> Self::Term<'_>;
}

/// [`GraphNameIndex`] extends [`TermIndex`] to support graph names.
///
/// This implies that one index value is reserved for the default graph.
pub trait GraphNameIndex: TermIndex {
    /// Get the index corresponding to the default Graph.
    ///
    /// This value is never returned by `get_index` or `ensure_index`.
    fn get_default_graph_index(&self) -> Self::Index;
    /// Get the graph name corresponding to index `i`.
    ///
    /// # Precondition
    /// `i` must have been returned previously by [`get_index`](TermIndex::get_index) or [`ensure_index`](TermIndex::ensure_index),
    /// otherwise this method may panic.
    fn get_graph_name(&self, i: Self::Index) -> GraphName<Self::Term<'_>> {
        if i == self.get_default_graph_index() {
            None
        } else {
            Some(self.get_term(i))
        }
    }
    /// Get the index corresponding to graph name `n`, if it exists.
    fn get_graph_name_index<T: Term>(&self, g: GraphName<T>) -> Option<Self::Index> {
        match g {
            None => Some(self.get_default_graph_index()),
            Some(t) => self.get_index(t),
        }
    }
}

//

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum InternalTerm<I: Index> {
    Iri(IriRef<Box<str>>),
    BlankNode(BnodeId<Box<str>>),
    LiteralTyped(Box<str>, I),
    LiteralLang(Box<str>, LanguageTag<Box<str>>, Option<BaseDirection>),
    Triple([I; 3]),
    Variable(VarName<Box<str>>),
}

impl<I: Index> InternalTerm<I> {
    fn copy_atom<T: Term>(t: T) -> Self {
        match t.as_simple() {
            SimpleTerm::Iri(iri_ref) => Self::Iri(iri_ref.as_ref().map_unchecked(Into::into)),
            SimpleTerm::BlankNode(bnid) => Self::BlankNode(bnid.as_ref().map_unchecked(Into::into)),
            SimpleTerm::LiteralDatatype(..) => {
                panic!("typed literal passed to IndexedTerm::copy_atom")
            }
            SimpleTerm::LiteralLanguage(lex, tag, dir) => Self::LiteralLang(
                lex.as_ref().into(),
                tag.as_ref().map_unchecked(Into::into),
                dir,
            ),
            SimpleTerm::Triple(_) => panic!("triple term passed to IndexedTerm::copy_atom"),
            SimpleTerm::Variable(name) => Self::Variable(name.as_ref().map_unchecked(Into::into)),
        }
    }
}

//

/// The [`Term`] implementation returned by [`BasicTermIndex`].
#[derive(Clone, Copy, Debug)]
pub struct IndexedTerm<'a, I: Index> {
    t: &'a BasicTermIndex<I>,
    i: I,
}

impl<'a, I: Index> IndexedTerm<'a, I> {
    /// Return the [`BasicTermIndex`] this [`IndexedTerm`] comes from
    pub fn term_index(&self) -> &BasicTermIndex<I> {
        self.t
    }

    /// Return the index of that [`IndexedTerm`]
    pub fn index(&self) -> I {
        self.i
    }

    /// Return the index of that [`IndexedTerm`]'s datatype of it contains one.
    pub fn datatype_index(&self) -> Option<I> {
        if let InternalTerm::LiteralTyped(_, idt) = &self.t.i2t[self.i.into_usize()] {
            Some(*idt)
        } else {
            None
        }
    }
}

impl<'a, I: Index> Term for IndexedTerm<'a, I> {
    type BorrowTerm<'x>
        = Self
    where
        Self: 'x;

    fn kind(&self) -> TermKind {
        match self.t.i2t[self.i.into_usize()] {
            InternalTerm::Iri(_) => TermKind::Iri,
            InternalTerm::BlankNode(_) => TermKind::BlankNode,
            InternalTerm::LiteralTyped(..) | InternalTerm::LiteralLang(..) => TermKind::Literal,
            InternalTerm::Triple(_) => TermKind::Triple,
            InternalTerm::Variable(_) => TermKind::Variable,
        }
    }

    fn borrow_term(&self) -> Self::BorrowTerm<'_> {
        *self
    }

    fn iri(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr<'_>>> {
        if let InternalTerm::Iri(iri) = &self.t.i2t[self.i.into_usize()] {
            Some(iri.as_ref().map_unchecked(Into::into))
        } else {
            None
        }
    }

    fn bnode_id(&self) -> Option<BnodeId<sophia_api::MownStr<'_>>> {
        if let InternalTerm::BlankNode(bnid) = &self.t.i2t[self.i.into_usize()] {
            Some(bnid.as_ref().map_unchecked(Into::into))
        } else {
            None
        }
    }

    fn lexical_form(&self) -> Option<sophia_api::MownStr<'_>> {
        match &self.t.i2t[self.i.into_usize()] {
            InternalTerm::LiteralTyped(lex, ..) | InternalTerm::LiteralLang(lex, ..) => {
                Some(lex.as_ref().into())
            }
            _ => None,
        }
    }

    fn datatype(&self) -> Option<sophia_api::term::IriRef<sophia_api::MownStr<'_>>> {
        match &self.t.i2t[self.i.into_usize()] {
            InternalTerm::LiteralTyped(_, dt) => {
                let InternalTerm::Iri(iri) = &self.t.i2t[dt.into_usize()] else {
                    unreachable!();
                };
                Some(iri.as_ref().map_unchecked(Into::into))
            }
            InternalTerm::LiteralLang(_, _, None) => rdf::langString.iri(),
            InternalTerm::LiteralLang(_, _, Some(_)) => rdf::dirLangString.iri(),
            _ => None,
        }
    }

    fn language_tag(&self) -> Option<LanguageTag<sophia_api::MownStr<'_>>> {
        if let InternalTerm::LiteralLang(_, tag, _) = &self.t.i2t[self.i.into_usize()] {
            Some(tag.as_ref().map_unchecked(Into::into))
        } else {
            None
        }
    }

    fn base_direction(&self) -> Option<BaseDirection> {
        if let InternalTerm::LiteralLang(.., dir) = &self.t.i2t[self.i.into_usize()] {
            *dir
        } else {
            None
        }
    }

    fn variable(&self) -> Option<VarName<sophia_api::MownStr<'_>>> {
        if let InternalTerm::Variable(varname) = &self.t.i2t[self.i.into_usize()] {
            Some(varname.as_ref().map_unchecked(Into::into))
        } else {
            None
        }
    }

    fn triple(&self) -> Option<[Self::BorrowTerm<'_>; 3]> {
        if let InternalTerm::Triple(spo) = &self.t.i2t[self.i.into_usize()] {
            let t = self.t;
            Some(spo.map(|i| Self { t, i }))
        } else {
            None
        }
    }

    fn to_triple(self) -> Option<[Self; 3]>
    where
        Self: Sized,
    {
        self.triple()
    }
}

impl<'a, I: Index, T: Term> PartialEq<T> for IndexedTerm<'a, I> {
    fn eq(&self, other: &T) -> bool {
        Term::eq(self, other.borrow_term())
    }
}

impl<'a, I: Index> Eq for IndexedTerm<'a, I> {}

impl<'a, I: Index, T: Term> PartialOrd<T> for IndexedTerm<'a, I> {
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        Some(Term::cmp(self, other.borrow_term()))
    }
}

impl<'a, I: Index> Ord for IndexedTerm<'a, I> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Term::cmp(self, *other)
    }
}

impl<'a, I: Index> std::hash::Hash for IndexedTerm<'a, I> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Term::hash(self, state)
    }
}

/// A generic implementation of [`TermIndex`].
#[derive(Clone, Debug, Default)]
pub struct BasicTermIndex<I: Index> {
    t2i: HashMap<InternalTerm<I>, I>,
    i2t: Vec<InternalTerm<I>>,
}

impl<I: Index> BasicTermIndex<I> {
    /// Construct an empty index
    pub fn new() -> Self {
        BasicTermIndex {
            t2i: HashMap::new(),
            i2t: vec![],
        }
    }

    /// The number of terms in this index
    pub fn len(&self) -> usize {
        self.i2t.len()
    }

    /// Whether this index is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Ensure a triple term is indexed, returning its index
    ///
    /// This is more efficient than [`TermIndex::ensure_index`]
    /// when the indexes of the constituents are already known.
    pub fn ensure_triple_term_index(&mut self, spo: [I; 3]) -> Result<I, TermIndexFullError> {
        debug_assert!(spo.iter().copied().all(|i| i.into_usize() < self.len()));
        self.ensure_index_internal(InternalTerm::Triple(spo))
    }

    fn ensure_index_internal(&mut self, it: InternalTerm<I>) -> Result<I, TermIndexFullError> {
        match self.t2i.entry(it) {
            Entry::Vacant(e) => {
                let i = I::from_usize(self.i2t.len());
                if i >= I::MAX {
                    return Err(TermIndexFullError());
                }
                self.i2t.push(e.key().clone());
                e.insert(i);
                Ok(i)
            }
            Entry::Occupied(e) => Ok(*e.get()),
        }
    }

    /// Iter over all term in this TermIndex
    pub fn iter(&self) -> impl Iterator<Item = IndexedTerm<'_, I>> {
        let t = self;
        (0..self.i2t.len()).map(|i| IndexedTerm {
            t,
            i: I::from_usize(i),
        })
    }

    /// Iter over all IRIs in this TermIndex
    pub fn iris(&self) -> impl Iterator<Item = IndexedTerm<'_, I>> {
        let t = self;
        self.i2t.iter().enumerate().filter_map(|(i, it)| {
            matches!(it, InternalTerm::Iri(_)).then_some(IndexedTerm {
                t,
                i: I::from_usize(i),
            })
        })
    }

    /// Iter over all blank nodes in this TermIndex
    pub fn blank_nodes(&self) -> impl Iterator<Item = IndexedTerm<'_, I>> {
        let t = self;
        self.i2t.iter().enumerate().filter_map(|(i, it)| {
            matches!(it, InternalTerm::BlankNode(_)).then_some(IndexedTerm {
                t,
                i: I::from_usize(i),
            })
        })
    }

    /// Iter over all literals in this TermIndex
    pub fn literals(&self) -> impl Iterator<Item = IndexedTerm<'_, I>> {
        let t = self;
        self.i2t.iter().enumerate().filter_map(|(i, it)| {
            matches!(
                it,
                InternalTerm::LiteralTyped(..) | InternalTerm::LiteralLang(..)
            )
            .then_some(IndexedTerm {
                t,
                i: I::from_usize(i),
            })
        })
    }

    /// Iter over all triple terms in this TermIndex
    pub fn triple_terms(&self) -> impl Iterator<Item = IndexedTerm<'_, I>> {
        let t = self;
        self.i2t.iter().enumerate().filter_map(|(i, it)| {
            matches!(it, InternalTerm::Triple(_)).then_some(IndexedTerm {
                t,
                i: I::from_usize(i),
            })
        })
    }

    /// Iter over all variables in this TermIndex
    pub fn variables(&self) -> impl Iterator<Item = IndexedTerm<'_, I>> {
        let t = self;
        self.i2t.iter().enumerate().filter_map(|(i, it)| {
            matches!(it, InternalTerm::Variable(_)).then_some(IndexedTerm {
                t,
                i: I::from_usize(i),
            })
        })
    }
}

impl<I: Index> TermIndex for BasicTermIndex<I> {
    type Term<'x>
        = IndexedTerm<'x, I>
    where
        Self: 'x;
    type Index = I;
    type Error = TermIndexFullError;

    fn get_index<T: Term>(&self, t: T) -> Option<Self::Index> {
        let key = if let Some([s, p, o]) = t.triple() {
            let is = self.get_index(s)?;
            let ip = self.get_index(p)?;
            let io = self.get_index(o)?;
            InternalTerm::Triple([is, ip, io])
        } else if let SimpleTerm::LiteralDatatype(lex, dt) = t.as_simple() {
            let idt = self.get_index(dt)?;
            InternalTerm::LiteralTyped(lex.as_ref().into(), idt)
        } else {
            InternalTerm::copy_atom(t)
        };
        self.t2i.get(&key).copied()
    }

    fn ensure_index<T: Term>(&mut self, t: T) -> Result<Self::Index, Self::Error> {
        let it = if let Some([s, p, o]) = t.triple() {
            let is = self.ensure_index(s)?;
            let ip = self.ensure_index(p)?;
            let io = self.ensure_index(o)?;
            InternalTerm::Triple([is, ip, io])
        } else if let SimpleTerm::LiteralDatatype(lex, dt) = t.as_simple() {
            let idt = self.ensure_index(dt)?;
            InternalTerm::LiteralTyped(lex.as_ref().into(), idt)
        } else {
            InternalTerm::copy_atom(t)
        };
        self.ensure_index_internal(it)
    }

    fn get_term(&self, i: Self::Index) -> Self::Term<'_> {
        assert!(i.into_usize() < self.i2t.len());
        IndexedTerm { t: self, i }
    }
}

impl<I: Index> GraphNameIndex for BasicTermIndex<I> {
    fn get_default_graph_index(&self) -> Self::Index {
        Self::Index::MAX
    }
}

/// An error type to indicate that a [`BasicTermIndex`] is full
#[derive(thiserror::Error, Copy, Clone, Debug)]
#[error("This TermIndex can not contain more terms")]
pub struct TermIndexFullError();

impl From<Infallible> for TermIndexFullError {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::ns::{Namespace, xsd};
    use sophia_api::term::BnodeId;

    #[test]
    fn basic_term_index() -> Result<(), Box<dyn std::error::Error>> {
        let ex = Namespace::new_unchecked("https://example.com/ns/");
        let exa = ex.get("a")?;
        let exb = ex.get("b")?;
        let bn1 = BnodeId::new_unchecked("bn1");

        let mut sti = BasicTermIndex::<u32>::new();
        assert!(sti.is_empty());
        assert_eq!(sti.len(), 0);
        assert_eq!(sti.get_default_graph_index(), u32::MAX);

        assert_eq!(sti.get_index(exa), None);
        assert_eq!(sti.get_index(exb), None);
        assert_eq!(sti.get_index(bn1), None);
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(exa)?, 0);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 1);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), None);
        assert_eq!(sti.get_index(bn1), None);
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(exb)?, 1);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 2);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), None);
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(bn1)?, 2);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 3);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), Some(2));
        assert_eq!(sti.get_index("hello world"), None);
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index("hello world")?, 4);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 5);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), Some(2));
        assert_eq!(sti.get_index(xsd::string), Some(3));
        assert_eq!(sti.get_index("hello world"), Some(4));
        assert_eq!(sti.get_index(42), None);

        assert_eq!(sti.ensure_index(42)?, 6);
        assert!(!sti.is_empty());
        assert_eq!(sti.len(), 7);
        assert_eq!(sti.get_index(exa), Some(0));
        assert_eq!(sti.get_index(exb), Some(1));
        assert_eq!(sti.get_index(bn1), Some(2));
        assert_eq!(sti.get_index(xsd::string), Some(3));
        assert_eq!(sti.get_index("hello world"), Some(4));
        assert_eq!(sti.get_index(xsd::integer), Some(5));
        assert_eq!(sti.get_index(42), Some(6));

        assert!(Term::eq(&sti.get_term(0), exa));
        assert!(Term::eq(&sti.get_term(1), exb));
        assert!(Term::eq(&sti.get_term(2), bn1));
        assert!(Term::eq(&sti.get_term(3), xsd::string));
        assert!(Term::eq(&sti.get_term(4), "hello world"));
        assert!(Term::eq(&sti.get_term(5), xsd::integer));
        assert!(Term::eq(&sti.get_term(6), 42));

        Ok(())
    }

    #[cfg(feature = "all_tests")]
    #[test]
    fn big_basic_term_index() {
        const MAX: i32 = 20_000;
        let mut sti = BasicTermIndex::<u32>::new();
        // NB: index 0 will contain the datatype xsd:integer
        for i in 1..MAX {
            assert_eq!(sti.ensure_index(i).unwrap(), i as u32);
        }
        assert_eq!(sti.len(), MAX as usize);
        for i in 1..MAX {
            assert_eq!(sti.ensure_index(i).unwrap(), i as u32);
        }
        assert_eq!(sti.len(), MAX as usize);
        for i in 1..MAX {
            assert!(Term::eq(&sti.get_term(i as u32), i));
        }
    }

    impl Index for i8 {
        const ZERO: Self = 0;
        const MAX: Self = i8::MAX;
        fn from_usize(other: usize) -> Self {
            other
                .try_into()
                .map_err(|_| ())
                .expect("usize too big to be converted to i8")
        }
        fn into_usize(self) -> usize {
            self as usize
        }
    }

    #[test]
    fn full_basic_term_index() {
        let mut sti = BasicTermIndex::<i8>::new();
        assert_eq!(sti.ensure_index(xsd::integer).unwrap(), 0);
        for i in 1..127 {
            assert_eq!(sti.ensure_index(i).unwrap(), i as i8);
        }
        assert!(sti.ensure_index(127).is_err());
    }
}
