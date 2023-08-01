//! A [stash](GenericStash) is a collection of strings that can be reused
//! to avoid allocating identical string multiple times.

use std::{borrow::Borrow, collections::BTreeSet};

use sophia_api::term::{BnodeId, IriRef, LanguageTag, SimpleTerm, Term, VarName};

use crate::{GenericLiteral, GenericTerm};

/// A [stash](GenericStash) is a collection of strings that can be reused
/// to avoid allocating identical string multiple times.
#[derive(Clone, Debug)]
pub struct GenericStash<T> {
    store: BTreeSet<T>,
}

impl<T> GenericStash<T>
where
    T: Ord,
{
    /// Create a new empty stash
    pub fn new() -> Self {
        Default::default()
    }

    /// Retrieve a value from the stash, if present
    pub fn get<Q>(&self, probe: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: Ord + ?Sized,
    {
        self.store.get(probe)
    }

    /// Retrieve a value from the stash, inserting it if not present
    pub fn get_or_insert<Q>(&mut self, probe: &Q) -> &T
    where
        T: Borrow<Q> + for<'x> From<&'x Q>,
        Q: Ord + ?Sized,
    {
        if !self.store.contains(probe) {
            let ins = T::from(probe);
            self.store.insert(ins);
        }
        let ret = self.store.get(probe);
        debug_assert!(ret.is_some());
        // this is safe because we just inserted this element
        unsafe { ret.unwrap_unchecked() }
    }

    /// How many values are stored in this stash
    pub fn len(&self) -> usize {
        self.store.len()
    }

    /// Is this stash empty?
    pub fn is_empty(&self) -> bool {
        self.store.is_empty()
    }
}

impl<T> Default for GenericStash<T> {
    fn default() -> Self {
        Self {
            store: BTreeSet::default(),
        }
    }
}

impl<T> GenericStash<T>
where
    T: Ord + for<'x> From<&'x str> + Borrow<str> + Clone,
{
    /// Copy any [`Borrow<str>`] into an `T` backed on this stash.
    pub fn copy_str<U: Borrow<str>>(&mut self, iri: U) -> T {
        self.get_or_insert(iri.borrow()).clone()
    }

    /// Copy any [`IriRef`] into an [`IriRef<T>`] backed on this stash.
    pub fn copy_iri<U: Borrow<str>>(&mut self, iri: IriRef<U>) -> IriRef<T> {
        IriRef::new_unchecked(self.copy_str(iri))
    }

    /// Copy any [`BnodeId`] into an [`BnodeId<T>`] backed on this stash.
    pub fn copy_bnode_id<U: Borrow<str>>(&mut self, bnid: BnodeId<U>) -> BnodeId<T> {
        BnodeId::new_unchecked(self.copy_str(bnid))
    }

    /// Copy any [`LanguageTag`] into an [`LanguageTag<T>`] backed on this stash.
    pub fn copy_language_tag<U: Borrow<str>>(&mut self, tag: LanguageTag<U>) -> LanguageTag<T> {
        LanguageTag::new_unchecked(self.copy_str(tag))
    }

    /// Copy any [`VarName`] into an [`VarName<T>`] backed on this stash.
    pub fn copy_var_name<U: Borrow<str>>(&mut self, vn: VarName<U>) -> VarName<T> {
        VarName::new_unchecked(self.copy_str(vn))
    }

    /// Copy any [`Term`] into an [`GenericTerm<T>`] backed on this stash.
    pub fn copy_term<U: Term>(&mut self, t: U) -> GenericTerm<T> {
        use SimpleTerm::*;
        match t.as_simple() {
            Iri(iri) => GenericTerm::Iri(self.copy_iri(iri)),
            BlankNode(bnid) => GenericTerm::BlankNode(self.copy_bnode_id(bnid)),
            LiteralDatatype(lex, dt) => {
                GenericTerm::Literal(GenericLiteral::Typed(self.copy_str(lex), self.copy_iri(dt)))
            }
            LiteralLanguage(lex, tag) => GenericTerm::Literal(GenericLiteral::LanguageString(
                self.copy_str(lex),
                self.copy_language_tag(tag),
            )),
            Triple(tr) => GenericTerm::Triple(Box::new(tr.map(|t| self.copy_term(t)))),
            Variable(vn) => GenericTerm::Variable(self.copy_var_name(vn)),
        }
    }
}
