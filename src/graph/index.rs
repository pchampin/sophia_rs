// TODO properly document

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet, hash_map};
use std::hash::Hash;

use super::super::term::*;

pub type TermIndex<T> = HashSet<Term<T>>;

pub struct PairIndex<T> where
    T: Borrow<str> + Eq + Hash,
{
    map: HashMap<Term<T>, TermIndex<T>>,
    len: usize,
}

impl<'a, T> PairIndex<T> where
    T: Borrow<str> + Eq + Hash,
{
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn map(&self) -> &HashMap<Term<T>, TermIndex<T>> {
        &self.map
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<Term<T>, TermIndex<T>> {
        self.map.iter()
    }

    #[inline]
    pub fn get(&self, t: &Term<T>) -> Option<&TermIndex<T>> {
        self.map.get(t)
    }

    pub fn insert(&mut self, t1: Term<T>, t2: Term<T>) -> bool {
        let modified =
            self.map.entry(t1)
                    .or_insert_with(TermIndex::new)
                    .insert(t2);
        if modified { self.len += 1 }
        modified
    }

    pub fn remove(&mut self, t1: &Term<T>, t2: &Term<T>) -> bool {
        if let Some(subindex) = self.map.get_mut(t1) {
            let modified = subindex.remove(t2);
            if modified {
                self.len -= 1;
            }
            modified
        } else {
            false
        }
    }
}

impl<T> Default for PairIndex<T> where
    T: Borrow<str> + Eq + Hash,
{
    fn default() -> Self {
        PairIndex {
            map: HashMap::default(),
            len: usize::default(),
        }
    }
}



pub struct TripleIndex<T> where
    T: Borrow<str> + Eq + Hash,
{
    map: HashMap<Term<T>, PairIndex<T>>,
    len: usize,
}

impl<'a, T> TripleIndex<T> where
    T: Borrow<str> + Eq + Hash,
{
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn map(&self) -> &HashMap<Term<T>, PairIndex<T>> {
        &self.map
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn get(&self, t: &Term<T>) -> Option<&PairIndex<T>> {
        self.map.get(t)
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<Term<T>, PairIndex<T>> {
        self.map.iter()
    }

    pub fn insert(&mut self, t1: Term<T>, t2: Term<T>, t3: Term<T>) -> bool {
        let modified =
            self.map.entry(t1)
                    .or_insert_with(PairIndex::new)
                    .insert(t2, t3);
        if modified { self.len += 1 }
        modified
    }

    pub fn remove(&mut self, t1: &Term<T>, t2: &Term<T>, t3: &Term<T>) -> bool {
        if let Some(subindex) = self.map.get_mut(t1) {
            let modified = subindex.remove(t2, t3);
            if modified {
                self.len -= 1;
            }
            modified
        } else {
            false
        }
    }
}

impl<T> Default for TripleIndex<T> where
    T: Borrow<str> + Eq + Hash,
{
    fn default() -> Self {
        TripleIndex {
            map: HashMap::default(),
            len: usize::default(),
        }
    }
}
