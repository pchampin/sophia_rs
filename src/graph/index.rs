// TODO properly document

use std::borrow::Borrow;
use std::collections::{HashMap, hash_map};

use ::term::*;

pub type TermIndex<'a, V> = HashMap<RefTerm<'a>, V>;

pub struct PairIndex<'a, V> {
    map: HashMap<RefTerm<'a>, TermIndex<'a, V>>,
    len: usize,
}

impl<'a, V> PairIndex<'a, V> {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn map(&self) -> &HashMap<RefTerm<'a>, TermIndex<'a, V>> {
        &self.map
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<RefTerm<'a>, TermIndex<'a, V>> {
        self.map.iter()
    }

    #[inline]
    pub fn values(&self) -> hash_map::Values<RefTerm<'a>, TermIndex<'a, V>> {
        self.map.values()
    }

    #[inline]
    pub fn values_flat(&self) -> impl Iterator<Item=&V> {
        self.map.values().flat_map(|subindex| subindex.values())
    }

    #[inline]
    pub fn get<'b, T> (&'b self, t: &'b Term<T>) -> Option<&'b TermIndex<'a, V>> where
        T: Borrow<str>,
    {
        let t = RefTerm::from(t);
        self.map.get(&t)
    }

    pub fn insert(&mut self, t1: RefTerm<'a>, t2: RefTerm<'a>, val: V) -> Option<V> {
        let modified =
            self.map.entry(t1)
                    .or_insert_with(TermIndex::new)
                    .insert(t2, val);
        if modified.is_none() { self.len += 1 }
        modified
    }

    pub fn remove(&mut self, t1: &RefTerm<'a>, t2: &RefTerm<'a>) -> Option<V> {
        if let Some(subindex) = self.map.get_mut(t1) {
            let modified = subindex.remove(t2);
            if modified.is_some() {
                self.len -= 1;
            }
            modified
        } else {
            None
        }
    }
}

impl<'a, V> Default for PairIndex<'a, V> {
    fn default() -> Self {
        PairIndex {
            map: HashMap::default(),
            len: usize::default(),
        }
    }
}



pub struct TripleIndex<'a, V> {
    map: HashMap<RefTerm<'a>, PairIndex<'a, V>>,
    len: usize,
}

impl<'a, V> TripleIndex<'a, V> where {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn map(&self) -> &HashMap<RefTerm<'a>, PairIndex<'a, V>> {
        &self.map
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn get<'b, T> (&'b self, t: &'b Term<T>) -> Option<&'b PairIndex<'a, V>> where
        T: Borrow<str>,
    {
        let t = RefTerm::from(t);
        self.map.get(&t)
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<RefTerm<'a>, PairIndex<'a, V>> {
        self.map.iter()
    }

    #[inline]
    pub fn values(&self) -> hash_map::Values<RefTerm<'a>, PairIndex<'a, V>> {
        self.map.values()
    }

    #[inline]
    pub fn values_flat(&self) -> impl Iterator<Item=&V> {
        self.map.values().flat_map(|subindex| subindex.values_flat())
    }

    pub fn insert(&mut self, t1: RefTerm<'a>, t2: RefTerm<'a>, t3: RefTerm<'a>, val: V) -> Option<V> {
        let modified =
            self.map.entry(t1)
                    .or_insert_with(PairIndex::new)
                    .insert(t2, t3, val);
        if modified.is_none() { self.len += 1 }
        modified
    }

    pub fn remove(&mut self, t1: &RefTerm<'a>, t2: &RefTerm<'a>, t3: &RefTerm<'a>) -> Option<V> {
        if let Some(subindex) = self.map.get_mut(t1) {
            let modified = subindex.remove(t2, t3);
            if modified.is_some() {
                self.len -= 1;
            }
            modified
        } else {
            None
        }
    }
}

impl<'a, V> Default for TripleIndex<'a, V> {
    fn default() -> Self {
        TripleIndex {
            map: HashMap::default(),
            len: usize::default(),
        }
    }
}