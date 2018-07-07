// TODO properly document

use std::borrow::Borrow;
use std::collections::{HashMap, hash_map};
use std::hash::Hash;

use ::term::*;



pub trait TermIndex: Default {
    type Index: Copy+Eq;
    type Holder: Borrow<str>;

    fn get_index(&self, t: &RefTerm) -> Option<Self::Index>;
    fn make_index(&mut self, t: Term<Self::Holder>) -> Self::Index;
    fn get_term(&self, i: Self::Index) -> Option<&Term<Self::Holder>>;
    fn inc_ref(&mut self, i: Self::Index);
    fn dec_ref(&mut self, i: Self::Index);
}



pub struct TermIndexU<I, T> where
    T: Borrow<str>,
{
    next_free: I,
    i2t: Vec<Option<Term<T>>>,
    i2c: Vec<I>,
    t2i: HashMap<StaticTerm, I>,
}

impl<I, T> TermIndexU<I, T> where
    I: Default,
    T: Borrow<str>,
{
    pub fn new() -> TermIndexU<I, T> {
        Self::default()
    }

    pub fn shrink_to_fit(&mut self) {
        self.i2c.shrink_to_fit();
        self.i2t.shrink_to_fit();
        self.t2i.shrink_to_fit();
        debug_assert_eq!(self.i2c.len(), self.i2t.len());
    }
}

impl<I, T> Default for TermIndexU<I, T> where
    I: Default,
    T: Borrow<str>,
{
    fn default() -> TermIndexU<I, T> {
        TermIndexU {
            next_free: I::default(),
            i2c: Vec::default(),
            i2t: Vec::default(),
            t2i: HashMap::default(),
        }
    }
}

macro_rules! impl_term_index {
    ($uXX:ty) => {
        impl<T> TermIndex for TermIndexU<$uXX, T> where
            T: Borrow<str>,
        {
            type Index = $uXX;
            type Holder = T;

            fn get_index(&self, t: &RefTerm) -> Option<$uXX> {
                self.t2i.get(t).map(|iref| *iref)
            }

            fn make_index(&mut self, t: Term<T>) -> $uXX {
                let rt = unsafe { fake_static(&t) };
                if let Some(i) = self.get_index(&rt) {
                    self.i2c[i as usize] += 1;
                    return i;
                }
                self.t2i.insert(rt, self.next_free);
                let i = self.next_free as usize;
                if i == self.i2t.len() {
                    self.next_free += 1;
                    self.i2t.push(Some(t));
                    self.i2c.push(1);
                } else {
                    self.next_free = self.i2c[i];
                    self.i2t[i] = Some(t);
                    self.i2c[i] = 1;
                }
                i as $uXX
            }

            fn get_term(&self, i: $uXX) -> Option<&Term<T>> {
                let i = i as usize;
                if i < self.i2t.len() {
                    self.i2t[i].as_ref()
                } else {
                    None
                }
            }

            fn inc_ref(&mut self, i: $uXX) {
                let i = i as usize;
                self.i2c[i] += 1;
            }

            fn dec_ref(&mut self, i: $uXX) {
                let i = i as usize;
                self.i2c[i] -= 1;
                if self.i2c[i] == 0 {
                    let t: Term<T> = self.i2t[i].take().unwrap();
                    self.t2i.remove(unsafe { &fake_static(&t) });
                    self.i2c[i] = self.next_free;
                    self.next_free = i as $uXX;
                }
            }

        }
    }
}

impl_term_index!(u16);
impl_term_index!(u32);



pub struct PairIndex<K, V> where K: Eq+Hash {
    map: HashMap<K, HashMap<K, V>>,
    len: usize,
}

impl<K, V> PairIndex<K, V> where K: Eq+Hash {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn map(&self) -> &HashMap<K, HashMap<K, V>> {
        &self.map
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<K, HashMap<K, V>> {
        self.map.iter()
    }

    #[inline]
    pub fn pairs<'a, I> (&'a self, ti: &'a I) -> impl Iterator<Item=(&Term<I::Holder>, &Term<I::Holder>, &V)>+'a where        
        K: Copy,
        I: TermIndex<Index=K>,
    {
        self.map.iter()
          .flat_map(move |(i1, subindex)| {
            let t1 = ti.get_term(*i1).unwrap();
            subindex.iter()
            .map(move |(i2, val)| (t1, ti.get_term(*i2).unwrap(), val))
          })
    }

    #[inline]
    pub fn get<T> (&self, t: &T) -> Option<&HashMap<K, V>> where
        K: Borrow<T>,
        T: Eq+Hash,
    {
        self.map.get(t)
    }

    pub fn insert(&mut self, t1: K, t2: K, val: V) -> Option<V> {
        let modified =
            self.map.entry(t1)
                    .or_insert_with(HashMap::new)
                    .insert(t2, val);
        if modified.is_none() { self.len += 1 }
        modified
    }

    pub fn remove(&mut self, t1: &K, t2: &K) -> Option<V> {
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

impl<K, V> Default for PairIndex<K, V> where K: Eq+Hash {
    fn default() -> Self {
        PairIndex {
            map: HashMap::default(),
            len: usize::default(),
        }
    }
}



pub struct TripleIndex<K, V> where K: Eq+Hash {
    map: HashMap<K, PairIndex<K, V>>,
    len: usize,
}

impl<K, V> TripleIndex<K, V> where K: Eq+Hash {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn map(&self) -> &HashMap<K, PairIndex<K, V>> {
        &self.map
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn get(&self, t: &K) -> Option<&PairIndex<K, V>> {
        self.map.get(t)
    }

    #[inline]
    pub fn iter(&self) -> hash_map::Iter<K, PairIndex<K, V>> {
        self.map.iter()
    }

    #[inline]
    pub fn triples<'a, I> (&'a self, ti: &'a I) -> impl Iterator<Item=(&Term<I::Holder>, &Term<I::Holder>, &Term<I::Holder>, &V)>+'a where        
        K: Copy,
        I: TermIndex<Index=K>,
    {
        self.map.iter()
        .flat_map(move |(i1, subindex)| {
            let t1 = ti.get_term(*i1).unwrap();
            subindex.pairs(ti)
            .map(move |(t2, t3, val)| (t1, t2, t3, val))
        })
    }

    pub fn insert(&mut self, t1: K, t2: K, t3: K, val: V) -> Option<V> {
        let modified =
            self.map.entry(t1)
                    .or_insert_with(PairIndex::new)
                    .insert(t2, t3, val);
        if modified.is_none() { self.len += 1 }
        modified
    }

    pub fn remove(&mut self, t1: &K, t2: &K, t3: &K) -> Option<V> {
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

impl<K, V> Default for TripleIndex<K, V> where K: Eq+Hash {
    fn default() -> Self {
        TripleIndex {
            map: HashMap::default(),
            len: usize::default(),
        }
    }
}



/// Unsafely converts a term into a StaticTerm.
/// This is to be used *only* when we can guarantee that the produced StaticTerm
/// will not outlive the source term.
/// We use this for keys in TermIndexXX::t2i, when the owning term is in TermIndexXX::i2t.
#[inline]
unsafe fn fake_static<S, T> (t: &T) -> StaticTerm where
    S: Borrow<str>,
    T: Borrow<Term<S>>,
{
    StaticTerm::from_with(t.borrow(), |txt| &*(txt as *const str))
}


#[test]
fn test_term_index() {
    let mut ti = TermIndexU::<u16, Box<str>>::default();
    assert_eq!(ti.next_free, 0);
    assert_eq!(ti.i2t.len(), 0);

    use ::ns::rdf;

    assert_eq!(ti.get_index(&rdf::subject), None);
    assert_eq!(ti.make_index(BoxTerm::from(&rdf::subject)), 0);
    assert_eq!(ti.get_index(&rdf::subject), Some(0));
    assert_eq!(ti.i2c[0], 1);
    assert_eq!(ti.next_free, 1);
    assert_eq!(ti.i2t.len(), 1);

    assert_eq!(ti.get_index(&rdf::predicate), None);
    assert_eq!(ti.make_index(BoxTerm::from(&rdf::predicate)), 1);
    assert_eq!(ti.get_index(&rdf::predicate), Some(1));
    assert_eq!(ti.i2c[1], 1);
    assert_eq!(ti.next_free, 2);
    assert_eq!(ti.i2t.len(), 2);

    assert_eq!(ti.get_index(&rdf::object), None);
    assert_eq!(ti.make_index(BoxTerm::from(&rdf::object)), 2);
    assert_eq!(ti.get_index(&rdf::object), Some(2));
    assert_eq!(ti.i2c[2], 1);
    assert_eq!(ti.next_free, 3);
    assert_eq!(ti.i2t.len(), 3);

    assert_eq!(ti.make_index(BoxTerm::from(&rdf::predicate)), 1);
    assert_eq!(ti.i2c[1], 2);

    ti.inc_ref(1);
    assert_eq!(ti.i2c[1], 3);

    ti.dec_ref(1);
    assert_eq!(ti.i2c[1], 2);
    assert_eq!(ti.next_free, 3);

    ti.dec_ref(1);
    assert_eq!(ti.i2c[1], 1);
    assert_eq!(ti.next_free, 3);

    ti.dec_ref(1);
    assert_eq!(ti.get_index(&rdf::predicate), None);
    assert_eq!(ti.next_free, 1);
    assert_eq!(ti.i2c[1], 3); // now the previous version of next_free

    ti.dec_ref(0);
    assert_eq!(ti.get_index(&rdf::subject), None);
    assert_eq!(ti.next_free, 0);
    assert_eq!(ti.i2c[0], 1); // now the previous version of next_free

    assert_eq!(ti.make_index(BoxTerm::from(&rdf::type_)), 0);
    assert_eq!(ti.i2c[0], 1);
    assert_eq!(ti.i2t.len(), 3);
    assert_eq!(ti.next_free, 1);

    // re-inserting rdf::subject, now ends up in a different place
    assert_eq!(ti.make_index(BoxTerm::from(&rdf::subject)), 1);
    assert_eq!(ti.i2c[1], 1);
    assert_eq!(ti.i2t.len(), 3);
    assert_eq!(ti.next_free, 3);

    assert_eq!(ti.make_index(BoxTerm::from(&rdf::Property)), 3);
    assert_eq!(ti.i2c[3], 1);
    assert_eq!(ti.i2t.len(), 4);
    assert_eq!(ti.next_free, 4);
}