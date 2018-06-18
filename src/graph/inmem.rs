// TODO Modular implementation of an in-memory Graph

use std::borrow::Borrow;
use std::iter::{empty, once};
use std::rc::Rc;

use super::*;
use ::term::*;
use ::term::factory::*;

#[derive(Default)]
pub struct SimpleGraph {
    factory: RcTermFactory,
    spo: index::TripleIndex<Rc<str>>,
}

impl SimpleGraph {
    pub fn new() -> SimpleGraph {
        SimpleGraph::default()
    }
}

impl traits::Graph for SimpleGraph {
    type Holder = Rc<str>;

    fn iter(&self) -> TripleIterator<Self::Holder> {
        Box::from(
            self.spo.iter().flat_map(move |(s,po)|
                po.iter().flat_map(move |(p,os)|
                os.iter().map(move |o|
                (s, p, o)))
            )
        )
    }
    fn iter_for_s<'a, T> (&'a self, s: &Term<T>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
    {
        let s0 = RcTerm::from(s);
        if let Some((s, po)) = self.spo.map().get_key_value(&s0) {
            Box::from(
                po.iter().flat_map(move |(p, os)|
                os.iter().map(move |o|
                (s, p, o)))
            )
        } else {
            Box::from(empty())
        }
    }
    fn iter_for_p<'a, T> (&'a self, p: &'a Term<T>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
    {
        let p0 = p;
        Box::from(
            self.spo.iter().flat_map(move |(s,po)|
                po.iter().filter(move |(p, _)| p==&p0).flat_map(move |(p,os)|
                os.iter().map(move |o|
                (s, p, o)))
            )
        )
    }
    fn iter_for_sp<'a, T, U> (&'a self, s: &'a Term<T>, p: &'a Term<U>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        let s0 = RcTerm::from(s);
        if let Some((s, po)) = self.spo.map().get_key_value(&s0) {
            let p0 = RcTerm::from(p);
            if let Some((p, os)) = po.map().get_key_value(&p0) {
                return Box::from(
                    os.iter().map(move |o|
                    (s, p, o))
                );
            }
        }
        Box::from(empty())
    }
    fn iter_for_spo<'a, T, U, V> (&'a self, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> TripleIterator<'a, Self::Holder> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        let s0 = RcTerm::from(s);
        if let Some((s, po)) = self.spo.map().get_key_value(&s0) {
            let p0 = RcTerm::from(p);
            if let Some((p, os)) = po.map().get_key_value(&p0) {
                let o0 = RcTerm::from(o);
                if let Some(o) = os.get(&o0) {
                    return Box::from(once((s, p, o)));
                }
            }
        }
        Box::from(empty())
    }

    fn len(&self) -> usize {
        self.spo.len()
    }

    fn hint(&self) -> (usize, Option<usize>) {
        let len = self.spo.len();
        (len, Some(len))
    }

    fn hint_for_s<T> (&self, s: &Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        let len = match self.spo.get(&RcTerm::from(s)) {
            None => 0,
            Some(subindex) => subindex.len(),
        };
        (len, Some(len))
    }

    fn hint_for_sp<T, U> (&self, s: &Term<T>, p: &Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        let len = match self.spo.get(&RcTerm::from(s)) {
            None => 0,
            Some(subindex) => match subindex.get(&RcTerm::from(p)) {
                None => 0,
                Some(subsubindex) => subsubindex.len()
            }
        };
        (len, Some(len))
    }
}

impl traits::MutableGraph for SimpleGraph {
    fn copy<T: Borrow<str>> (&mut self, t: &Term<T>) -> Term<Self::Holder> {
        self.factory.copy(t)
    }

    fn insert_as_is(&mut self, s: Term<Self::Holder>, p: Term<Self::Holder>, o: Term<Self::Holder>) -> bool {
        self.spo.insert(s, p, o)
    }

    fn remove_as_is(&mut self, s: &Term<Self::Holder>, p: &Term<Self::Holder>, o: &Term<Self::Holder>) -> bool {
        self.spo.remove(s, p, o)
    }

    fn remove<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> bool where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        traits::helper::remove_with_from(self, s, p, o)
    }
}