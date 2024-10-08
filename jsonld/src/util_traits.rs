//! Utility traits used internally by `JsonLdSerializer`
use sophia_api::quad::Quad;
use sophia_api::term::{
    Term,
    TermKind::{BlankNode, Iri, Literal},
};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;

pub trait TermJsonLdUtil {
    fn is_subject(&self) -> bool;
    fn is_object(&self) -> bool;
    fn is_bnode(&self) -> bool;
    fn as_id(&self) -> Box<str>;
}

impl<T: Term + ?Sized> TermJsonLdUtil for T {
    #[inline]
    fn is_subject(&self) -> bool {
        matches!(self.kind(), Iri | BlankNode)
    }
    #[inline]
    fn is_object(&self) -> bool {
        matches!(self.kind(), Iri | BlankNode | Literal)
    }
    #[inline]
    fn is_bnode(&self) -> bool {
        matches!(self.kind(), BlankNode)
    }
    #[inline]
    fn as_id(&self) -> Box<str> {
        match self.kind() {
            Iri => self.iri().unwrap().unwrap().into(),
            BlankNode => format!("_:{}", &self.bnode_id().unwrap().as_str()).into(),
            _ => panic!("not a subject term"),
        }
    }
}

pub trait QuadJsonLdUtil {
    fn is_jsonld(&self) -> bool;
}

impl<Q: Quad> QuadJsonLdUtil for Q {
    #[inline]
    fn is_jsonld(&self) -> bool {
        self.s().is_subject()
            && self.p().is_iri()
            && self.o().is_object()
            && self.g().map_or(true, |g| g.is_subject())
    }
}

pub trait VecUtil<T> {
    fn push_if_new(&mut self, val: T);
}

impl<T, U: From<T> + PartialEq> VecUtil<T> for Vec<U> {
    fn push_if_new(&mut self, val: T) {
        let val = val.into();
        if !self.contains(&val) {
            self.push(val);
        }
    }
}

pub trait HashMapUtil<T> {
    fn push_if_new<U: Into<Box<str>>>(&mut self, key: U, val: T);
}

impl<T, U: From<T> + PartialEq> HashMapUtil<T> for HashMap<Box<str>, Vec<U>> {
    fn push_if_new<V: Into<Box<str>>>(&mut self, key: V, val: T) {
        match self.entry(key.into()) {
            Occupied(e) => {
                e.into_mut().push_if_new(val);
            }
            Vacant(e) => {
                e.insert(vec![val.into()]);
            }
        };
    }
}
