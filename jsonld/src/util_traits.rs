//! Utility traits used internally by JsonLdSerializer
use sophia_api::quad::Quad;
use sophia_api::term::{TTerm, TermKind::*};
use std::collections::hash_map::Entry::*;
use std::collections::HashMap;

pub trait TermJsonLdUtil {
    fn is_subject(&self) -> bool;
    fn is_object(&self) -> bool;
    fn is_iri(&self) -> bool;
    fn is_bnode(&self) -> bool;
    fn is_literal(&self) -> bool;
    fn as_id(&self) -> String;
}

impl<T: TTerm + ?Sized> TermJsonLdUtil for T {
    #[inline]
    fn is_subject(&self) -> bool {
        matches!(self.kind(), Iri | BlankNode)
    }
    #[inline]
    fn is_object(&self) -> bool {
        matches!(self.kind(), Iri | BlankNode | Literal)
    }
    #[inline]
    fn is_iri(&self) -> bool {
        matches!(self.kind(), Iri)
    }
    #[inline]
    fn is_bnode(&self) -> bool {
        matches!(self.kind(), BlankNode)
    }
    #[inline]
    fn is_literal(&self) -> bool {
        matches!(self.kind(), Literal)
    }
    #[inline]
    fn as_id(&self) -> String {
        match self.kind() {
            Iri => self.value().into(),
            BlankNode => format!("_:{}", self.value_raw().0),
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
            && self.g().map(|g| g.is_subject()).unwrap_or(true)
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
    fn push_if_new(&mut self, key: String, val: T);
}

impl<T, U: From<T> + PartialEq> HashMapUtil<T> for HashMap<String, Vec<U>> {
    fn push_if_new(&mut self, key: String, val: T) {
        let vals = match self.entry(key) {
            Vacant(e) => e.insert(Vec::new()),
            Occupied(e) => e.into_mut(),
        };
        vals.push_if_new(val);
    }
}
