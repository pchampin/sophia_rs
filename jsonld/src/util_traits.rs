//! Utility traits used internally by JsonLdSerializer
use sophia::quad::Quad;
use sophia_term::{Term, TermData};
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

impl<TD: TermData> TermJsonLdUtil for Term<TD> {
    #[inline]
    fn is_subject(&self) -> bool {
        matches!(self, Term::Iri(_) | Term::BNode(_))
    }
    #[inline]
    fn is_object(&self) -> bool {
        matches!(self, Term::Iri(_) | Term::BNode(_) | Term::Literal(_))
    }
    #[inline]
    fn is_iri(&self) -> bool {
        matches!(self, Term::Iri(_))
    }
    #[inline]
    fn is_bnode(&self) -> bool {
        matches!(self, Term::BNode(_))
    }
    #[inline]
    fn is_literal(&self) -> bool {
        matches!(self, Term::Literal(_))
    }
    #[inline]
    fn as_id(&self) -> String {
        match self {
            Term::Iri(i) => i.value().into(),
            Term::BNode(b) => format!("{}", b),
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
