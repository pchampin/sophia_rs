//! A graph identifier is the fourth component of a quad.
//! It identifies the graph of a dataset to which this quad belongs.

use crate::term::{Term, TermData};

pub type GraphId<T> = Option<Term<T>>;

pub trait GraphNameExt {
    fn same_graph_name<U>(&self, other: &Option<Term<U>>) -> bool where U: TermData;
    fn convert_graph_name<'a, U>(&'a self) -> Option<Term<U>> where U: TermData + From<&'a str>;
}

/// An extension of `Option<Term>` to ease its uses as graph names in quads.
impl<T: TermData> GraphNameExt for Option<Term<T>> {
    fn same_graph_name<U>(&self, other: &Option<Term<U>>) -> bool
    where
        U: TermData,
    {
        match (self, other) {
            (Some(ts), Some(to)) => ts == to,
            (None, None) => true,
            _ => false,
        }
    }
    fn convert_graph_name<'a, U>(&'a self) -> Option<Term<U>>
    where
        U: TermData + From<&'a str>
    {
        self.as_ref().map(|n| n.into())
    }
}