// this module is transparently re-exported by its parent `term`

use crate::term::{Term, TermData};

/// Type alias for conveniently handling graph names in quad.
pub type GraphName<T> = Option<Term<T>>;

/// An extension of `Option<_>` to ease its uses as graph names in quads.
pub trait GraphNameExt {
    fn same_graph_name<U>(&self, other: &Option<Term<U>>) -> bool where U: TermData;
    fn convert_graph_name<'a, U>(&'a self) -> Option<Term<U>> where U: TermData + From<&'a str>;
}

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