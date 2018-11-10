//! Types for indexing terms.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::hash::Hash;

use ::graph::*;
use ::term::*;
use ::term::factory::TermFactory;

/// A bidirectionnal mapping between [`Term`](../../term/enum.Term.html)s
/// and a smaller type.
/// 
/// The TermIndex maintains a reference count for each index,
/// to automatically free them whenever they are not used.
/// 
pub trait TermIndex: Default {
    /// The type used to represent terms
    type Index: Copy+Eq;
    /// The factory used to instantiate terms.
    type Factory: TermFactory;

    /// Return the index associated to the given term, if it exists.
    fn get_index(&self, t: &RefTerm) -> Option<Self::Index>;
    /// Return the index associated to the given term, creating it if required.
    fn make_index(&mut self, t: &RefTerm) -> Self::Index;
    /// Return the term associated to the given index, if it exists.
    fn get_term(&self, i: Self::Index) -> Option<&Term<<Self::Factory as TermFactory>::Holder>>;
    /// Increase the reference count of a given index.
    fn inc_ref(&mut self, i: Self::Index);
    /// Decrease the reference count of a given index.
    fn dec_ref(&mut self, i: Self::Index);
    /// Shrinks the capacity of the TermIndex as much as possible.
    fn shrink_to_fit(&mut self);
}



/// A utility trait for implementing [`MutableGraph`]
/// based on an internal [`TermIndex`] for efficient storage.
/// 
/// The `impl_mutable_graph_for_indexed_mutable_graph!` macro
/// can be used to derive the `MutableGraph` implementation
/// for any implementation of `IndexedMutableGraph`.
/// 
/// [`MutableGraph`]: ../trait.MutableGraph.html
/// [`TermIndex`]: trait.TermIndex.html
/// 
pub trait IndexedMutableGraph: Graph {
    /// The type used to represent terms internally.
    type Index: Copy + Eq + Hash;

    /// Return the index for the given term, if it exists.
    fn get_index<T> (&self, t: &Term<T>) -> Option<Self::Index> where
        T: Borrow<str>,
    ;

    /// Return the term for the given index, if it exists.
    fn get_term(&self, i: Self::Index) -> Option<&Term<Self::Holder>>;

    /// Insert a triple in this Graph,
    /// and return the corresponding tuple of indices.
    fn insert_indexed<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Option<(Self::Index, Self::Index, Self::Index)> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    ;

    /// Remove a triple from this Graph,
    /// and return the corresponding tuple of indices.
    fn remove_indexed<T, U, V> (&mut self, s: &Term<T>, p: &Term<U>, o: &Term<V>) -> Option<(Self::Index, Self::Index, Self::Index)> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    ;

    fn shrink_to_fit(&mut self);
}

/// Defines the implementation of [`MutableGraph`] for [`IndexedMutableGraph`].
/// 
/// [`MutableGraph`]: ../trait.MutableGraph.html
/// [`MutableGraph`]: trait.IndexedMutableGraph.html
#[macro_export]
macro_rules! impl_mutable_graph_for_indexed_mutable_graph {
    ($indexed_mutable_graph: ty) => {
        impl MutableGraph for $indexed_mutable_graph {
            impl_mutable_graph_for_indexed_mutable_graph!();
        }
    };
    () => {
        fn insert<T_, U_, V_> (&mut self, s: &Term<T_>, p: &Term<U_>, o: &Term<V_>) -> GResult<Self, bool> where
            T_: Borrow<str>,
            U_: Borrow<str>,
            V_: Borrow<str>,
        {
            Ok(self.insert_indexed(s, p, o).is_some())
        }
        fn remove<T_, U_, V_> (&mut self, s: &Term<T_>, p: &Term<U_>, o: &Term<V_>) -> GResult<Self, bool> where
            T_: Borrow<str>,
            U_: Borrow<str>,
            V_: Borrow<str>,
        {
            Ok(self.remove_indexed(s, p, o).is_some())
        }        
    };
}




/// Remove *one* value in the Vec value of a HashMap,
/// removing the entry completely if the Vec ends up empty.
///
/// # Panics
/// 
/// This function will panic if either
/// * `k` is not a key of `hm`, or
/// * `w` is not contained in the value associated to `k`.
pub(crate) fn remove_one_val<K, W> (hm: &mut HashMap<K, Vec<W>>, k: K, w: W) where
    K: Eq + Hash,
    W: Copy + Eq,
{
    match hm.entry(k) {
        Entry::Occupied(mut e) => {
            {
                let ws = e.get_mut();
                if ws.len() > 1 {
                    let wi = ws.iter().enumerate()
                        .filter_map(|(i, w2)| if *w2 == w { Some(i) } else { None })
                        .next().unwrap();
                    ws.swap_remove(wi);
                    return;
                }
            }
            e.remove_entry();
        }
        Entry::Vacant(_) => unreachable!()
    }
}
