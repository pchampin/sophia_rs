//! A utility trait for building graphs using indexed terms.

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

use crate::term::*;

/// A utility trait for implementing [`Graph`] and [`MutableGraph`]
/// based on an internal [`TermIndexMap`] for efficient storage.
///
/// The [`impl_mutable_graph_for_indexed_graph!`] macro
/// can be used to derive the `MutableGraph` implementation
/// for any implementation of `IndexedGraph`.
///
/// [`Graph`]: ../trait.Graph.html
/// [`MutableGraph`]: ../trait.MutableGraph.html
/// [`TermIndexMap`]: ../../term/index_map/trait.TermIndexMap.html
/// [`impl_mutable_graph_for_indexed_graph!`]: ../../macro.impl_mutable_graph_for_indexed_graph.html
///
pub trait IndexedGraph {
    /// The type used to represent terms internally.
    type Index: Copy + Eq + Hash;
    type TermData: TermData + 'static;

    /// Return the index for the given term, if it exists.
    fn get_index<T>(&self, t: &Term<T>) -> Option<Self::Index>
    where
        T: TermData;

    /// Return the term for the given index, if it exists.
    fn get_term(&self, i: Self::Index) -> Option<&Term<Self::TermData>>;

    /// Insert a triple in this Graph,
    /// and return the corresponding tuple of indices.
    fn insert_indexed<T, U, V>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
    ) -> Option<[Self::Index; 3]>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    /// Remove a triple from this Graph,
    /// and return the corresponding tuple of indices.
    fn remove_indexed<T, U, V>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
    ) -> Option<[Self::Index; 3]>
    where
        T: TermData,
        U: TermData,
        V: TermData;

    fn shrink_to_fit(&mut self);
}

/// Defines the implementation of [`MutableGraph`] for [`IndexedGraph`].
///
/// [`MutableGraph`]: graph/trait.MutableGraph.html
/// [`IndexedGraph`]: graph/indexed/trait.IndexedGraph.html
#[macro_export]
macro_rules! impl_mutable_graph_for_indexed_graph {
    ($indexed_mutable_graph: ty) => {
        impl MutableGraph for $indexed_mutable_graph {
            impl_mutable_graph_for_indexed_graph!();
        }
    };
    () => {
        type MutationError = $crate::error::Infallible;

        fn insert<T_, U_, V_> (&mut self, s: &Term<T_>, p: &Term<U_>, o: &Term<V_>) -> Result<bool, Self::MutationError> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            Ok(self.insert_indexed(s, p, o).is_some())
        }
        fn remove<T_, U_, V_> (&mut self, s: &Term<T_>, p: &Term<U_>, o: &Term<V_>) -> Result<bool, Self::MutationError> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            Ok(self.remove_indexed(s, p, o).is_some())
        }
    };
}

/// Insert an absent value in the Vec value of a HashMap,
/// creating the Vec if it does not exist.
///
/// # Returns
///
/// `true` if the Vec was created,
///  meaning that "parent" indexes need to be updated.
///
pub(crate) fn insert_in_index<K, W>(hm: &mut HashMap<K, Vec<W>>, k: K, w: W) -> bool
where
    K: Eq + Hash,
    W: Copy + Eq,
{
    let mut ret = false;
    hm.entry(k)
        .or_insert_with(|| {
            ret = true;
            Vec::new()
        })
        .push(w);
    ret
}

/// Remove an existing value in the Vec value of a HashMap,
/// removing the entry completely if the Vec ends up empty.
///
/// # Returns
///
/// `true` if the entry was removed,
///  meaning that "parent" indexes need to be updated.
///
/// # Panics
///
/// This function will panic if either
/// * `k` is not a key of `hm`, or
/// * `w` is not contained in the value associated to `k`.
pub(crate) fn remove_from_index<K, W>(hm: &mut HashMap<K, Vec<W>>, k: K, w: W) -> bool
where
    K: Eq + Hash,
    W: Copy + Eq,
{
    match hm.entry(k) {
        Entry::Occupied(mut e) => {
            {
                let ws = e.get_mut();
                if ws.len() > 1 {
                    let wi = ws
                        .iter()
                        .enumerate()
                        .filter_map(|(i, w2)| if *w2 == w { Some(i) } else { None })
                        .next()
                        .unwrap();
                    ws.swap_remove(wi);
                    return false;
                }
            }
            e.remove_entry();
            true
        }
        Entry::Vacant(_) => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
