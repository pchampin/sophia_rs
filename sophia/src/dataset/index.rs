//! Types for indexing terms.

use std::hash::Hash;

use crate::term::graph_id::*;
use crate::term::*;

/// A utility trait for implementing [`Dataset`] and [`MutableDataset`]
/// based on an internal [`TermIndexMap`] for efficient storage.
///
/// The `impl_mutable_dataset_for_indexed_mutable_dataset!` macro
/// can be used to derive the `MutableDataset` implementation
/// for any implementation of `IndexedDataset`.
///
/// [`Dataset`]: ../trait.Dataset.html
/// [`MutableDataset`]: ../trait.MutableDataset.html
/// [`TermIndexMap`]: ../../graph/index/trait.TermIndexMap.html
///
pub trait IndexedDataset {
    /// The type used to represent terms internally.
    type Index: Copy + Eq + Hash;
    type TermData: TermData + 'static;

    /// Return the index for the given term, if it exists.
    fn get_index<T>(&self, t: &Term<T>) -> Option<Self::Index>
    where
        T: TermData;

    /// Return the index for the given graph identifier, if it exists.
    fn get_index_for_graph_id<T>(&self, g: &GraphId<T>) -> Option<Self::Index>
    where
        T: TermData;

    /// Return the term for the given index, if it exists.
    fn get_term(&self, i: Self::Index) -> Option<&Term<Self::TermData>>;

    /// Return the graph identifier for the given index, if it exists
    fn get_graph_id(&self, i: Self::Index) -> Option<&GraphId<Self::TermData>>;

    /// Insert a triple in this Dataset,
    /// and return the corresponding tuple of indices.
    fn insert_indexed<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphId<W>,
    ) -> Option<[Self::Index; 4]>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData;

    /// Remove a triple from this Dataset,
    /// and return the corresponding tuple of indices.
    fn remove_indexed<T, U, V, W>(
        &mut self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: &GraphId<W>,
    ) -> Option<[Self::Index; 4]>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData;

    fn shrink_to_fit(&mut self);
}

/// Defines the implementation of [`MutableDataset`] for [`IndexedDataset`].
///
/// [`MutableDataset`]: dataset/trait.MutableDataset.html
/// [`IndexedDataset`]: dataset/index/trait.IndexedDataset.html
#[macro_export]
macro_rules! impl_mutable_dataset_for_indexed_mutable_dataset {
    ($indexed_mutable_dataset: ty) => {
        impl MutableDataset for $indexed_mutable_dataset {
            impl_mutable_dataset_for_indexed_mutable_dataset!();
        }
    };
    () => {
        type MutationError = coercible_errors::Never;

        fn insert<T_, U_, V_, W_> (&mut self, s: &Term<T_>, p: &Term<U_>, o: &Term<V_>, g: &GraphId<W_>) -> MDResult< Self, bool> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
            W_: $crate::term::TermData,
        {
            Ok(self.insert_indexed(s, p, o, g).is_some())
        }
        fn remove<T_, U_, V_, W_> (&mut self, s: &Term<T_>, p: &Term<U_>, o: &Term<V_>, g: &GraphId<W_>) -> MDResult< Self, bool> where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
            W_: $crate::term::TermData,
        {
            Ok(self.remove_indexed(s, p, o, g).is_some())
        }
    };
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
