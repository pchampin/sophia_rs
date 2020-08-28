//! A utility trait for building datasets using indexed terms.

use std::hash::Hash;

use sophia_api::term::TTerm;
use sophia_term::*;

/// A utility trait for implementing [`Dataset`] and [`MutableDataset`]
/// based on an internal [`TermIndexMap`] for efficient storage.
///
/// The [`impl_mutable_dataset_for_indexed_dataset!`] macro
/// can be used to derive the `MutableDataset` implementation
/// for any implementation of `IndexedDataset`.
///
/// [`Dataset`]: ../trait.Dataset.html
/// [`MutableDataset`]: ../trait.MutableDataset.html
/// [`TermIndexMap`]: ../../term/index_map/trait.TermIndexMap.html
/// [`impl_mutable_dataset_for_indexed_dataset!`]: ../../macro.impl_mutable_dataset_for_indexed_dataset.html
///
pub trait IndexedDataset {
    /// The type used to represent terms internally.
    type Index: Copy + Eq + Hash;

    /// The type used to hold the term data.
    type TermData: TermData + 'static;

    /// Construct a new empty dataset, provisioning for storing `capacity` quads.
    fn with_capacity(capacity: usize) -> Self;

    /// Shrink the memory consumption of the dataset as much as possible.
    fn shrink_to_fit(&mut self);

    /// Return the index for the given term, if it exists.
    fn get_index<T>(&self, t: &T) -> Option<Self::Index>
    where
        T: TTerm + ?Sized;

    /// Return the index for the given graph name, if it exists.
    fn get_index_for_graph_name<T>(&self, g: Option<&T>) -> Option<Self::Index>
    where
        T: TTerm + ?Sized;

    /// Return the term for the given index, if it exists.
    fn get_term(&self, i: Self::Index) -> Option<&Term<Self::TermData>>;

    /// Return the graph name for the given index, if it exists
    ///
    /// NB: a graph name is already an `Option`, `None` meaning the (unnamed) default graph.
    /// As a consequence, this methods returns *an option of option* :
    /// * `None` means that given index is *not* associated to any graph name,
    /// * `Some(None)` means that the given index is associated to the default graph,
    /// * `Some(Some(term))` means that given index is associated to a proper graph name.
    #[allow(clippy::option_option)]
    fn get_graph_name(&self, i: Self::Index) -> Option<Option<&Term<Self::TermData>>>;

    /// Insert a triple in this Dataset,
    /// and return the corresponding tuple of indices.
    fn insert_indexed<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> Option<[Self::Index; 4]>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized;

    /// Remove a triple from this Dataset,
    /// and return the corresponding tuple of indices.
    fn remove_indexed<TS, TP, TO, TG>(
        &mut self,
        s: &TS,
        p: &TP,
        o: &TO,
        g: Option<&TG>,
    ) -> Option<[Self::Index; 4]>
    where
        TS: TTerm + ?Sized,
        TP: TTerm + ?Sized,
        TO: TTerm + ?Sized,
        TG: TTerm + ?Sized;
}

/// Defines the implementation of [`CollectibleDataset`] for [`IndexedDataset`].
///
/// [`CollectibleDataset`]: dataset/trait.CollectibleDataset.html
/// [`IndexedDataset`]: dataset/indexed/trait.IndexedDataset.html
#[macro_export]
macro_rules! impl_collectible_dataset_for_indexed_dataset {
    ($indexed_mutable_dataset: ty) => {
        impl CollectibleDataset for $indexed_mutable_dataset {
            impl_collectible_dataset_for_indexed_dataset!();
        }
    };
    () => {
        fn from_quad_source<QS: $crate::quad::stream::QuadSource>(
            mut quads: QS,
        ) -> $crate::quad::stream::StreamResult<Self, QS::Error, Self::Error> {
            use $crate::quad::Quad;
            let (tmin, tmax) = quads.size_hint_quads();
            let cap = tmax.unwrap_or(tmin);
            let mut g = Self::with_capacity(cap);
            quads
                .try_for_each_quad(|q| -> Result<(), Self::Error> {
                    g.insert_indexed(q.s(), q.p(), q.o(), q.g());
                    Ok(())
                })
                .map(|_| g)
        }
    };
}

/// Defines the implementation of [`MutableDataset`] for [`IndexedDataset`].
///
/// [`MutableDataset`]: dataset/trait.MutableDataset.html
/// [`IndexedDataset`]: dataset/indexed/trait.IndexedDataset.html
#[macro_export]
macro_rules! impl_mutable_dataset_for_indexed_dataset {
    ($indexed_mutable_dataset: ty) => {
        impl MutableDataset for $indexed_mutable_dataset {
            impl_mutable_dataset_for_indexed_dataset!();
        }
    };
    () => {
        type MutationError = std::convert::Infallible;

        fn insert<TS_, TP_, TO_, TG_>(
            &mut self,
            s: &TS_,
            p: &TP_,
            o: &TO_,
            g: Option<&TG_>,
        ) -> $crate::dataset::MDResult<Self, bool>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TP_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
            TG_: sophia_api::term::TTerm + ?Sized,
        {
            Ok(self.insert_indexed(s, p, o, g).is_some())
        }
        fn remove<TS_, TP_, TO_, TG_>(
            &mut self,
            s: &TS_,
            p: &TP_,
            o: &TO_,
            g: Option<&TG_>,
        ) -> $crate::dataset::MDResult<Self, bool>
        where
            TS_: sophia_api::term::TTerm + ?Sized,
            TP_: sophia_api::term::TTerm + ?Sized,
            TO_: sophia_api::term::TTerm + ?Sized,
            TG_: sophia_api::term::TTerm + ?Sized,
        {
            Ok(self.remove_indexed(s, p, o, g).is_some())
        }
    };
}

#[cfg(test)]
mod test {
    // Nothing really worth testing here
}
