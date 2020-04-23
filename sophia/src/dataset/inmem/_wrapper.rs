// this module is transparently re-exported by its parent `dataset::inmem`

use super::*;
use crate::dataset::indexed::IndexedDataset;

/// A dataset wrapper wraps a [`Dataset`] and overrides some of its methods.
///
/// This trait mimmics the interface of the [`Dataset`] trait,
/// with all methods having a default implementation
/// that delegates to the corresponding method of the wrapped dataset.
/// Implementation of this trait may however expected to override
/// *some* of the methods.
///
/// Conversely, the [`impl_dataset_for_wrapper!`] macro can be used to derive
/// the Dataset implementation for any implementation of DatasetWrapper.
///
/// [`Dataset`]: ../trait.Dataset.html
/// [`impl_dataset_for_wrapper!`]: ../../macro.impl_dataset_for_wrapper.html
pub trait DatasetWrapper {
    /// The type of the wrapped dataset.
    type Wrapped: Dataset;

    /// Borrow the wrapped dataset.
    fn get_wrapped(&self) -> &Self::Wrapped;

    /// Borrow the wrapped dataset mutably.
    fn get_wrapped_mut(&mut self) -> &mut Self::Wrapped;

    #[inline]
    /// Mimmic the [`iter`](../trait.Dataset.html#tymethod.iter) method.
    fn dw_quads(&self) -> DQuadSource<Self::Wrapped> {
        self.get_wrapped().quads()
    }

    #[inline]
    /// Mimmic the [`quads_with_s`](../trait.Dataset.html#method.quads_with_s) method.
    fn dw_quads_with_s<'s, T>(&'s self, s: &'s Term<T>) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_s(s)
    }
    #[inline]
    /// Mimmic the [`quads_with_p`](../trait.Dataset.html#method.quads_with_p) method.
    fn dw_quads_with_p<'s, T>(&'s self, p: &'s Term<T>) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_p(p)
    }
    #[inline]
    /// Mimmic the [`quads_with_o`](../trait.Dataset.html#method.quads_with_o) method.
    fn dw_quads_with_o<'s, T>(&'s self, o: &'s Term<T>) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_o(o)
    }
    #[inline]
    /// Mimmic the [`quads_with_g`](../trait.Dataset.html#method.quads_with_g) method.
    fn dw_quads_with_g<'s, T>(&'s self, g: Option<&'s Term<T>>) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_g(g)
    }
    #[inline]
    /// Mimmic the [`quads_with_sp`](../trait.Dataset.html#method.quads_with_sp) method.
    fn dw_quads_with_sp<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_sp(s, p)
    }
    #[inline]
    /// Mimmic the [`quads_with_so`](../trait.Dataset.html#method.quads_with_so) method.
    fn dw_quads_with_so<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        o: &'s Term<U>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_so(s, o)
    }
    #[inline]
    /// Mimmic the [`quads_with_sg`](../trait.Dataset.html#method.quads_with_sg) method.
    fn dw_quads_with_sg<'s, T, U>(
        &'s self,
        s: &'s Term<T>,
        g: Option<&'s Term<U>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_sg(s, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_po`](../trait.Dataset.html#method.quads_with_po) method.
    fn dw_quads_with_po<'s, T, U>(
        &'s self,
        p: &'s Term<T>,
        o: &'s Term<U>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_po(p, o)
    }
    #[inline]
    /// Mimmic the [`quads_with_pg`](../trait.Dataset.html#method.quads_with_pg) method.
    fn dw_quads_with_pg<'s, T, U>(
        &'s self,
        p: &'s Term<T>,
        g: Option<&'s Term<U>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_pg(p, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_og`](../trait.Dataset.html#method.quads_with_og) method.
    fn dw_quads_with_og<'s, T, U>(
        &'s self,
        o: &'s Term<T>,
        g: Option<&'s Term<U>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_og(o, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_spo`](../trait.Dataset.html#method.quads_with_spo) method.
    fn dw_quads_with_spo<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        o: &'s Term<V>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_spo(s, p, o)
    }
    #[inline]
    /// Mimmic the [`quads_with_spg`](../trait.Dataset.html#method.quads_with_spg) method.
    fn dw_quads_with_spg<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        g: Option<&'s Term<V>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_spg(s, p, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_sog`](../trait.Dataset.html#method.quads_with_sog) method.
    fn dw_quads_with_sog<'s, T, U, V>(
        &'s self,
        s: &'s Term<T>,
        o: &'s Term<U>,
        g: Option<&'s Term<V>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_sog(s, o, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_pog`](../trait.Dataset.html#method.quads_with_pog) method.
    fn dw_quads_with_pog<'s, T, U, V>(
        &'s self,
        p: &'s Term<T>,
        o: &'s Term<U>,
        g: Option<&'s Term<V>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_pog(p, o, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_spog`](../trait.Dataset.html#method.quads_with_spog) method.
    fn dw_quads_with_spog<'s, T, U, V, W>(
        &'s self,
        s: &'s Term<T>,
        p: &'s Term<U>,
        o: &'s Term<V>,
        g: Option<&'s Term<W>>,
    ) -> DQuadSource<'s, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        self.get_wrapped().quads_with_spog(s, p, o, g)
    }

    #[inline]
    /// Mimmic the [`contains`](../trait.Dataset.html#method.contains) method.
    fn dw_contains<T, U, V, W>(
        &self,
        s: &Term<T>,
        p: &Term<U>,
        o: &Term<V>,
        g: Option<&Term<W>>,
    ) -> DResult<Self::Wrapped, bool>
    where
        T: TermData,
        U: TermData,
        V: TermData,
        W: TermData,
    {
        self.get_wrapped().contains(s, p, o, g)
    }

    #[inline]
    /// Mimmic the [`subjects`](../trait.Dataset.html#method.subjects) method.
    fn dw_subjects(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().subjects()
    }

    #[inline]
    /// Mimmic the [`predicates`](../trait.Dataset.html#method.predicates) method.
    fn dw_predicates(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().predicates()
    }

    #[inline]
    /// Mimmic the [`objects`](../trait.Dataset.html#method.objects) method.
    fn dw_objects(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().objects()
    }

    #[inline]
    /// Mimmic the [`graph_names`](../trait.Dataset.html#method.graph_names) method.
    fn dw_graph_names(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().graph_names()
    }

    #[inline]
    /// Mimmic the [`iris`](../trait.Dataset.html#method.iris) method.
    fn dw_iris(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().iris()
    }

    #[inline]
    /// Mimmic the [`bnodes`](../trait.Dataset.html#method.bnodes) method.
    fn dw_bnodes(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().bnodes()
    }

    #[inline]
    /// Mimmic the [`literals`](../trait.Dataset.html#method.literals) method.
    fn dw_literals(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().literals()
    }

    #[inline]
    /// Mimmic the [`variables`](../trait.Dataset.html#method.variables) method.
    fn dw_variables(&self) -> DResultTermSet<Self::Wrapped> {
        self.get_wrapped().variables()
    }
}

/// Defines the implementation of [`Dataset`] for [`DatasetWrapper`].
///
/// [`Dataset`]: dataset/trait.Dataset.html
/// [`DatasetWrapper`]: dataset/inmem/trait.DatasetWrapper.html
#[macro_export]
macro_rules! impl_dataset_for_wrapper {
    ($wrapper: ty) => {
        impl<'a> $crate::dataset::Dataset<'a> for $wrapper {
            impl_dataset_for_wrapper!();
        }
    };
    () => {
        type Quad = <<Self as $crate::dataset::inmem::DatasetWrapper>::Wrapped as $crate::dataset::Dataset>::Quad;
        type Error = <<Self as $crate::dataset::inmem::DatasetWrapper>::Wrapped as $crate::dataset::Dataset>::Error;

        #[inline]
        fn quads(&self) -> $crate::dataset::DQuadSource<Self> {
            DatasetWrapper::dw_quads(self)
        }
        #[inline]
        fn quads_with_s<'s_, T_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_s(self, s)
        }
        #[inline]
        fn quads_with_p<'s_, T_>(
            &'s_ self,
            p: &'s_ sophia_term::Term<T_>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_p(self, p)
        }
        #[inline]
        fn quads_with_o<'s_, T_>(
            &'s_ self,
            o: &'s_ sophia_term::Term<T_>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
        {

            DatasetWrapper::dw_quads_with_o(self, o)
        }
        #[inline]
        fn quads_with_g<'s_, T_>(
            &'s_ self,
            g: std::option::Option<&'s_ sophia_term::Term<T_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_g(self, g)
        }
        #[inline]
        fn quads_with_sp<'s_, T_, U_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            p: &'s_ sophia_term::Term<U_>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_sp(self, s, p)
        }
        #[inline]
        fn quads_with_so<'s_, T_, U_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            o: &'s_ sophia_term::Term<U_>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_so(self, s, o)
        }
        #[inline]
        fn quads_with_sg<'s_, T_, U_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            g: std::option::Option<&'s_ sophia_term::Term<U_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_sg(self, s, g)
        }
        #[inline]
        fn quads_with_po<'s_, T_, U_>(
            &'s_ self,
            p: &'s_ sophia_term::Term<T_>,
            o: &'s_ sophia_term::Term<U_>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_po(self, p, o)
        }
        #[inline]
        fn quads_with_pg<'s_, T_, U_>(
            &'s_ self,
            p: &'s_ sophia_term::Term<T_>,
            g: std::option::Option<&'s_ sophia_term::Term<U_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_pg(self, p, g)
        }
        #[inline]
        fn quads_with_og<'s_, T_, U_>(
            &'s_ self,
            o: &'s_ sophia_term::Term<T_>,
            g: std::option::Option<&'s_ sophia_term::Term<U_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_og(self, o, g)
        }
        #[inline]
        fn quads_with_spo<'s_, T_, U_, V_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            p: &'s_ sophia_term::Term<U_>,
            o: &'s_ sophia_term::Term<V_>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_spo(self, s, p, o)
        }
        #[inline]
        fn quads_with_spg<'s_, T_, U_, V_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            p: &'s_ sophia_term::Term<U_>,
            g: std::option::Option<&'s_ sophia_term::Term<V_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_spg(self, s, p, g)
        }
        #[inline]
        fn quads_with_sog<'s_, T_, U_, V_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            o: &'s_ sophia_term::Term<U_>,
            g: std::option::Option<&'s_ sophia_term::Term<V_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_sog(self, s, o, g)
        }
        #[inline]
        fn quads_with_pog<'s_, T_, U_, V_>(
            &'s_ self,
            p: &'s_ sophia_term::Term<T_>,
            o: &'s_ sophia_term::Term<U_>,
            g: std::option::Option<&'s_ sophia_term::Term<V_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_pog(self, p, o, g)
        }
        #[inline]
        fn quads_with_spog<'s_, T_, U_, V_, W_>(
            &'s_ self,
            s: &'s_ sophia_term::Term<T_>,
            p: &'s_ sophia_term::Term<U_>,
            o: &'s_ sophia_term::Term<V_>,
            g: std::option::Option<&'s_ sophia_term::Term<W_>>
        ) -> $crate::dataset::DQuadSource<'s_, Self>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
            W_: sophia_term::TermData,
        {
            DatasetWrapper::dw_quads_with_spog(self, s, p, o, g)
        }

        #[inline]
        fn contains<T_, U_, V_, W_>(
            & self,
            s: &sophia_term::Term<T_>,
            p: &sophia_term::Term<U_>,
            o: &sophia_term::Term<V_>,
            g: std::option::Option<&sophia_term::Term<W_>>
        ) -> $crate::dataset::DResult<Self, bool>
        where
            T_: sophia_term::TermData,
            U_: sophia_term::TermData,
            V_: sophia_term::TermData,
            W_: sophia_term::TermData,
        {
            DatasetWrapper::dw_contains(self, s, p, o, g)
        }

        #[inline]
        fn subjects(&self) -> $crate::dataset::DResultTermSet<Self> {
            DatasetWrapper::dw_subjects(self)
        }

        #[inline]
        fn predicates(&self) -> $crate::dataset::DResultTermSet<Self> {
            DatasetWrapper::dw_predicates(self)
        }

        #[inline]
        fn objects(&self) -> $crate::dataset::DResultTermSet<Self> {
            DatasetWrapper::dw_objects(self)
        }

        #[inline]
        fn iris(&self) -> $crate::dataset::DResultTermSet<Self> {
            DatasetWrapper::dw_iris(self)
        }

        #[inline]
        fn bnodes(&self) -> $crate::dataset::DResultTermSet<Self> {
            DatasetWrapper::dw_bnodes(self)
        }

        #[inline]
        fn literals(&self) -> $crate::dataset::DResultTermSet<Self> {
            DatasetWrapper::dw_literals(self)
        }

        #[inline]
        fn variables(&self) -> $crate::dataset::DResultTermSet<Self> {
            DatasetWrapper::dw_variables(self)
        }
    };
}

/// An indexed dataset wrapper wraps an [`IndexedDataset`] and augments some of its methods,
/// through hooks of the forme `before_x` and `after_x`.
///
/// This trait is designed to add mutability to [`DatasetWrapper`],
/// through the [`impl_indexed_dataset_for_wrapper!`] macro.
///
/// [`IndexedDataset`]: ../indexed/trait.IndexedDataset.html
/// [`DatasetWrapper`]: ./trait.DatasetWrapper.html
/// [`impl_indexed_dataset_for_wrapper!`]: ../../macro.impl_indexed_dataset_for_wrapper.html
pub trait IndexedDatasetWrapper<T>
where
    T: IndexedDataset,
{
    /// Wrap the given dataset.
    ///
    /// # Safety
    /// This method requires that the given dataset is empty.
    unsafe fn idw_wrap_empty(dataset: T) -> Self;

    /// Hook to be executed at the end of
    /// [`IndexedDataset::insert_indexed`](../indexed/trait.IndexedDataset.html#tymethod.insert_indexed).
    fn idw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 4]>);

    /// Hook to be executed at the end of
    /// [`IndexedDataset::remove_indexed`](../indexed/trait.IndexedDataset.html#tymethod.remove_indexed).
    fn idw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 4]>);

    /// Hook to be executed at the end of
    /// [`IndexedDataset::shrink_to_fit`](../indexed/trait.IndexedDataset.html#tymethod.shrink_to_fit).
    fn idw_hook_shrink_to_fit(&mut self);
}

/// Defines the implementation of [`IndexedDataset`] for [`DatasetWrapper`] around another [`IndexedDataset`].
///
/// [`IndexedDataset`]: dataset/indexed/trait.IndexedDataset.html
/// [`DatasetWrapper`]: dataset/inmem/trait.DatasetWrapper.html
#[macro_export]
macro_rules! impl_indexed_dataset_for_wrapper {
    () => {
        type Index = T::Index;
        type TermData = T::TermData;

        #[inline]
        fn with_capacity(capacity: usize) -> Self {
            unsafe {
                Self::idw_wrap_empty(T::with_capacity(capacity))
            }
        }

        #[inline]
        fn shrink_to_fit(&mut self) {
            self.get_wrapped_mut().shrink_to_fit();
            self.idw_hook_shrink_to_fit();
        }

        #[inline]
        fn get_index<U>(&self, t: &sophia_term::Term<U>) -> Option<Self::Index>
        where
            U: sophia_term::TermData,
        {
            self.get_wrapped().get_index(t)
        }

        #[inline]
        fn get_index_for_graph_name<U>(
            &self,
            g: std::option::Option<&'_ sophia_term::Term<U>>
        ) -> Option<Self::Index>
        where
            U: sophia_term::TermData,
        {
            self.get_wrapped().get_index_for_graph_name(g)
        }

        #[inline]
        fn get_term(& self, i: Self::Index) -> Option<&Term<Self::TermData>> {
            self.get_wrapped().get_term(i)
        }

        #[inline]
        fn get_graph_name(&self, i: Self::Index) -> Option<Option<&Term<Self::TermData>>> {
            self.get_wrapped().get_graph_name(i)
        }

        fn insert_indexed<U, V, W, X>(
            &mut self,
            s: &sophia_term::Term<U>,
            p: &sophia_term::Term<V>,
            o: &sophia_term::Term<W>,
            g: std::option::Option<&'_ sophia_term::Term<X>>,
        ) -> Option<[Self::Index; 4]>
        where
            U: sophia_term::TermData,
            V: sophia_term::TermData,
            W: sophia_term::TermData,
            X: sophia_term::TermData,
        {
            let modified = self.get_wrapped_mut().insert_indexed(s, p, o, g);
            self.idw_hook_insert_indexed(&modified);
            modified
        }

        fn remove_indexed<U, V, W, X>(
            &mut self,
            s: &sophia_term::Term<U>,
            p: &sophia_term::Term<V>,
            o: &sophia_term::Term<W>,
            g: std::option::Option<&'_ sophia_term::Term<X>>
        ) -> Option<[Self::Index; 4]>
        where
            U: sophia_term::TermData,
            V: sophia_term::TermData,
            W: sophia_term::TermData,
            X: sophia_term::TermData,
        {
            let modified = self.get_wrapped_mut().remove_indexed(s, p, o, g);
            self.idw_hook_remove_indexed(&modified);
            modified
        }
    };
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in ./inmem.rs).
}
