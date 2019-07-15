// this module is transparently re-exported by its parent `dataset::inmem`

use super::*;

/// A dataset wrapper wraps a [`Dataset`] and overrides some of its methods.
///
/// This trait mimmics the interface of the [`Dataset`] trait,
/// with all methods having a default implementation
/// that delegates to the corresponding method of the wrapped dataset.
/// Implementation of this trait may however expected to override
/// *some* of the methods.
///
/// Conversely, the `impl_dataset_for_wrapper!` macro can be used to derive
/// the Dataset implementation for any implementation of DatasetWrapper.
///
/// [`Dataset`]: ../trait.Dataset.html
pub trait DatasetWrapper<'a> {
    /// The type of the wrapped dataset.
    type Wrapped: Dataset<'a>;

    /// Borrow the wrapped dataset.
    fn get_wrapped(&'a self) -> &'a Self::Wrapped;

    /// Borrow the wrapped dataset mutably.
    fn get_wrapped_mut(&'a mut self) -> &'a mut Self::Wrapped;

    #[inline]
    /// Mimmic the [`iter`](../trait.Dataset.html#tymethod.iter) method.
    fn dw_quads(&'a self) -> DQuadSource<'a, Self::Wrapped> {
        self.get_wrapped().quads()
    }

    #[inline]
    /// Mimmic the [`quads_with_s`](../trait.Dataset.html#method.quads_with_s) method.
    fn dw_quads_with_s<T>(&'a self, s: &'a Term<T>) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_s(s)
    }
    #[inline]
    /// Mimmic the [`quads_with_p`](../trait.Dataset.html#method.quads_with_p) method.
    fn dw_quads_with_p<T>(&'a self, p: &'a Term<T>) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_p(p)
    }
    #[inline]
    /// Mimmic the [`quads_with_o`](../trait.Dataset.html#method.quads_with_o) method.
    fn dw_quads_with_o<T>(&'a self, o: &'a Term<T>) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_o(o)
    }
    #[inline]
    /// Mimmic the [`quads_with_g`](../trait.Dataset.html#method.quads_with_g) method.
    fn dw_quads_with_g<T>(&'a self, g: &'a GraphId<T>) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
    {
        self.get_wrapped().quads_with_g(g)
    }
    #[inline]
    /// Mimmic the [`quads_with_sp`](../trait.Dataset.html#method.quads_with_sp) method.
    fn dw_quads_with_sp<T, U>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_sp(s, p)
    }
    #[inline]
    /// Mimmic the [`quads_with_so`](../trait.Dataset.html#method.quads_with_so) method.
    fn dw_quads_with_so<T, U>(
        &'a self,
        s: &'a Term<T>,
        o: &'a Term<U>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_so(s, o)
    }
    #[inline]
    /// Mimmic the [`quads_with_sg`](../trait.Dataset.html#method.quads_with_sg) method.
    fn dw_quads_with_sg<T, U>(
        &'a self,
        s: &'a Term<T>,
        g: &'a GraphId<U>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_sg(s, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_po`](../trait.Dataset.html#method.quads_with_po) method.
    fn dw_quads_with_po<T, U>(
        &'a self,
        p: &'a Term<T>,
        o: &'a Term<U>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_po(p, o)
    }
    #[inline]
    /// Mimmic the [`quads_with_pg`](../trait.Dataset.html#method.quads_with_pg) method.
    fn dw_quads_with_pg<T, U>(
        &'a self,
        p: &'a Term<T>,
        g: &'a GraphId<U>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_pg(p, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_og`](../trait.Dataset.html#method.quads_with_og) method.
    fn dw_quads_with_og<T, U>(
        &'a self,
        o: &'a Term<T>,
        g: &'a GraphId<U>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
    {
        self.get_wrapped().quads_with_og(o, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_spo`](../trait.Dataset.html#method.quads_with_spo) method.
    fn dw_quads_with_spo<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_spo(s, p, o)
    }
    #[inline]
    /// Mimmic the [`quads_with_spg`](../trait.Dataset.html#method.quads_with_spg) method.
    fn dw_quads_with_spg<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        g: &'a GraphId<V>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_spg(s, p, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_sog`](../trait.Dataset.html#method.quads_with_sog) method.
    fn dw_quads_with_sog<T, U, V>(
        &'a self,
        s: &'a Term<T>,
        o: &'a Term<U>,
        g: &'a GraphId<V>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_sog(s, o, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_pog`](../trait.Dataset.html#method.quads_with_pog) method.
    fn dw_quads_with_pog<T, U, V>(
        &'a self,
        p: &'a Term<T>,
        o: &'a Term<U>,
        g: &'a GraphId<V>,
    ) -> DQuadSource<'a, Self::Wrapped>
    where
        T: TermData,
        U: TermData,
        V: TermData,
    {
        self.get_wrapped().quads_with_pog(p, o, g)
    }
    #[inline]
    /// Mimmic the [`quads_with_spog`](../trait.Dataset.html#method.quads_with_spog) method.
    fn dw_quads_with_spog<T, U, V, W>(
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
        g: &'a GraphId<W>,
    ) -> DQuadSource<'a, Self::Wrapped>
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
        &'a self,
        s: &'a Term<T>,
        p: &'a Term<U>,
        o: &'a Term<V>,
        g: &'a GraphId<W>,
    ) -> DResult<'a, Self::Wrapped, bool>
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
    fn dw_subjects(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().subjects()
    }

    #[inline]
    /// Mimmic the [`predicates`](../trait.Dataset.html#method.predicates) method.
    fn dw_predicates(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().predicates()
    }

    #[inline]
    /// Mimmic the [`objects`](../trait.Dataset.html#method.objects) method.
    fn dw_objects(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().objects()
    }

    #[inline]
    /// Mimmic the [`graph_names`](../trait.Dataset.html#method.graph_names) method.
    fn dw_graph_names(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().graph_names()
    }

    #[inline]
    /// Mimmic the [`iris`](../trait.Dataset.html#method.iris) method.
    fn dw_iris(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().iris()
    }

    #[inline]
    /// Mimmic the [`bnodes`](../trait.Dataset.html#method.bnodes) method.
    fn dw_bnodes(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().bnodes()
    }

    #[inline]
    /// Mimmic the [`literals`](../trait.Dataset.html#method.literals) method.
    fn dw_literals(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().literals()
    }

    #[inline]
    /// Mimmic the [`variables`](../trait.Dataset.html#method.variables) method.
    fn dw_variables(&'a self) -> DResultTermSet<'a, Self::Wrapped> {
        self.get_wrapped().variables()
    }
}

macro_rules! impl_dataset_for_wrapper {
    ($wrapper: ty) => {
        impl<'a> $crate::dataset::Dataset<'a> for $wrapper {
            impl_dataset_for_wrapper!();
        }
    };
    () => {
        type Quad = <<Self as $crate::dataset::inmem::DatasetWrapper<'a>>::Wrapped as $crate::dataset::Dataset<'a>>::Quad;
        type Error = <<Self as $crate::dataset::inmem::DatasetWrapper<'a>>::Wrapped as $crate::dataset::Dataset<'a>>::Error;

        #[inline]
        fn quads(&'a self) -> $crate::dataset::DQuadSource<'a, Self> {
            DatasetWrapper::dw_quads(self)
        }
        #[inline]
        fn quads_with_s<T_>(
            &'a self,
            s: &'a $crate::term::Term<T_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_s(self, s)
        }
        #[inline]
        fn quads_with_p<T_>(
            &'a self,
            p: &'a $crate::term::Term<T_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_p(self, p)
        }
        #[inline]
        fn quads_with_o<T_>(
            &'a self,
            o: &'a $crate::term::Term<T_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_o(self, o)
        }
        #[inline]
        fn quads_with_g<T_>(
            &'a self,
            g: &'a  $crate::term::graph_id::GraphId<T_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_g(self, g)
        }
        #[inline]
        fn quads_with_sp<T_, U_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            p: &'a $crate::term::Term<U_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_sp(self, s, p)
        }
        #[inline]
        fn quads_with_so<T_, U_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            o: &'a $crate::term::Term<U_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_so(self, s, o)
        }
        #[inline]
        fn quads_with_sg<T_, U_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            g: &'a  $crate::term::graph_id::GraphId<U_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_sg(self, s, g)
        }
        #[inline]
        fn quads_with_po<T_, U_>(
            &'a self,
            p: &'a $crate::term::Term<T_>,
            o: &'a $crate::term::Term<U_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_po(self, p, o)
        }
        #[inline]
        fn quads_with_pg<T_, U_>(
            &'a self,
            p: &'a $crate::term::Term<T_>,
            g: &'a  $crate::term::graph_id::GraphId<U_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_pg(self, p, g)
        }
        #[inline]
        fn quads_with_og<T_, U_>(
            &'a self,
            o: &'a $crate::term::Term<T_>,
            g: &'a  $crate::term::graph_id::GraphId<U_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_og(self, o, g)
        }
        #[inline]
        fn quads_with_spo<T_, U_, V_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            p: &'a $crate::term::Term<U_>,
            o: &'a $crate::term::Term<V_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_spo(self, s, p, o)
        }
        #[inline]
        fn quads_with_spg<T_, U_, V_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            p: &'a $crate::term::Term<U_>,
            g: &'a  $crate::term::graph_id::GraphId<V_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_spg(self, s, p, g)
        }
        #[inline]
        fn quads_with_sog<T_, U_, V_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            o: &'a $crate::term::Term<U_>,
            g: &'a $crate::term::graph_id::GraphId<V_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_sog(self, s, o, g)
        }
        #[inline]
        fn quads_with_pog<T_, U_, V_>(
            &'a self,
            p: &'a $crate::term::Term<T_>,
            o: &'a $crate::term::Term<U_>,
            g: &'a $crate::term::graph_id::GraphId<V_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_pog(self, p, o, g)
        }
        #[inline]
        fn quads_with_spog<T_, U_, V_, W_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            p: &'a $crate::term::Term<U_>,
            o: &'a $crate::term::Term<V_>,
            g: &'a  $crate::term::graph_id::GraphId<W_>
        ) -> $crate::dataset::DQuadSource<'a, Self>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
            W_: $crate::term::TermData,
        {
            DatasetWrapper::dw_quads_with_spog(self, s, p, o, g)
        }

        #[inline]
        fn contains<T_, U_, V_, W_>(
            &'a self,
            s: &'a $crate::term::Term<T_>,
            p: &'a $crate::term::Term<U_>,
            o: &'a $crate::term::Term<V_>,
            g: &'a  $crate::term::graph_id::GraphId<W_>
        ) -> $crate::dataset::DResult<'a, Self, bool>
        where
            T_: $crate::term::TermData,
            U_: $crate::term::TermData,
            V_: $crate::term::TermData,
            W_: $crate::term::TermData,
        {
            DatasetWrapper::dw_contains(self, s, p, o, g)
        }

        #[inline]
        fn subjects(&'a self) -> DResult<'a, Self, std::collections::HashSet<$crate::dataset::DTerm<'a, Self>>> {
            DatasetWrapper::dw_subjects(self)
        }

        #[inline]
        fn predicates(&'a self) -> DResult<'a, Self, std::collections::HashSet<$crate::dataset::DTerm<'a, Self>>> {
            DatasetWrapper::dw_predicates(self)
        }

        #[inline]
        fn objects(&'a self) -> DResult<'a, Self, std::collections::HashSet<$crate::dataset::DTerm<'a, Self>>> {
            DatasetWrapper::dw_objects(self)
        }

        #[inline]
        fn iris(&'a self) -> DResult<'a, Self, std::collections::HashSet<$crate::dataset::DTerm<'a, Self>>> {
            DatasetWrapper::dw_iris(self)
        }

        #[inline]
        fn bnodes(&'a self) -> DResult<'a, Self, std::collections::HashSet<$crate::dataset::DTerm<'a, Self>>> {
            DatasetWrapper::dw_bnodes(self)
        }

        #[inline]
        fn literals(&'a self) -> DResult<'a, Self, std::collections::HashSet<$crate::dataset::DTerm<'a, Self>>> {
            DatasetWrapper::dw_literals(self)
        }

        #[inline]
        fn variables(&'a self) -> DResult<'a, Self, std::collections::HashSet<$crate::dataset::DTerm<'a, Self>>> {
            DatasetWrapper::dw_variables(self)
        }
    };
}

/// An indexed dataset wrapper wraps an [`IndexedDataset`] and augments some of its methods,
/// through hooks of the forme `before_x` and `after_x`.
///
/// This trait is designed to add mutability to [`DatasetWrapper`],
/// through yje `impl_indexed_dataset_for_wrapper!` macro.
///
/// [`IndexedDataset`]: ../index/trait.IndexedDataset.html
/// [`DatasetWrapper`]: ./trait.DatasetWrapper.html
pub trait IndexedDatasetWrapper<T>
where
    T: IndexedDataset,
{
    /// Hook to be executed at the end of
    /// [`IndexedDataset::insert_indexed`](../index/trait.IndexedDataset.html#tymethod.insert_indexed).
    fn idw_hook_insert_indexed(&mut self, modified: &Option<[T::Index; 4]>);

    /// Hook to be executed at the end of
    /// [`IndexedDataset::remove_indexed`](../index/trait.IndexedDataset.html#tymethod.remove_indexed).
    fn idw_hook_remove_indexed(&mut self, modified: &Option<[T::Index; 4]>);

    /// Hook to be executed at the end of
    /// [`IndexedDataset::shrink_to_fit`](../index/trait.IndexedDataset.html#tymethod.shrink_to_fit).
    fn idw_hook_shrink_to_fit(&mut self);
}

macro_rules! impl_indexed_dataset_for_wrapper {
    ($wrapper: ty) => {
        impl $crate::dataset::index::IndexedDataset for $wrapper
        where
            T: $crate::dataset::index::IndexedDataset + for<'a> $crate::dataset::Dataset<'a, Quad = [&'a $crate::term::Term<<T as $crate::dataset::index::IndexedDataset>::TermData>; 3]>,
        {
            impl_indexed_dataset_for_wrapper!();
        }
    };
    () => {
        type Index = T::Index;
        type TermData = T::TermData;

        #[inline]
        fn get_index<U>(&self, t: &$crate::term::Term<U>) -> Option<Self::Index>
        where
            U: $crate::term::TermData,
        {
            self.get_wrapped().get_index(t)
        }

        #[inline]
        fn get_index_for_graph_id<U>(
            &self,
            g: & $crate::term::graph_id::GraphId<U>
        ) -> Option<Self::Index>
        where
            U: $crate::term::TermData,
        {
            self.get_wrapped().get_index_for_graph_id(g)
        }

        #[inline]
        fn get_term(& self, i: Self::Index) -> Option<&Term<Self::TermData>> {
            self.get_wrapped().get_term(i)
        }

        #[inline]
        fn get_graph_id(&self, i: Self::Index) -> Option<&GraphId<Self::TermData>> {
            self.get_wrapped().get_graph_id(i)
        }

        fn insert_indexed<U, V, W, X>(
            &mut self,
            s: &$crate::term::Term<U>,
            p: &$crate::term::Term<V>,
            o: &$crate::term::Term<W>,
            g: &$crate::term::graph_id::GraphId<X>,
        ) -> Option<[Self::Index; 4]>
        where
            U: $crate::term::TermData,
            V: $crate::term::TermData,
            W: $crate::term::TermData,
            X: $crate::term::TermData,
        {
            let modified = self.get_wrapped_mut().insert_indexed(s, p, o, g);
            self.idw_hook_insert_indexed(&modified);
            modified
        }

        fn remove_indexed<U, V, W, X>(
            &mut self,
            s: &$crate::term::Term<U>,
            p: &$crate::term::Term<V>,
            o: &$crate::term::Term<W>,
            g: &$crate::term::graph_id::GraphId<X>,
        ) -> Option<([Self::Index; 4])>
        where
            U: $crate::term::TermData,
            V: $crate::term::TermData,
            W: $crate::term::TermData,
            X: $crate::term::TermData,
        {
            let modified = self.get_wrapped_mut().remove_indexed(s, p, o, g);
            self.idw_hook_remove_indexed(&modified);
            modified
        }

        fn shrink_to_fit(&mut self) {
            self.get_wrapped_mut().shrink_to_fit();
            self.idw_hook_shrink_to_fit();
        }
    };
}

#[cfg(test)]
mod test {
    // The code from this module is tested through its use in other modules
    // (especially in ./inmem.rs).
}
