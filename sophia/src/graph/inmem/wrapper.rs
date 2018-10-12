// this module is transparently re-exported by its parent `graph`

use super::*;

/// This trait mimmics the interface of the `Graph` trait (TODO link),
/// with default implementations delegating the methods to the wrapped graph.
/// 
/// The macro `impl_graph_for_wrapper!` (TODO link) can be used to derive
/// the Graph implementation for any implementation of GraphWrapper.
pub trait GraphWrapper
{
    /// String Holder (used internally by terms returned by the methods)
    type Wrapped: Graph;

    fn get_wrapped(&self) -> &Self::Wrapped;

    fn get_wrapped_mut(&mut self) -> &mut Self::Wrapped;

    #[inline]
    fn gw_iter<'a> (&'a self) -> FallibleTripleIterator<'a, Self::Wrapped> {
        self.get_wrapped().iter()
    }

    #[inline]
    fn gw_iter_for_s<'a, T> (&'a self, s: &'a Term<T>) -> FallibleTripleIterator<'a, Self::Wrapped> where
        T: Borrow<str>,
    {
        self.get_wrapped().iter_for_s(s)
    }
    #[inline]
    fn gw_iter_for_p<'a, T> (&'a self, p: &'a Term<T>) -> FallibleTripleIterator<'a, Self::Wrapped> where
        T: Borrow<str>,
    {
        self.get_wrapped().iter_for_p(p)
    }
    #[inline]
    fn gw_iter_for_o<'a, T> (&'a self, o: &'a Term<T>) -> FallibleTripleIterator<'a, Self::Wrapped> where
        T: Borrow<str>,
    {
        self.get_wrapped().iter_for_o(o)
    }
    #[inline]
    fn gw_iter_for_sp<'a, T, U> (&'a self, s: &'a Term<T>, p: &'a Term<U>) -> FallibleTripleIterator<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().iter_for_sp(s, p)
    }
    #[inline]
    fn gw_iter_for_so<'a, T, U> (&'a self, s: &'a Term<T>, o: &'a Term<U>) -> FallibleTripleIterator<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().iter_for_so(s, o)
    }
    #[inline]
    fn gw_iter_for_po<'a, T, U> (&'a self, p: &'a Term<T>, o: &'a Term<U>) -> FallibleTripleIterator<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().iter_for_po(p, o)
    }
    #[inline]
    fn gw_iter_for_spo<'a, T, U, V> (&'a self, s: &'a Term<T>, p: &'a Term<U>, o: &'a Term<V>) -> FallibleTripleIterator<'a, Self::Wrapped> where
        T: Borrow<str>,
        U: Borrow<str>,
        V: Borrow<str>,
    {
        self.get_wrapped().iter_for_spo(s, p, o)
    }

    #[inline]
    fn gw_contains(&self, s: &RefTerm, p: &RefTerm, o: &RefTerm) -> GResult<Self::Wrapped, bool> {
        self.get_wrapped().contains(s, p, o)
    }

    #[inline]
    fn gw_hint(&self) -> (usize, Option<usize>) {
        self.get_wrapped().hint()
    }

    #[inline]
    fn gw_hint_for_s<T> (&self, s: &Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        self.get_wrapped().hint_for_s(s)
    }

    #[inline]
    fn gw_hint_for_p<T> (&self, p: &Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        self.get_wrapped().hint_for_p(p)
    }

    #[inline]
    fn gw_hint_for_o<T> (&self, o: &Term<T>) -> (usize, Option<usize>) where
        T: Borrow<str>,
    {
        self.get_wrapped().hint_for_o(o)
    }

    #[inline]
    fn gw_hint_for_sp<T, U> (&self, s: &Term<T>, p: &Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().hint_for_sp(s, p)
    }

    #[inline]
    fn gw_hint_for_so<T, U> (&self, s: &Term<T>, o: &Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().hint_for_so(s, o)
    }

    #[inline]
    fn gw_hint_for_po<T, U> (&self, p: &Term<T>, o: &Term<U>) -> (usize, Option<usize>) where
        T: Borrow<str>,
        U: Borrow<str>,
    {
        self.get_wrapped().hint_for_po(p, o)
    }
}

macro_rules! impl_graph_for_wrapper {
    ($wrapper: ty) => {
        impl Graph for $wrapper {
            impl_graph_for_wrapper!();
        }
    };
    () => {
        type Holder = <<Self as GraphWrapper>::Wrapped as Graph>::Holder;
        type Error = <<Self as GraphWrapper>::Wrapped as Graph>::Error;

        #[inline]
        fn iter<'a> (&'a self) -> FallibleTripleIterator<'a, Self> {
            GraphWrapper::gw_iter(self)
        }
        #[inline]
        fn iter_for_s<'a, T_> (&'a self, s: &'a Term<T_>) -> FallibleTripleIterator<'a, Self> where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_s(self, s)
        }
        #[inline]
        fn iter_for_p<'a, T_> (&'a self, p: &'a Term<T_>) -> FallibleTripleIterator<'a, Self> where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_p(self, p)
        }
        #[inline]
        fn iter_for_o<'a, T_> (&'a self, o: &'a Term<T_>) -> FallibleTripleIterator<'a, Self> where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_o(self, o)
        }
        #[inline]
        fn iter_for_sp<'a, T_, U_> (&'a self, s: &'a Term<T_>, p: &'a Term<U_>) -> FallibleTripleIterator<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_sp(self, s, p)
        }
        #[inline]
        fn iter_for_so<'a, T_, U_> (&'a self, s: &'a Term<T_>, o: &'a Term<U_>) -> FallibleTripleIterator<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_so(self, s, o)
        }
        #[inline]
        fn iter_for_po<'a, T_, U_> (&'a self, p: &'a Term<T_>, o: &'a Term<U_>) -> FallibleTripleIterator<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_po(self, p, o)
        }
        #[inline]
        fn iter_for_spo<'a, T_, U_, V_> (&'a self, s: &'a Term<T_>, p: &'a Term<U_>, o: &'a Term<V_>) -> FallibleTripleIterator<'a, Self> where
            T_: Borrow<str>,
            U_: Borrow<str>,
            V_: Borrow<str>,
        {
            GraphWrapper::gw_iter_for_spo(self, s, p, o)
        }

        #[inline]
        fn contains(&self, s: &RefTerm, p: &RefTerm, o: &RefTerm) -> GResult<Self, bool> {
            GraphWrapper::gw_contains(self, s, p, o)
        }

        #[inline]
        fn hint(&self) -> (usize, Option<usize>) {
            GraphWrapper::gw_hint(self)
        }

        #[inline]
        fn hint_for_s<T_> (&self, s: &Term<T_>) -> (usize, Option<usize>) where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_hint_for_s(self, s)
        }

        #[inline]
        fn hint_for_p<T_> (&self, p: &Term<T_>) -> (usize, Option<usize>) where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_hint_for_p(self, p)
        }

        #[inline]
        fn hint_for_o<T_> (&self, o: &Term<T_>) -> (usize, Option<usize>) where
            T_: Borrow<str>,
        {
            GraphWrapper::gw_hint_for_o(self, o)
        }

        #[inline]
        fn hint_for_sp<T_, U_> (&self, s: &Term<T_>, p: &Term<U_>) -> (usize, Option<usize>) where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_hint_for_sp(self, s, p)
        }

        #[inline]
        fn hint_for_so<T_, U_> (&self, s: &Term<T_>, o: &Term<U_>) -> (usize, Option<usize>) where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_hint_for_so(self, s, o)
        }

        #[inline]
        fn hint_for_po<T_, U_> (&self, p: &Term<T_>, o: &Term<U_>) -> (usize, Option<usize>) where
            T_: Borrow<str>,
            U_: Borrow<str>,
        {
            GraphWrapper::gw_hint_for_po(self, p, o)
        }
    };
}

