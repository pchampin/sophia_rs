//! I define trait for loader factories.
//!

use std::{fmt::Display, marker::PhantomData, sync::Arc};

use json_ld::Loader;
use json_syntax::Value;
use locspan::Location;
use sophia_iri::Iri;

/// A trait for factory of document loaders.
pub trait LoaderFactory {
    /// Type of loaders this factory yields.
    type Loader<'l>: Loader<
            Iri<Arc<str>>,
            Location<Iri<Arc<str>>>,
            Output = Value<Location<Iri<Arc<str>>>>,
            Error = Self::LoaderError,
        > + Send
        + Sync
        + 'l
    where
        Self: 'l;

    /// Type of loader error.
    type LoaderError: Display + Send;

    /// Yield a new loader.
    fn yield_loader(&self) -> Self::Loader<'_>;
}

/// A loader factory that yields default loaders.
#[derive(Debug, Clone, Default)]
pub struct DefaultLoaderFactory<L> {
    _phantom: PhantomData<L>,
}

impl<L> DefaultLoaderFactory<L>
where
    L: Loader<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Output = Value<Location<Iri<Arc<str>>>>>
        + Default
        + Send
        + Sync,
    L::Error: Display + Send,
{
    /// Create a new [`DefaultLoaderFactory`].
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }
}

impl<L> LoaderFactory for DefaultLoaderFactory<L>
where
    L: Loader<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Output = Value<Location<Iri<Arc<str>>>>>
        + Default
        + Send
        + Sync,
    L::Error: Display + Send,
{
    type Loader<'l> = L
    where
        Self: 'l;

    type LoaderError = L::Error;

    #[inline]
    fn yield_loader(&self) -> Self::Loader<'_> {
        L::default()
    }
}

/// A loader factory that delegates to an underlying closure.
#[derive(Debug, Clone)]
pub struct ClosureLoaderFactory<L, F> {
    closure: F,
    _phantom: PhantomData<fn() -> L>,
}

impl<L, F> ClosureLoaderFactory<L, F>
where
    L: Loader<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Output = Value<Location<Iri<Arc<str>>>>>
        + Send
        + Sync,
    L::Error: Display + Send,
    F: Fn() -> L,
{
    /// Create a new [`ClosureLoaderFactory`] with given closure.
    #[inline]
    pub fn new(closure: F) -> Self {
        Self {
            closure,
            _phantom: PhantomData,
        }
    }
}

impl<L> ClosureLoaderFactory<L, fn() -> L>
where
    L: Loader<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Output = Value<Location<Iri<Arc<str>>>>>
        + Send
        + Sync,
    L::Error: Display + Send,
{
    /// Create a new [`ClosureLoaderFactory`] that yields loaders by cloning a template loader.
    #[inline]
    pub fn new_cloning(template_loader: L) -> ClosureLoaderFactory<L, impl Fn() -> L>
    where
        L: Clone,
    {
        ClosureLoaderFactory::new(move || template_loader.clone())
    }
}

impl<L, F> LoaderFactory for ClosureLoaderFactory<L, F>
where
    L: Loader<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Output = Value<Location<Iri<Arc<str>>>>>
        + Send
        + Sync,
    L::Error: Display + Send,
    F: Fn() -> L,
{
    type Loader<'l> = L
    where
        Self: 'l;

    type LoaderError = L::Error;

    #[inline]
    fn yield_loader(&self) -> Self::Loader<'_> {
        (self.closure)()
    }
}
