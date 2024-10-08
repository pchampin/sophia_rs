use std::marker::PhantomData;

use super::{Resource, ResourceError::NoValueFor, ResourceError, ResourceResult, TypedResource};
use crate::Loader;
use sophia_api::{graph::CollectibleGraph, prelude::*, term::SimpleTerm};

/// An iterator of terms over a "blank node label" such as the ones modelling RDF lists.
///
/// # Error
/// This iterator will yield an error if the underlyin graph errs,
/// or if the ladder is malformed (no unique value or successor on a node).
/// Note however than cyclic ladders will not be detected, and will iterate forever.
#[derive(Debug)]
pub struct LadderTermIterator<G: Graph, L>(LadderCursor<G, L>);

impl<G: Graph, L> LadderTermIterator<G, L> {
    /// Constructs a [`LadderTermIterator`].
    ///
    /// `start` is expected to be `Some(Ok(first_blank_node));
    /// if it is `Some(Err(_))`, the iterator will yield this error;
    /// if it is `None`, the iterator will be empty.
    pub const fn new(
        start: Option<ResourceResult<Resource<G, L>, G>>,
        value: SimpleTerm<'static>,
        next: SimpleTerm<'static>,
    ) -> Self {
        Self(LadderCursor {
            current: start,
            value,
            next,
        })
    }
}

impl<G, L> Iterator for LadderTermIterator<G, L>
where
    G: Graph + 'static,
    L: Loader,
{
    type Item = Result<SimpleTerm<'static>, ResourceError<G::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_apply(|_, t| Ok(t))
    }
}

//

/// An iterator of [`Resource`]s over a "blank node label" such as the ones modelling RDF lists.
///
/// See also [`LadderTermIterator`]
#[derive(Debug)]
pub struct LadderResourceIterator<G: Graph, L>(LadderCursor<G, L>);

impl<G: Graph, L> From<LadderTermIterator<G, L>> for LadderResourceIterator<G, L>
where
    G: CollectibleGraph + 'static,
    L: Loader,
{
    fn from(value: LadderTermIterator<G, L>) -> Self {
        Self(value.0)
    }
}

impl<G, L> Iterator for LadderResourceIterator<G, L>
where
    G: CollectibleGraph + 'static,
    L: Loader,
{
    type Item = Result<Resource<G, L>, ResourceError<G::Error>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_apply(|c, t| c.get_neighbour(Ok(t)))
    }
}

//

/// An iterator of [`TypedResource`]s over a "blank node label" such as the ones modelling RDF lists.
///
/// See also [`LadderTermIterator`]
pub struct LadderTypedIterator<T, G: Graph, L>(LadderCursor<G, L>, PhantomData<T>);

impl<T, G, L> From<LadderTermIterator<G, L>> for LadderTypedIterator<T, G, L>
where
    T: TypedResource<G, L>,
    G: CollectibleGraph + 'static,
{
    fn from(value: LadderTermIterator<G, L>) -> Self {
        Self(value.0, PhantomData)
    }
}

impl<T, G, L> Iterator for LadderTypedIterator<T, G, L>
where
    T: TypedResource<G, L>,
    G: CollectibleGraph + 'static,
    L: Loader,
{
    type Item = Result<T, T::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_apply(|c, t| {
            c.get_neighbour(Ok(t))
                .map_err(T::Error::from)
                .and_then(T::try_from)
        })
    }
}

//

#[derive(Debug)]
struct LadderCursor<G: Graph, L> {
    value: SimpleTerm<'static>,
    next: SimpleTerm<'static>,
    #[allow(clippy::type_complexity)]
    current: Option<Result<Resource<G, L>, ResourceError<G::Error>>>,
}

impl<G, L> LadderCursor<G, L>
where
    G: Graph + 'static,
    L: Loader,
{
    fn next_apply<T, E, F>(&mut self, mut f: F) -> Option<Result<T, E>>
    where
        F: FnMut(&Resource<G, L>, SimpleTerm<'static>) -> Result<T, E>,
        E: From<ResourceError<G::Error>>,
    {
        match self.current.take() {
            None => None,
            Some(Err(e)) => Some(Err(e.into())),
            Some(Ok(c)) => match c.get_term(&self.value) {
                Err(NoValueFor { .. }) => None,
                Err(e) => Some(Err(e.into())),
                Ok(v) => match c.get_term(&self.next) {
                    Err(NoValueFor { .. }) => Some(f(&c, v)),
                    Err(e) => Some(Err(e.into())),
                    Ok(n) => {
                        let res = f(&c, v);
                        self.current = Some(Ok(Resource {
                            id: n,
                            base: c.base,
                            graph: c.graph,
                            loader: c.loader,
                        }));
                        Some(res)
                    }
                },
            },
        }
    }
}
