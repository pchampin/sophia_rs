use crate::Loader;
use sophia_api::graph::CollectibleGraph;
use sophia_api::ns::rdf;
use sophia_api::term::matcher::Any;
use sophia_api::Error;
use sophia_api::MownStr;
use sophia_api::{prelude::*, term::SimpleTerm};
use sophia_iri::is_absolute_iri_ref;
use std::borrow::Borrow;
use std::sync::Arc;

use super::{ResourceError::*, *};

/// A [`Resource`] represents a specific node in a given graph.
#[derive(Debug)]
pub struct Resource<G, L> {
    pub(super) id: SimpleTerm<'static>,
    pub(super) base: Option<Iri<String>>,
    pub(super) graph: Arc<G>,
    pub(super) loader: Arc<L>,
}

impl<G, L> Resource<G, L>
where
    G: Graph + 'static,
    // G::Error: Error,
    L: Loader,
{
    /// Constructor
    pub fn new<T: Term>(id: T, base: Option<Iri<String>>, graph: Arc<G>, loader: Arc<L>) -> Self {
        let id = id.into_term();
        Resource {
            id,
            base,
            graph,
            loader,
        }
    }

    /// The identifying term of this resource
    pub fn id(&self) -> &SimpleTerm<'static> {
        &self.id
    }

    /// The URL of the underlying graph of this resource
    pub fn base(&self) -> Option<&Iri<String>> {
        self.base.as_ref()
    }

    /// The underlying graph of this resource
    pub fn graph(&self) -> &Arc<G> {
        &self.graph
    }

    /// The loader used to load neighbouring resources
    pub fn loader(&self) -> &Arc<L> {
        &self.loader
    }

    /// Get the unique value of this resource for the given predicate, as a [`TypedResource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// if there is not exactly one value,
    /// if it can not be loaded,
    /// or if it can not be converted to `R`.
    ///
    /// See also [`Self::get_resource`], [`Self::get_term`],
    /// [`Self::get_any_typed`], [`Self::get_all_typed`],
    /// [`Self::pred_typed`],
    pub fn get_typed<R, T>(&self, predicate: T) -> Result<R, R::Error>
    where
        R: TypedResource<G, L>,
        T: Term,
        G: CollectibleGraph,
    {
        self.get_resource(predicate)?.try_into()
    }

    /// Get any value of this resource for the given predicate, as a [`TypedResource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// if the found value can not be loaded,
    /// or if it can not be converted to `R`.
    ///
    /// Note that this method will try to convert the first found value,
    /// and fail if it can not be converted.
    /// It will **not** try other values until one can be converted.
    /// In other words, the assumption is that all values of `predicate`
    /// can be converted to `R`.
    ///
    /// See also [`Self::get_any_resource`], [`Self::get_any_term`],
    /// [`Self::get_typed`], [`Self::get_all_typed`],
    /// [`Self::pred_any_typed`],
    pub fn get_any_typed<R, T>(&self, predicate: T) -> Result<Option<R>, R::Error>
    where
        R: TypedResource<G, L>,
        T: Term,
        G: CollectibleGraph,
    {
        self.get_any_resource(predicate)?
            .map(R::try_from)
            .transpose()
    }

    /// Get all values of this resource for the given predicate, as [`TypedResource`]s.
    ///
    /// Yield an error if the underlying graph errs,
    /// if a value can not be loaded,
    /// or can not be converted to `R`.
    ///
    /// See also [`Self::get_all_resources`], [`Self::get_all_terms`],
    /// [`Self::get_typed`], [`Self::get_any_typed`],
    /// [`Self::get_typed_items`],
    /// [`Self::pred_all_typed`],
    pub fn get_all_typed<'s, R, T>(
        &'s self,
        predicate: T,
    ) -> impl Iterator<Item = Result<R, R::Error>> + 's
    where
        R: TypedResource<G, L>,
        T: Term + 's,
        G: CollectibleGraph + 's,
        L: Loader + 's,
    {
        self.get_all_resources(predicate)
            .map(|res| R::try_from(res.map_err(R::Error::from)?))
    }

    /// Get all elements of the list value for the given predicate as [`TypedResource`]s.
    ///
    /// Yield an error if the underlying graph errs,
    /// if the list is ambiguously malformed (see [`Self::get_resource_items`] for more details),
    /// if a value can not be loaded,
    /// or if a value can not be converted to `R`.
    ///
    /// See also [`Self::get_resource_items`], [`Self::get_term_items`],
    /// [`Self::get_all_typed`].
    pub fn get_typed_items<R, T>(&self, predicate: T) -> LadderTypedIterator<R, G, L>
    where
        R: TypedResource<G, L>,
        T: Term,
        G: CollectibleGraph,
        L: Loader,
    {
        self.get_term_items(predicate).into()
    }

    /// Get the unique predecessor of this resource for the given predicate, as a [`TypedResource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// if there is not exactly one predecessor,
    /// if it can not be loaded,
    /// or if it can not be converted to `R`.
    ///
    /// See also [`Self::pred_resource`], [`Self::pred_term`],
    /// [`Self::pred_any_typed`], [`Self::pred_all_typed`],
    /// [`Self::get_typed`],
    pub fn pred_typed<R, T>(&self, predicate: T) -> Result<R, R::Error>
    where
        R: TypedResource<G, L>,
        T: Term,
        G: CollectibleGraph,
    {
        self.pred_resource(predicate)?.try_into()
    }

    /// Get any predecessor of this resource for the given predicate, as a [`TypedResource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// if the found predecessor can not be loaded,
    /// or if it can not be converted to `R`.
    ///
    /// Note that this method will try to convert the first found predecessor,
    /// and fail if it can not be converted.
    /// It will **not** try other predecessors until one can be converted.
    /// In other words, the assumption is that all predecessors of `predicate`
    /// can be converted to `R`.
    ///
    /// See also [`Self::pred_any_resource`], [`Self::pred_any_term`],
    /// [`Self::pred_typed`], [`Self::pred_all_typed`],
    /// [`Self::get_any_typed`],
    pub fn pred_any_typed<R, T>(&self, predicate: T) -> Result<Option<R>, R::Error>
    where
        R: TypedResource<G, L>,
        T: Term,
        G: CollectibleGraph,
    {
        self.pred_any_resource(predicate)?
            .map(R::try_from)
            .transpose()
    }

    /// Get all predecessors of this resource for the given predicate, as [`TypedResource`]s.
    ///
    /// Yield an error if the underlying graph errs,
    /// if a predecessor can not be loaded,
    /// or can not be converted to `R`.
    ///
    /// See also [`Self::pred_all_resources`], [`Self::pred_all_terms`],
    /// [`Self::pred_typed`], [`Self::pred_any_typed`],
    /// [`Self::get_all_typed`],
    pub fn pred_all_typed<'s, R, T>(
        &'s self,
        predicate: T,
    ) -> impl Iterator<Item = Result<R, R::Error>> + 's
    where
        R: TypedResource<G, L>,
        T: Term + 's,
        G: CollectibleGraph + 's,
        L: Loader + 's,
    {
        self.pred_all_resources(predicate)
            .map(|res| R::try_from(res.map_err(R::Error::from)?))
    }

    /// Get the unique value of this resource for the given predicate as a [`Resource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// if there is not exactly one value,
    /// or if it can not be loaded.
    ///
    /// See also [`Self::get_typed`], [`Self::get_term`],
    /// [`Self::get_any_resource`], [`Self::get_all_resources`],
    /// [`Self::pred_resource`],
    pub fn get_resource<T>(&self, predicate: T) -> ResourceResult<Self, G>
    where
        T: Term,
        G: CollectibleGraph,
    {
        let mut objects = self.get_all_resources(predicate.borrow_term());
        let first = objects.next();
        if objects.next().is_some() {
            Err(UnexpectedMultipleValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })
        } else {
            first.ok_or_else(|| NoValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })?
        }
    }

    /// Get any value of this resource for the given predicate, as a [`Resource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// or if the found value can not be loaded.
    ///
    /// See also [`Self::get_any_typed`], [`Self::get_any_term`],
    /// [`Self::get_resource`], [`Self::get_all_resources`],
    /// [`Self::pred_any_resource`],
    pub fn get_any_resource<T>(&self, predicate: T) -> ResourceResult<Option<Self>, G>
    where
        T: Term,
        G: CollectibleGraph,
    {
        self.get_all_resources(predicate).next().transpose()
    }

    /// Get all values of this resource for the given predicate, as [`Resource`]s.
    ///
    /// Yield an error if the underlying graph errs,
    /// or if a value can not be loaded.
    ///
    /// See also [`Self::get_all_typed`], [`Self::get_all_terms`],
    /// [`Self::get_resource`], [`Self::get_any_resource`],
    /// [`Self::get_resource_items`],
    /// [`Self::pred_all_resources`],
    pub fn get_all_resources<'s, T>(
        &'s self,
        predicate: T,
    ) -> impl Iterator<Item = ResourceResult<Self, G>> + 's
    where
        T: Term + 's,
        G: CollectibleGraph + 's,
        L: Loader + 's,
    {
        self.get_all_terms(predicate)
            .map(|res| self.get_neighbour(res))
    }

    /// Get all elements of the list value for the given predicate as [`Resource`]s.
    ///
    /// Yield an error if the underlying graph errs,
    /// if the list is ambiguously malformed (see [`Self::get_resource_items`] for more details),
    /// or if a value can not be loaded.
    ///
    /// See also [`Self::get_typed_items`], [`Self::get_term_items`],
    /// [`Self::get_all_resources`].
    pub fn get_resource_items<T>(&self, predicate: T) -> LadderResourceIterator<G, L>
    where
        T: Term,
        G: CollectibleGraph,
        L: Loader + 'static,
    {
        LadderResourceIterator::from(self.get_term_items(predicate))
    }

    /// Get the unique predecessor of this resource for the given predicate as a [`Resource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// if there is not exactly one predecessor,
    /// or if it can not be loaded.
    ///
    /// See also [`Self::pred_typed`], [`Self::pred_term`],
    /// [`Self::pred_any_resource`], [`Self::pred_all_resources`],
    /// [`Self::get_resource`],
    pub fn pred_resource<T>(&self, predicate: T) -> ResourceResult<Self, G>
    where
        T: Term,
        G: CollectibleGraph,
    {
        let mut objects = self.pred_all_resources(predicate.borrow_term());
        let first = objects.next();
        if objects.next().is_some() {
            Err(UnexpectedMultipleValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })
        } else {
            first.ok_or_else(|| NoValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })?
        }
    }

    /// Get any predecessor of this resource for the given predicate, as a [`Resource`].
    ///
    /// Raise an error if the underlying graph errs,
    /// or if the found predecessor can not be loaded.
    ///
    /// See also [`Self::pred_any_typed`], [`Self::pred_any_term`],
    /// [`Self::pred_resource`], [`Self::pred_all_resources`],
    /// [`Self::get_any_resource`],
    pub fn pred_any_resource<T>(&self, predicate: T) -> ResourceResult<Option<Self>, G>
    where
        T: Term,
        G: CollectibleGraph,
    {
        self.pred_all_resources(predicate).next().transpose()
    }

    /// Get all predecessors of this resource for the given predicate, as [`Resource`]s.
    ///
    /// Yield an error if the underlying graph errs,
    /// or if a predecessor can not be loaded.
    ///
    /// See also [`Self::pred_all_typed`], [`Self::pred_all_terms`],
    /// [`Self::pred_resource`], [`Self::pred_any_resource`],
    /// [`Self::get_all_resources`],
    pub fn pred_all_resources<'s, T>(
        &'s self,
        predicate: T,
    ) -> impl Iterator<Item = ResourceResult<Self, G>> + 's
    where
        T: Term + 's,
        G: CollectibleGraph + 's,
        L: Loader + 's,
    {
        self.pred_all_terms(predicate)
            .map(|res| self.get_neighbour(res))
    }

    /// Get the unique value of this resource for the given predicate as a [`Term`].
    ///
    /// Raise an error if the underlying graph errs,
    /// or if there is not exactly one value.
    ///
    /// See also [`Self::get_typed`], [`Self::get_resource`],
    /// [`Self::get_any_term`], [`Self::get_all_terms`],
    /// [`Self::pred_term`],
    pub fn get_term<T: Term>(&self, predicate: T) -> ResourceResult<SimpleTerm<'static>, G> {
        let mut objects = self.get_all_terms(predicate.borrow_term());
        let first = objects.next();
        if objects.next().is_some() {
            Err(UnexpectedMultipleValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })
        } else {
            first.ok_or_else(|| NoValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })?
        }
    }

    /// Get any value of this resource for the given predicate, as a [`Term`].
    ///
    /// Raise an error if the underlying graph errs.
    ///
    /// See also [`Self::get_any_typed`], [`Self::get_any_resource`],
    /// [`Self::get_term`], [`Self::get_all_terms`],
    /// [`Self::pred_any_term`],
    pub fn get_any_term<T: Term>(
        &self,
        predicate: T,
    ) -> ResourceResult<Option<SimpleTerm<'static>>, G> {
        self.get_all_terms(predicate).next().transpose()
    }

    /// Get all values of this resource for the given predicate, as [`Term`]s.
    ///
    /// Yield an error if the underlying graph errs.
    ///
    /// See also [`Self::get_all_typed`], [`Self::get_all_resources`],
    /// [`Self::get_term`], [`Self::get_any_term`],
    /// [`Self::get_term_items`],
    /// [`Self::pred_all_terms`],
    pub fn get_all_terms<T: Term>(
        &self,
        predicate: T,
    ) -> impl Iterator<Item = ResourceResult<SimpleTerm<'static>, G>> {
        self.graph
            .triples_matching([&self.id], [predicate], Any)
            .map(|res| {
                res.map(|t| t.to_o().into_term())
                    .map_err(|error| GraphError {
                        id: self.id.clone(),
                        error,
                    })
            })
            .collect::<Vec<_>>()
            .into_iter()
    }

    /// Get all elements of the list value for the given predicate as [`Resource`]s.
    ///
    /// Yield an error if the underlying graph errs,
    /// or if the list is ambiguously malformed (see below).
    ///
    /// This implementation is very lenient with the list structure:
    /// it stops as soon as an element without `rdf:first` or `rdf:next` is found
    /// (even if it is not `rdf:nil`).
    /// Note also that if the list is circular,
    /// the iterator will loop indefinitely.
    /// The only case where a malformed list will raise an error is when the list is ambiguous,
    /// i.e. when a node has several `rdf:value` or `rdf:next` properties.
    ///
    /// See also [`Self::get_typed_items`], [`Self::get_resource_items`],
    /// [`Self::get_all_terms`].
    pub fn get_term_items<T: Term>(&self, predicate: T) -> LadderTermIterator<G, L> {
        let current = match self.get_term(predicate) {
            Err(NoValueFor { .. }) => None,
            Err(err) => Some(Err(err)),
            Ok(id) => Some(Ok(Resource {
                id,
                base: self.base.clone(),
                graph: self.graph.clone(),
                loader: self.loader.clone(),
            })),
        };
        LadderTermIterator::new(current, rdf::first.into_term(), rdf::rest.into_term())
    }

    /// Get the unique predecessor of this resource for the given predicate as a [`Term`].
    ///
    /// Raise an error if the underlying graph errs,
    /// or if there is not exactly one predecessor.
    ///
    /// See also [`Self::pred_typed`], [`Self::pred_resource`],
    /// [`Self::pred_any_term`], [`Self::pred_all_terms`],
    /// [`Self::get_term`],
    pub fn pred_term<T: Term>(&self, predicate: T) -> ResourceResult<SimpleTerm<'static>, G> {
        let mut objects = self.pred_all_terms(predicate.borrow_term());
        let first = objects.next();
        if objects.next().is_some() {
            Err(UnexpectedMultipleValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })
        } else {
            first.ok_or_else(|| NoValueFor {
                id: self.id.clone(),
                predicate: predicate.borrow_term().into_term(),
            })?
        }
    }

    /// Get any predecessor of this resource for the given predicate, as a [`Term`].
    ///
    /// Raise an error if the underlying graph errs.
    ///
    /// See also [`Self::pred_any_typed`], [`Self::pred_any_resource`],
    /// [`Self::pred_term`], [`Self::pred_all_terms`],
    /// [`Self::get_any_term`],
    pub fn pred_any_term<T: Term>(
        &self,
        predicate: T,
    ) -> ResourceResult<Option<SimpleTerm<'static>>, G> {
        self.pred_all_terms(predicate).next().transpose()
    }

    /// Get all predecessors of this resource for the given predicate, as [`Term`]s.
    ///
    /// Yield an error if the underlying graph errs.
    ///
    /// See also [`Self::pred_all_typed`], [`Self::pred_all_resources`],
    /// [`Self::pred_term`], [`Self::pred_any_term`],
    /// [`Self::get_all_terms`],
    pub fn pred_all_terms<'s, T: Term + 's>(
        &'s self,
        predicate: T,
    ) -> impl Iterator<Item = ResourceResult<SimpleTerm<'static>, G>> + 's {
        self.graph
            .triples_matching(Any, [predicate], [&self.id])
            .map(|res| {
                res.map(|t| t.to_s().into_term())
                    .map_err(|error| GraphError {
                        id: self.id.clone(),
                        error,
                    })
            })
    }

    /// A utility method for checking that a literal matches expected values,
    /// and raise the correct error otherwise.
    ///
    /// * `value` is the value to check
    /// * `datatype` is the expected datatype
    /// * `lexical_values` is the list of expected values
    ///   (empty slice if any value is acceptable)
    /// * `predicate` is the predicate used to get value,
    ///    (required to generate errors)
    pub fn check_literal<'a, T: Term, U: Term>(
        &self,
        value: &'a SimpleTerm<'static>,
        datatype: T,
        lexical_forms: &[&str],
        predicate: U,
    ) -> ResourceResult<MownStr<'a>, G> {
        let Some(dt) = value.datatype() else {
            return Err(ResourceError::UnexpectedKind {
                id: self.id.clone(),
                predicate: predicate.into_term(),
                found_kind: value.kind(),
            });
        };
        if !Term::eq(dt.borrow_term(), datatype) {
            return Err(ResourceError::UnexpectedDatatype {
                id: self.id.clone(),
                predicate: predicate.into_term(),
                found_datatype: dt.into_term(),
            });
        }
        let lex = value.lexical_form().unwrap();
        if !lexical_forms.is_empty() && lexical_forms.iter().all(|i| *i != &*lex) {
            return Err(ResourceError::UnexpectedValue {
                id: self.id.clone(),
                predicate: predicate.into_term(),
                found_value: value.borrow_term().into_term(),
            });
        }
        Ok(lex)
    }

    pub(crate) fn get_neighbour<T: Term>(
        &self,
        res: Result<T, ResourceError<G::Error>>,
    ) -> Result<Self, ResourceError<G::Error>>
    where
        G: CollectibleGraph,
    {
        let t = res?;
        if let Some(iri_ref) = t.iri() {
            let iri = to_iri(iri_ref).map_err(IriNotAbsolute)?;
            if let Some(base) = &self.base {
                let other_base = iri.as_str().split('#').next().unwrap();
                if other_base != base.as_str() {
                    return self.loader.get_resource(iri).map_err(LoaderError);
                }
            }
        }
        Ok(Resource::new(
            t.borrow_term(),
            self.base.clone(),
            self.graph.clone(),
            self.loader.clone(),
        ))
    }
}

impl<G, L> Clone for Resource<G, L> {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            base: self.base.clone(),
            graph: self.graph.clone(),
            loader: self.loader.clone(),
        }
    }
}

pub(crate) fn to_iri<T: Borrow<str>>(iri_ref: IriRef<T>) -> Result<Iri<T>, IriRef<Box<str>>> {
    if is_absolute_iri_ref(iri_ref.as_str()) {
        Ok(Iri::new_unchecked(iri_ref.unwrap()))
    } else {
        Err(iri_ref.map_unchecked(|m| Box::from(m.borrow())))
    }
}
