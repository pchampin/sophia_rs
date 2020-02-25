//! API for serializing RDF syntaxes.
//!
//! This module specifies, through dedicated traits,
//! how serialization of triples/graphs and quads/dataset is handled in sophia.
//! These traits are designed with the idea that each serializer has a specific
//! “target” (typically a file or something similar)
//! associated to it.
//! If you want to serialize to two different files,
//! you must create two different serializers.
//!
//! Note however that this API does not cover the creation of serializers,
//! and therefore does not cover how their target is specified.

use crate::dataset::*;
use crate::graph::*;
use crate::quad::stream::*;
use crate::triple::{stream::*, *};

pub mod nq;
pub mod nt;

/// A triple serializer writes triples according to a given format.
pub trait TripleSerializer {
    type Error: 'static + std::error::Error;

    /// Serialize all triples from the given [`TripleSource`].
    ///
    /// [`TripleSource`]: ../triple/stream/trait.TripleSource.html
    fn serialize_triples<TS>(
        &mut self,
        source: TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: TripleSource,
        Self: Sized;

    /// Serialize a whole [`Graph`].
    ///
    /// While this method has a default implementation based on [`serialize_triples`],
    /// some implementations may override it in order to better use the structure of the Graph.
    ///
    /// [`Graph`]: ../graph/trait.Graph.html
    /// [`serialize_triples`]: #tymethod.serialize_triples
    #[inline]
    fn serialize_graph<G>(&mut self, graph: &G) -> StreamResult<&mut Self, G::Error, Self::Error>
    where
        G: Graph,
        Self: Sized,
    {
        self.serialize_triples(graph.triples())
    }
}

/// A quad serializer writes quads according to a given format.
pub trait QuadSerializer {
    type Error: 'static + std::error::Error;

    /// Serialize all quads from the given [`QuadSource`].
    ///
    /// [`QuadSource`]: ../quad/stream/trait.QuadSource.html
    fn serialize_quads<QS>(
        &mut self,
        source: QS,
    ) -> StreamResult<&mut Self, QS::Error, Self::Error>
    where
        QS: QuadSource,
        Self: Sized;

    /// Serialize a whole [`Dataset`].
    ///
    /// While this method has a default implementation based on [`serialize_quads`],
    /// some implementations may override it in order to better use the structure of the Dataset.
    ///
    /// [`Dataset`]: ../dataset/trait.Dataset.html
    /// [`serialize_quads`]: #tymethod.serialize_quads
    #[inline]
    fn serialize_dataset<D>(
        &mut self,
        dataset: &D,
    ) -> StreamResult<&mut Self, D::Error, Self::Error>
    where
        D: Dataset,
        Self: Sized,
    {
        self.serialize_quads(dataset.quads())
    }
}

/// A stringifier is special kind of [`TripleSerializer`] or [`QuadSerializer`]:
///
/// + it uses a text-based format encoded in UTF8;
/// + it stores the serialize data in memory;
/// + it gives access to the serialized data as `str` or `String`.
///
/// [`TripleSerializer`]: trait.TripleSerializer.html
/// [`QuadSerializer`]: trait.QuadSerializer.html
pub trait Stringifier {
    /// Borrows the internal serialized data.
    ///
    /// # Safety
    /// It is the responsibility of implementors to ensure that this data is valid UTF8.
    /// The methods [`as_str`](#method.as_str) and
    /// [`to_string`](#method.to_string) rely on this.
    fn as_utf8(&self) -> &[u8];

    /// Borrows the internal serialized data as a `str`.
    fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.as_utf8()) }
    }

    /// Copy the internal serialized data to a `String`.
    fn to_string(&self) -> String {
        self.as_str().to_string()
    }
}
