///! API for serializing RDF syntaxes.
use std::io;

use crate::dataset::*;
use crate::graph::*;
use crate::quad::stream::*;
use crate::triple::{stream::*, *};

use std::result::Result; // override ::error::Result

/// TODO doc
pub trait TripleSerializer<T> {
    type Sink: TripleSink;

    /// A sink serializing the triples fed to it.
    fn sink(&self, target: T) -> Self::Sink;

    #[inline]
    fn serialize_triples_in<TS>(&self, mut source: TS, target: T) -> TSResult<Self, T, TS::Error>
    where
        TS: TripleSource,
    {
        source.in_sink(&mut self.sink(target))
    }

    #[inline]
    fn serialize_graph_in<G>(&self, graph: &G, target: T) -> TSResult<Self, T, G::Error>
    where
        G: Graph,
    {
        self.serialize_triples_in(graph.triples(), target)
    }
}

/// TODO doc
pub trait TripleStringifier {
    fn stringify_triples<TS>(
        &self,
        source: TS,
    ) -> std::result::Result<std::string::String, StreamError<TS::Error, std::io::Error>>
    where
        TS: TripleSource;

    fn stringify_graph<'g, G>(
        &self,
        graph: &'g G,
    ) -> Result<String, StreamError<G::Error, io::Error>>
    where
        G: Graph;
}

/// Derive the `TripleStringifier` trait from a `TripleSerializer` type supporting `&mut Vec[u8]`.
/// Eventually, this should be refactored as a proper *derive* macro.
#[macro_export]
macro_rules! derive_triple_stringifier {
    ($stringifier: ty) => {
        impl TripleStringifier for $stringifier {
            #[inline]
            fn stringify_triples<TS>(
                &self,
                source: TS,
            ) -> std::result::Result<std::string::String, StreamError<TS::Error, std::io::Error>>
            where
                TS: $crate::triple::stream::TripleSource,
            {
                let mut v = Vec::new();
                self.serialize_triples_in(source, &mut v).and_then(|_| {
                    String::from_utf8(v).map_err(|_| {
                        StreamError::SinkError(io::Error::from(io::ErrorKind::InvalidData).into())
                    })
                })
            }

            #[inline]
            fn stringify_graph<'g, G>(
                &self,
                graph: &'g G,
            ) -> std::result::Result<std::string::String, StreamError<G::Error, std::io::Error>>
            where
                G: $crate::graph::Graph,
            {
                let mut v = Vec::new();
                self.serialize_graph_in(graph, &mut v).and_then(|_| {
                    String::from_utf8(v).map_err(|_| {
                        StreamError::SinkError(io::Error::from(io::ErrorKind::InvalidData).into())
                    })
                })
            }
        }
    };
}

/// Tyoe alias for `Result`s returned by `TripleSerializer`s.
pub type TSResult<TS, T, E> = Result<
    <<TS as TripleSerializer<T>>::Sink as TripleSink>::Outcome,
    StreamError<E, <<TS as TripleSerializer<T>>::Sink as TripleSink>::Error>,
>;

/// Define convenience module-level functions for a serializer implementation supporting Write.
#[macro_export]
macro_rules! def_mod_functions_for_write_triple_serializer {
    ($serializer_type: ident) => {
        #[inline]
        /// Convenience function for serializing a triple source with the default serializer
        pub fn sink<W>(
            write: W,
        ) -> <$serializer_type as $crate::serializer::TripleSerializer<W>>::Sink
        where
            W: std::io::Write,
        {
            $serializer_type::default().sink(write)
        }

        #[inline]
        /// Convenience function for serializing a triple source with the default serializer
        pub fn serialize_triples_in<TS, W>(
            source: TS,
            write: W,
        ) -> $crate::serializer::TSResult<$serializer_type, W, TS::Error>
        where
            TS: $crate::triple::stream::TripleSource,
            W: std::io::Write,
        {
            $serializer_type::default().serialize_triples_in(source, write)
        }

        #[inline]
        /// Convenience function for serializing a graph with the default serializer
        pub fn serialize_graph_in<G, W>(
            graph: &G,
            write: W,
        ) -> $crate::serializer::TSResult<$serializer_type, W, G::Error>
        where
            G: $crate::graph::Graph,
            W: std::io::Write,
        {
            $serializer_type::default().serialize_graph_in(graph, write)
        }

        #[inline]
        /// Convenience function for stringifying a triple source with the default serializer
        pub fn stringify_triples<TS>(
            source: TS,
        ) -> std::result::Result<std::string::String, StreamError<TS::Error, std::io::Error>>
        where
            TS: $crate::triple::stream::TripleSource,
        {
            $serializer_type::default().stringify_triples(source)
        }

        #[inline]
        /// Convenience function for stringifying a graph with the default serializer
        pub fn stringify_graph<G>(
            graph: &G,
        ) -> std::result::Result<std::string::String, StreamError<G::Error, std::io::Error>>
        where
            G: $crate::graph::Graph,
        {
            $serializer_type::default().stringify_graph(graph)
        }
    };
}

/// TODO doc
pub trait QuadSerializer<T> {
    type Sink: QuadSink;

    /// A sink serializing the triples fed to it.
    fn sink(&self, target: T) -> Self::Sink;

    #[inline]
    fn serialize_quads_in<QS>(&self, mut source: QS, target: T) -> QSResult<Self, T, QS::Error>
    where
        QS: QuadSource,
    {
        source.in_sink(&mut self.sink(target))
    }

    #[inline]
    fn serialize_dataset_in<D>(&self, dataset: &D, target: T) -> QSResult<Self, T, D::Error>
    where
        D: Dataset,
    {
        self.serialize_quads_in(dataset.quads(), target)
    }
}

/// TODO doc
pub trait QuadStringifier {
    fn stringify_quads<QS>(
        &self,
        source: QS,
    ) -> std::result::Result<std::string::String, StreamError<QS::Error, std::io::Error>>
    where
        QS: QuadSource;

    fn stringify_dataset<'g, D>(
        &self,
        dataset: &'g D,
    ) -> Result<String, StreamError<D::Error, io::Error>>
    where
        D: Dataset;
}

/// Derive the `QuadStringifier` trait from a `QuadSerializer` type supporting `&mut Vec[u8]`.
/// Eventually, this should be refactored as a proper *derive* macro.
#[macro_export]
macro_rules! derive_quad_stringifier {
    ($stringifier: ty) => {
        impl QuadStringifier for $stringifier {
            #[inline]
            fn stringify_quads<QS>(
                &self,
                source: QS,
            ) -> std::result::Result<std::string::String, StreamError<QS::Error, std::io::Error>>
            where
                QS: $crate::quad::stream::QuadSource,
            {
                let mut v = Vec::new();
                self.serialize_quads_in(source, &mut v).and_then(|_| {
                    String::from_utf8(v).map_err(|_| {
                        StreamError::SinkError(io::Error::from(io::ErrorKind::InvalidData).into())
                    })
                })
            }

            #[inline]
            fn stringify_dataset<'g, D>(
                &self,
                dataset: &'g D,
            ) -> std::result::Result<std::string::String, StreamError<D::Error, std::io::Error>>
            where
                D: $crate::dataset::Dataset,
            {
                let mut v = Vec::new();
                self.serialize_dataset_in(dataset, &mut v).and_then(|_| {
                    String::from_utf8(v).map_err(|_| {
                        StreamError::SinkError(io::Error::from(io::ErrorKind::InvalidData).into())
                    })
                })
            }
        }
    };
}

/// Tyoe alias for `Result`s returned by `QuadSerializer`s.
pub type QSResult<QS, T, E> = Result<
    <<QS as QuadSerializer<T>>::Sink as QuadSink>::Outcome,
    StreamError<E, <<QS as QuadSerializer<T>>::Sink as QuadSink>::Error>,
>;

#[macro_export]
macro_rules! def_mod_functions_for_write_quad_serializer {
    ($serializer_type: ident) => {
        #[inline]
        /// Convenience function for serializing a quad source with the default serializer
        pub fn sink<W>(
            write: W,
        ) -> <$serializer_type as $crate::serializer::QuadSerializer<W>>::Sink
        where
            W: std::io::Write,
        {
            $serializer_type::default().sink(write)
        }

        #[inline]
        /// Convenience function for serializing a quad source with the default serializer
        pub fn serialize_quads_in<QS, W>(
            source: QS,
            write: W,
        ) -> $crate::serializer::QSResult<$serializer_type, W, QS::Error>
        where
            QS: $crate::quad::stream::QuadSource,
            W: std::io::Write,
        {
            $serializer_type::default().serialize_quads_in(source, write)
        }

        #[inline]
        /// Convenience function for serializing a dataset with the default serializer
        pub fn serialize_dataset_in<D, W>(
            dataset: &D,
            write: W,
        ) -> $crate::serializer::QSResult<$serializer_type, W, D::Error>
        where
            D: $crate::dataset::Dataset,
            W: std::io::Write,
        {
            $serializer_type::default().serialize_dataset_in(dataset, write)
        }

        #[inline]
        /// Convenience function for stringifying a quad source with the default serializer
        pub fn stringify_quads<QS>(
            source: QS,
        ) -> std::result::Result<std::string::String, StreamError<QS::Error, std::io::Error>>
        where
            QS: $crate::quad::stream::QuadSource,
        {
            $serializer_type::default().stringify_quads(source)
        }

        #[inline]
        /// Convenience function for stringifying a dataset with the default serializer
        pub fn stringify_dataset<D>(
            dataset: &D,
        ) -> std::result::Result<std::string::String, StreamError<D::Error, std::io::Error>>
        where
            D: $crate::dataset::Dataset,
        {
            $serializer_type::default().stringify_dataset(dataset)
        }
    };
}

// TODO macro for quads

pub mod nq;
pub mod nt;
