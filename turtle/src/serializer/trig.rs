//! Serializer for the [TriG] concrete syntax of RDF.
//!
//! **Important**:
//! the methods in this module accepting a [`Write`]
//! make no effort to minimize the number of write operations.
//! Hence, in most cased, they should be passed a [`BufWriter`].
//!
//! ## Pretty vs. lazy serializer
//! The option [`TriGConfig::pretty`]
//! determines how much effort the serializer will make:
//! * when `true`, it will first analyze the whole graph,
//!   in order to group quads in an optimal way,
//!   and use as much syntactic sugar as possible;
//! * when `false`, it will serializer quads in the order they come,
//!   only using similarities with the previous quad to simplify the output.
//!
//! The first option is *much more* costly,
//! and therefore is not the default.
//!
//! [TriG]: https://www.w3.org/TR/rdf12-trig/
//! [`Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
//! [`BufWriter`]: https://doc.rust-lang.org/std/io/struct.BufWriter.html

use sophia_api::quad::Quad;
use sophia_api::serializer::{QuadSerializer, Stringifier};
use sophia_api::source::StreamResult;
use sophia_api::source::{QuadSource, SinkError, StreamResultExt};
use sophia_api::term::{SimpleTerm, Term};
use std::io;

use crate::serializer::_streaming::StreamingSerializerState;

use super::_pretty::*;

/// TriG serializer configuration.
pub type TriGConfig = super::turtle::TurtleConfig;

/// Type alias of `NTriplesConfig` for backward compatibility
#[deprecated(since = "0.10.0", note = "please use TriGConfig instead")]
pub type TrigConfig = TriGConfig;
/// Type alias of `NTriplesSerializer` for backward compatibility
#[deprecated(since = "0.10.0", note = "please use TriGSerializer instead")]
pub type TrigSerializer<W> = TriGSerializer<W>;

/// TriG serializer.
pub struct TriGSerializer<W> {
    pub(super) config: TriGConfig,
    pub(super) write: W,
}

impl<W> TriGSerializer<W>
where
    W: io::Write,
{
    /// Build a new TriG serializer writing to `write`, with the default config.
    #[inline]
    pub fn new(write: W) -> Self {
        Self::new_with_config(write, TriGConfig::default())
    }

    /// Build a new TriG serializer writing to `write`, with the given config.
    #[inline]
    pub const fn new_with_config(write: W, config: TriGConfig) -> Self {
        Self { config, write }
    }

    /// Borrow this serializer's configuration.
    #[inline]
    pub const fn config(&self) -> &TriGConfig {
        &self.config
    }
}

impl<W> QuadSerializer for TriGSerializer<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn serialize_quads<TS>(
        &mut self,
        mut source: TS,
    ) -> StreamResult<&mut Self, TS::Error, Self::Error>
    where
        TS: QuadSource,
    {
        if self.config.pretty {
            let dataset = source
                .collect_quads::<PrettifiableDataset>()
                .map_sink_err(|_| -> io::Error { unreachable!() })?;
            prettify(dataset, &mut self.write, &self.config, "").map_err(SinkError)?;
        } else {
            for (prefix, ns) in &self.config.prefix_map {
                writeln!(&mut self.write, "PREFIX {}: <{ns}>", prefix.as_str())
                    .map_err(SinkError)?;
            }
            let mut current_graph: Option<SimpleTerm<'static>> = None;
            let mut state = StreamingSerializerState::new(&mut self.write, &self.config);
            source.try_for_each_quad(|q| {
                match (&current_graph, q.g()) {
                    (None, None) => {}
                    (Some(_), None) => {
                        if state.has_s() {
                            state.write_all(b".\n")?;
                        }
                        state.write_all(b"\n}\n")?;
                        state.pop_all();
                        current_graph.take();
                    }
                    (_, Some(gnq)) => {
                        let change = current_graph
                            .as_ref()
                            .map(|gnc| !Term::eq(gnc, gnq))
                            .unwrap_or(true);
                        if change {
                            if state.has_s() {
                                state.write_all(b".\n")?;
                            }
                            state.write_all(b"\n")?;
                            if current_graph.is_some() {
                                state.write_all(b"}\n")?;
                            }
                            state.write_all(b"\nGRAPH ")?;
                            state.write_node(gnq)?;
                            state.write_all(b" {\n")?;
                            state.pop_all();
                            current_graph = q.g().map(|t| t.into_term())
                        }
                    }
                }
                state.write_asserted_triple(q.spog().0)
            })?;
            if state.has_s() {
                state.write_all(b".\n").map_err(SinkError)?;
            }
            if current_graph.is_some() {
                state.write_all(b"}\n").map_err(SinkError)?;
            }
        }
        Ok(self)
    }
}

impl TriGSerializer<Vec<u8>> {
    /// Create a new serializer which targets a `String`.
    #[inline]
    pub fn new_stringifier() -> Self {
        Self::new(Vec::new())
    }
    /// Create a new serializer which targets a `String` with a custom config.
    #[inline]
    pub const fn new_stringifier_with_config(config: TriGConfig) -> Self {
        Self::new_with_config(Vec::new(), config)
    }
}

impl Stringifier for TriGSerializer<Vec<u8>> {
    fn as_utf8(&self) -> &[u8] {
        &self.write[..]
    }
}

#[cfg(test)]
mod test;
