//! I provide different document loaders:
//! * [`NoLoader`]: does not load anything (to be used with self-sufficient JSON-LD documents)
//! * [`FsLoader`]: loads documents from the file system, by mapping directories to specific URLs
//! * [`StaticLoader`]: loads a selected set of URLs from memory (useful to embed in a program normative contexts)
//! * [`SurfLoader`]: loads documents directly from the web (only if the `surf_loader` feature is enabled)
//! * [`ChainLoader`]: loads document from the first loader, otherwise falls back to the second one.
use std::sync::Arc;

use json_syntax::Value;
use locspan::Location;
use sophia_iri::Iri;

/// A dummy document loader, that does not load anything.
pub type NoLoader =
    json_ld::NoLoader<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Value<Location<Iri<Arc<str>>>>>;

/// A document loader that can load documents from the file system,
/// by mapping directories to specific URLs.
///
/// See [`jsonld::FsLoader`]
pub type FsLoader =
    json_ld::FsLoader<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Value<Location<Iri<Arc<str>>>>>;

mod static_loader;
pub use static_loader::*;

#[cfg(feature = "surf_loader")]
mod surf_loader;
#[cfg(feature = "surf_loader")]
pub use surf_loader::*;

mod chain_loader;
pub use chain_loader::*;
