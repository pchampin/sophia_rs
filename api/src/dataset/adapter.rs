//! Adapters for exposing `Dataset`s as `Graph`s and vice-versa

mod _dataset_graph;
pub use _dataset_graph::*;
mod _error;
pub use _error::*;
mod _graph_as_dataset;
pub use _graph_as_dataset::*;
