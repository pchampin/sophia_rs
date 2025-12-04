//! Example of use:
//! ```
//! # use sophia_api::{prelude::*, sparql::*};
//! # use sophia_sparql::*;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! # let dataset: Vec<[i32; 4]> = vec![]; // dummy dataset
//! #
//! let dataset = SparqlWrapper(&dataset);
//! let query = SparqlQuery::parse("SELECT ?o { ?s a ?o }")?;
//! let bindings = dataset.query(&query)?.into_bindings();
//! for b in bindings {
//!     let b = b?;
//!     if let Some(o) = &b[0] {
//!         println!("found {o}");
//!     }
//! }
//! #
//! # Ok(()) }
//! ```

mod bgp;
mod binding;
mod exec;
mod expression;
mod function;
mod matcher;
mod ns;
mod stash;
mod term;
mod value;
mod wrapper;

pub use binding::{BindingMap, Bindings};
pub use term::*;
pub use wrapper::*;

#[cfg(test)]
mod test;
