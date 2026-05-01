//! A crate for working with [BCP47](https://tools.ietf.org/search/bcp47) language tags.

mod _i18n_string;
mod _tag;

pub use _i18n_string::*;
pub use _tag::*;
#[cfg(feature = "serde")]
mod _serde;
