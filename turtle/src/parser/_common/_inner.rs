use sophia_api::version::Version;

use crate::{_stash::StringStash, _term::IndexedTerm};

/// Common attributes for all triple/quad sources of the parsers in this crate.
#[derive(Clone, Debug)]
pub(crate) struct Inner {
    pub line: usize,
    pub col: usize,
    pub version: Version,
    pub buffers: StringStash,
    pub terms: Vec<IndexedTerm>,
}

impl Default for Inner {
    fn default() -> Self {
        Self {
            line: 0,
            col: 0,
            version: Version::default(),
            terms: Vec::with_capacity(3),
            buffers: StringStash::with_capacity(3),
        }
    }
}
