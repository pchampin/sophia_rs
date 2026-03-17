use sophia_api::version::Version;

use crate::{_stash::StringStash, _term::IndexedTerm};

/// Common attributes for all triple/quad sources of the parsers in this crate.
#[derive(Clone, Debug)]
pub(crate) struct Inner {
    pub line: usize,
    pub col: usize,
    pub version: Version,
    pub bn_suffix: String,
    pub buffers: StringStash,
    pub terms: Vec<IndexedTerm>,
}

impl Default for Inner {
    fn default() -> Self {
        Self::new(Version::default(), true)
    }
}

impl Inner {
    fn make_bn_suffix(preserve: bool) -> String {
        if preserve {
            "".into()
        } else {
            let mut bnode_suffix = vec![b'_'; uuid::fmt::Simple::LENGTH + 1];
            uuid::Uuid::now_v7()
                .simple()
                .encode_lower(&mut bnode_suffix[1..]);
            unsafe {
                // SAFETY: uuids only contain ASCII characters
                String::from_utf8_unchecked(bnode_suffix)
            }
        }
    }

    pub(crate) fn new(version: Version, preserve_bn_labels: bool) -> Self {
        Self {
            line: 0,
            col: 0,
            version,
            bn_suffix: Self::make_bn_suffix(preserve_bn_labels),
            terms: Vec::with_capacity(3),
            buffers: StringStash::with_capacity(3),
        }
    }
}
