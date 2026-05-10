use std::{
    borrow::Borrow,
    path::{Path, PathBuf},
};

use sophia_iri::Iri;

/// A document loader that can load documents from the file system,
/// by mapping directories to specific URLs.
///
/// See [`json_ld::FsLoader`]
#[derive(Default)]
pub struct FsLoader(json_ld::FsLoader);

impl FsLoader {
    /// Constructor
    pub fn new() -> Self {
        Self(json_ld::FsLoader::new())
    }

    /// Bind the given IRI prefix to the given path.
    ///
    /// Any document with an IRI matching the given prefix will be loaded from the referenced local directory.
    pub fn mount<T, P>(&mut self, url: Iri<T>, path: P)
    where
        T: Borrow<str> + ToString,
        P: AsRef<Path>,
    {
        let iribuf = unsafe {
            // SAFETY url is known to hold a valid IRI
            iref::IriBuf::new_unchecked(url.unwrap().to_string())
        };
        self.0.mount(iribuf, path)
    }

    /// Returns the local file path associated to the given url if any.
    pub fn filepath(&self, url: Iri<&str>) -> Option<PathBuf> {
        let iri = unsafe {
            // SAFETY url is known to hold a valid IRI
            iref::Iri::new_unchecked(url.as_str())
        };
        self.0.filepath(iri)
    }
}

impl json_ld::Loader for FsLoader {
    async fn load(
        &self,
        url: &iref::Iri,
    ) -> Result<json_ld::RemoteDocument<iref::IriBuf>, json_ld::LoadError> {
        self.0.load(url).await
    }
}
