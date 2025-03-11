use super::{
    Loader, LoaderError,
    util::{IriBuf, iri_buf},
};
use sophia_iri::Iri;
use std::borrow::Borrow;
use std::fmt::Debug;
use std::fs::read;
use std::io::ErrorKind as IoErrorKind;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// A resource loader using local versions of the resources.
///
/// It guesses content-type from the file extensions,
/// and emulates content-negotiation :
/// if a resource is not found, it will try adding a few well-known extensions to it.
#[derive(Clone, Debug, Default)]
pub struct LocalLoader {
    caches: Vec<(IriBuf, PathBuf)>,
}

impl LocalLoader {
    /// Constructor
    pub fn new(caches: Vec<(IriBuf, PathBuf)>) -> Result<Self, LocalLoaderError> {
        let checked_caches = caches
            .into_iter()
            .map(|(iri, path)| Self::check(iri, path))
            .collect::<Result<Vec<(IriBuf, PathBuf)>, LocalLoaderError>>()?;
        Ok(Self {
            caches: checked_caches,
        })
    }

    /// Add a local cache to this loader.
    ///
    /// # Pre-conditions
    ///
    /// * the IRI must end with a slash (`/`)
    /// * the path must identify an existing directory
    pub fn add(&mut self, iri: IriBuf, path: PathBuf) -> Result<(), LocalLoaderError> {
        self.caches.push(Self::check(iri, path)?);
        Ok(())
    }

    /// Wrap this loader into an `Arc<Loader>`.
    #[must_use]
    pub fn arced(self) -> Arc<Self> {
        Arc::new(self)
    }

    fn check(iri: IriBuf, path: PathBuf) -> Result<(IriBuf, PathBuf), LocalLoaderError> {
        if !iri.as_str().ends_with('/') {
            Err(LocalLoaderError::IriMustEndWithSlash(iri))
        } else if !path.is_absolute() {
            Err(LocalLoaderError::PathMustBeAbsolute(path))
        } else if !path.is_dir() {
            Err(LocalLoaderError::PathMustBeDirectory(path))
        } else {
            Ok((iri, path))
        }
    }

    fn ctype(&self, iri: &str) -> String {
        if iri.ends_with(".ttl") {
            "text/turtle".into()
        } else if iri.ends_with(".nt") {
            "application/n-triples".into()
        } else if cfg!(feature = "jsonld") && iri.ends_with(".jsonld") {
            "application/ld+json".into()
        } else if cfg!(feature = "xml") && iri.ends_with(".rdf") {
            "application/rdf+xml".into()
        } else {
            "application/octet-stream".into()
        }
    }
}

impl Loader for LocalLoader {
    /// Get the representation available t iri
    fn get<T: Borrow<str>>(&self, iri: Iri<T>) -> Result<(Vec<u8>, String), LoaderError> {
        let iri = iri.as_str().split('#').next().unwrap();
        for (ns, path) in &self.caches {
            if iri.starts_with(ns.as_str()) {
                let subpath = Path::new(&iri[ns.len()..]);
                let resource_path: PathBuf = path.join(subpath);
                return match read(resource_path) {
                    Ok(data) => Ok((data, self.ctype(iri))),
                    Err(e) if e.kind() == IoErrorKind::NotFound => {
                        // emulate conneg if there is no extension
                        let no_ext = iri.as_bytes()[iri.rfind(['.', '/']).unwrap_or(0)] != b'.';
                        if no_ext {
                            for ext in [
                                "ttl",
                                "nt",
                                #[cfg(feature = "jsonld")]
                                "jsonld",
                                #[cfg(feature = "xml")]
                                "rdf",
                            ] {
                                let alt = Iri::new_unchecked(format!("{iri}.{ext}"));
                                if let Ok(res) = self.get(alt) {
                                    return Ok(res);
                                }
                            }
                        }
                        Err(LoaderError::NotFound(iri_buf(iri)))
                    }
                    Err(e) => Err(LoaderError::IoError(iri_buf(iri), e)),
                };
            }
        }
        Err(LoaderError::UnsupportedIri(
            Iri::new_unchecked(iri.to_owned().into()),
            "no matching local path".into(),
        ))
    }
}

//

/// An error raised while constructing a [`LocalLoader`]
#[derive(Debug, thiserror::Error)]
pub enum LocalLoaderError {
    /// The IRIs mapped by [`LocalLoader`] must always end with a slash (`/`)
    #[error("Iri must end with a slash: {0:?}")]
    IriMustEndWithSlash(IriBuf),
    /// The local paths mapped by [`LocalLoader`] must always be absolute
    #[error("Path must be absolute: {0}")]
    PathMustBeAbsolute(PathBuf),
    /// The local paths mapped by [`LocalLoader`] must always identify an existing directory
    #[error("Path must be a directory: {0}")]
    PathMustBeDirectory(PathBuf),
}
