use iref::{Iri, IriBuf};
use json_ld::{LoadError, Loader, RemoteDocument};
use json_syntax::Parse;
use std::fs::File;
use std::io::{BufReader, Read};
use url::Url;

/// File-URL loader.
///
/// This is a special JSON-LD document loader that can load document from any file: URL.
///
/// Loaded documents are not cached: a new file system read is made each time
/// an URL is loaded even if it has already been queried before.
#[derive(Clone, Copy, Debug, Default)]
pub struct FileUrlLoader {}

impl Loader for FileUrlLoader {
    async fn load(&self, url: &Iri) -> Result<RemoteDocument<IriBuf>, LoadError> {
        let url_str = url.as_str();
        if !url_str.starts_with("file:") {
            return Err(LoadError::new(
                url.to_owned(),
                FileUrlLoaderError::NotFileUrl(url_str.to_string()),
            ));
        }
        let url_parsed = Url::parse(url_str)
            .map_err(|e| LoadError::new(url.to_owned(), FileUrlLoaderError::InvalidUrl(e)))?;
        let path = url_parsed.to_file_path().map_err(|()| {
            LoadError::new(
                url.to_owned(),
                FileUrlLoaderError::BadFileUrl(url_str.to_string()),
            )
        })?;
        let file = File::open(path)
            .map_err(|e| LoadError::new(url.to_owned(), FileUrlLoaderError::IO(e)))?;
        let mut buf_reader = BufReader::new(file);
        let mut contents = String::new();
        buf_reader
            .read_to_string(&mut contents)
            .map_err(|e| LoadError::new(url.to_owned(), FileUrlLoaderError::IO(e)))?;
        let (doc, _) = json_syntax::Value::parse_str(&contents).map_err(|e| {
            LoadError::new(url.to_owned(), FileUrlLoaderError::Parse(format!("{e}")))
        })?;
        Ok(RemoteDocument::new(
            Some(url.to_owned()),
            Some("application/ld+json".parse().unwrap()),
            doc,
        ))
    }
}

impl FileUrlLoader {
    /// Creates a new file system loader.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

/// Loading error.
#[derive(Debug, thiserror::Error)]
pub enum FileUrlLoaderError {
    /// URL parse error
    #[error("{0}")]
    InvalidUrl(url::ParseError),

    /// URL is not a file
    #[error("Not a file: URL: {0}")]
    NotFileUrl(String),

    /// file: URL does not encode a correct path
    #[error("Invalid path in {0}")]
    BadFileUrl(String),

    /// IO error.
    #[error("{0}")]
    IO(std::io::Error),

    /// Parse error.
    #[error("parse error: {0}")]
    Parse(String),
}
