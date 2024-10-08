use super::{Arc, Iri, Value};
use json_ld::future::{BoxFuture, FutureExt};
use json_ld::{Loader, RemoteDocument};
use json_syntax::Parse;
use locspan::{Location, Meta};
use rdf_types::IriVocabulary;
use std::fmt;
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

impl Loader<Iri<Arc<str>>, Location<Iri<Arc<str>>>> for FileUrlLoader {
    type Output = Value<Location<Iri<Arc<str>>>>;
    type Error = FileUrlLoaderError;

    fn load_with<'a>(
        &'a mut self,
        vocabulary: &'a mut (impl Sync + Send + IriVocabulary<Iri = Iri<Arc<str>>>),
        url: Iri<Arc<str>>,
    ) -> BoxFuture<
        'a,
        Result<
            RemoteDocument<Iri<Arc<str>>, Location<Iri<Arc<str>>>, Value<Location<Iri<Arc<str>>>>>,
            Self::Error,
        >,
    >
    where
        Iri<Arc<str>>: 'a,
    {
        async move {
            let iri = vocabulary.iri(&url).unwrap();
            let url_str = iri.as_str();
            if !url_str.starts_with("file:") {
                return Err(Self::Error::NotFileUrl(url_str.into()));
            }
            let url_parsed = Url::parse(url_str).map_err(Self::Error::InvalidUrl)?;
            let path = url_parsed
                .to_file_path()
                .map_err(|()| Self::Error::BadFileUrl(url_str.into()))?;
            let file = File::open(path).map_err(Self::Error::IO)?;
            let mut buf_reader = BufReader::new(file);
            let mut contents = String::new();
            buf_reader
                .read_to_string(&mut contents)
                .map_err(Self::Error::IO)?;
            let doc = json_syntax::Value::parse_str(contents.as_str(), |span| {
                locspan::Location::new(url.clone(), span)
            })
            .map_err(Self::Error::Parse)?;
            Ok(RemoteDocument::new(
                Some(url),
                Some("application/ld+json".parse().unwrap()),
                doc,
            ))
        }
        .boxed()
    }
}

impl FileUrlLoader {
    /// Creates a new file system loader with the given content `parser`.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

/// Loading error.
#[derive(Debug)]
pub enum FileUrlLoaderError {
    /// URL parse error
    InvalidUrl(url::ParseError),

    /// URL is not a file
    NotFileUrl(String),

    /// file: URL does not encode a correct path
    BadFileUrl(String),

    /// IO error.
    IO(std::io::Error),

    /// Parse error.
    Parse(JsonParseError),
}

impl fmt::Display for FileUrlLoaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidUrl(e) => e.fmt(f),
            Self::NotFileUrl(url) => write!(f, "Not a file: URL: {url}"),
            Self::BadFileUrl(url) => write!(f, "Invalid path in {url}"),
            Self::IO(e) => e.fmt(f),
            Self::Parse(e) => e.fmt(f),
        }
    }
}

type JsonParseError =
    Meta<json_syntax::parse::Error<Location<Iri<Arc<str>>>>, Location<Iri<Arc<str>>>>;
