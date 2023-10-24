use super::{util::*, *};
use crate::{Resource, ResourceError, TypedResource};
use sophia_api::graph::CollectibleGraph;
use sophia_api::parser::TripleParser;
use sophia_api::source::TripleSource;
use sophia_api::term::Term;
use sophia_iri::Iri;
use sophia_turtle::parser::{nt, turtle};
use std::borrow::Borrow;
use std::fmt::Debug;
use std::io;
use std::sync::Arc;

/// A loader resolves URLs into [`Resource`]s.
///
/// TODO:once async traits are stable, make this trait async
pub trait Loader: Sync + Sized {
    /// Get the representation of the resource identified by `iri`, with its content-type.
    ///
    /// NB: the content-type must not contain any parameter.
    fn get<T: Borrow<str>>(&self, iri: Iri<T>) -> Result<(Vec<u8>, String), LoaderError>;

    /// Get the RDF representation of the resource identified by `iri`.
    ///
    /// # Precondition
    /// `iri` must contain no fragment identifier.
    fn get_graph<G, T>(&self, iri: Iri<T>) -> Result<G, LoaderError>
    where
        T: Borrow<str>,
        G: CollectibleGraph,
    {
        debug_assert!(iri.as_str().find(|c| c == '#').is_none());
        let iri_str = iri.as_str();
        let (data, ctype) = self.get(iri.as_ref())?;
        let bufread = io::BufReader::new(&data[..]);
        match &ctype[..] {
            "text/turtle" => turtle::TurtleParser {
                base: Some(iri.as_ref().map_unchecked(|t| t.borrow().to_string())),
            }
            .parse(bufread)
            .collect_triples()
            .map_err(|err| LoaderError::ParseError(iri_buf(iri_str), Box::new(err))),

            "application/n-triples" => nt::NTriplesParser {}
                .parse(bufread)
                .collect_triples()
                .map_err(|err| LoaderError::ParseError(iri_buf(iri_str), Box::new(err))),

            #[cfg(feature = "jsonld")]
            "application/ld+json" => {
                use sophia_api::prelude::{Quad, QuadParser, QuadSource};
                use sophia_jsonld::{loader::ClosureLoader, JsonLdOptions, JsonLdParser};
                let options = JsonLdOptions::new()
                    .with_base(iri.as_ref().map_unchecked(|t| t.into()))
                    .with_document_loader(ClosureLoader::new(|url| {
                        let (content, ctype) = self.get(url).map_err(|e| e.to_string())?;
                        if ctype != "application/ld+json" {
                            return Err(format!("{url} is not JSON-LD: {ctype}"));
                        }
                        String::from_utf8(content).map_err(|e| e.to_string())
                    }));
                JsonLdParser::new_with_options(options)
                    .parse(bufread)
                    .filter_quads(|q| q.g().is_none())
                    .map_quads(Quad::into_triple)
                    .collect_triples()
                    .map_err(|err| LoaderError::ParseError(iri_buf(iri_str), Box::new(err)))
            }

            #[cfg(feature = "xml")]
            "application/rdf+xml" => sophia_xml::parser::RdfXmlParser {
                base: Some(iri.as_ref().map_unchecked(|t| t.borrow().to_string())),
            }
            .parse(bufread)
            .collect_triples()
            .map_err(|err| LoaderError::ParseError(iri_buf(iri_str), Box::new(err))),

            _ => Err(LoaderError::CantGuessSyntax(iri_buf(iri_str))),
        }
    }

    /// Get the resource identified by `iri`
    fn get_resource<T, G>(self: &Arc<Self>, iri: Iri<T>) -> Result<Resource<G, Self>, LoaderError>
    where
        T: Borrow<str> + Debug,
        G: CollectibleGraph + 'static,
    {
        let url: Box<str> = iri.as_str().split('#').next().unwrap().into();
        let url = Iri::new_unchecked(url);
        self.get_resource_from(iri, url)
    }

    /// Get the resource identified by `iri` as described at `url`
    fn get_resource_from<T, U, G>(
        self: &Arc<Self>,
        id: T,
        url: Iri<U>,
    ) -> Result<Resource<G, Self>, LoaderError>
    where
        T: Term,
        U: Borrow<str> + Debug,
        G: CollectibleGraph + 'static,
    {
        let base = url.as_str().split('#').next().unwrap();
        let base = Iri::new_unchecked(base);
        let g: G = self.get_graph(base)?;
        let base = Some(base.map_unchecked(String::from));
        Ok(Resource::new(id, base, Arc::new(g), self.clone()))
    }

    /// Get the resource identified by `iri`, typed as T
    fn get_typed<R, T, G>(self: &Arc<Self>, iri: Iri<T>) -> Result<R, R::Error>
    where
        R: TypedResource<G, Self>,
        T: Borrow<str> + Debug,
        G: CollectibleGraph + 'static,
    {
        self.get_resource(iri)
            .map_err(ResourceError::LoaderError)?
            .try_into()
    }

    /// Get the resource identified by `iri` as described at `url`, typed as T
    fn get_typed_from<R, T, U, G>(self: &Arc<Self>, id: T, url: Iri<U>) -> Result<R, R::Error>
    where
        R: TryFrom<Resource<G, Self>>,
        R::Error: From<ResourceError<G::Error>>,
        T: Term,
        U: Borrow<str> + Debug,
        G: CollectibleGraph + 'static,
        Self: 'static,
    {
        self.get_resource_from(id, url)
            .map_err(ResourceError::LoaderError)?
            .try_into()
    }
}
