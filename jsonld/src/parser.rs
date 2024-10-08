//! A JSON-LD parser based on Thimothée Haudebourg's [`json_ld`] crate.

use std::{io::BufRead, sync::Arc};

use json_ld::{JsonLdProcessor, RemoteDocument, ToRdfError};
use json_syntax::{Parse, Value};
use locspan::{Location, Span};
use sophia_api::{prelude::QuadParser, quad::Spog};
use sophia_iri::Iri;

use crate::{
    loader::NoLoader,
    loader_factory::{DefaultLoaderFactory, LoaderFactory},
    vocabulary::{ArcIri, ArcVoc},
    JsonLdOptions,
};

mod adapter;
use adapter::convert_quad;
pub use adapter::RdfTerm;

mod source;
pub use source::JsonLdQuadSource;

#[cfg(test)]
mod test;

/// A JSON-LD parser.
///
/// ## Developers
///
/// * the generic parameter `L` is the type of the [document loader](json_ld::Loader)
///   (determined by the `options` parameters)
///
/// ## WebAssembly
///
/// In some situations,
/// the methods from the [`QuadParser`] trait panic when executed as WebAssembly
/// (this is not the case when using the [`NoLoader`],
/// but occurs when using the [`HttpLoader`](crate::loader::HttpLoader)).
/// Those panics can be avoided by using the [`JsonLdParser::async_parse_str`] method.
pub struct JsonLdParser<LF = DefaultLoaderFactory<NoLoader>> {
    options: JsonLdOptions<LF>,
}

impl Default for JsonLdParser<DefaultLoaderFactory<NoLoader>> {
    fn default() -> Self {
        Self::new()
    }
}

impl JsonLdParser<DefaultLoaderFactory<NoLoader>> {
    /// Make a new [`JsonLdParser`] with the default options
    #[must_use] pub fn new() -> Self {
        Self {
            options: JsonLdOptions::default(),
        }
    }
}

impl<LF> JsonLdParser<LF> {
    /// Make a new [`JsonLdParser`] with the given options
    pub const fn new_with_options(options: JsonLdOptions<LF>) -> Self {
        Self { options }
    }

    /// Borrow the options of this parser
    pub const fn options(&self) -> &JsonLdOptions<LF> {
        &self.options
    }

    /// Parse (as RDF) a pre-parsed (as JSON) document
    pub async fn parse_json(&self, data: &RemoteDocument<ArcIri>) -> JsonLdQuadSource
    where
        LF: LoaderFactory,
    {
        let gen_loc = Location::new(
            Iri::new_unchecked(Arc::from("x-bnode-gen://")),
            Span::default(),
        );
        let mut generator = rdf_types::generator::Blank::new().with_metadata(gen_loc);
        let mut loader = self.options.document_loader();
        let mut vocab = ArcVoc {};
        let options = self.options.inner().clone();
        match data
            .to_rdf_with_using(&mut vocab, &mut generator, &mut loader, options)
            .await
        {
            Err(ToRdfError::Expand(err)) => JsonLdQuadSource::from_err(err),
            Ok(mut to_rdf) => JsonLdQuadSource::Quads(
                to_rdf
                    .cloned_quads()
                    .map(convert_quad)
                    .collect::<Vec<Spog<RdfTerm>>>()
                    .into_iter(),
            ),
        }
    }

    /// Parse (as RDF) a JSON-LD string, asynchronously
    pub async fn async_parse_str<'t>(&self, txt: &'t str) -> JsonLdQuadSource
    where
        LF: LoaderFactory,
    {
        let base = self
            .options()
            .base()
            .unwrap_or(Iri::new_unchecked_const("x-string://"))
            .map_unchecked(Arc::from);
        let json_res = Value::parse_str(txt, |span| Location::new(base.clone(), span));
        let json = match json_res {
            Ok(json) => json,
            Err(err) => {
                return JsonLdQuadSource::from_err(err);
            }
        };
        let doc = RemoteDocument::new(Some(base), None, json);
        self.parse_json(&doc).await
    }
}

impl<B: BufRead, LF> QuadParser<B> for JsonLdParser<LF>
where
    LF: LoaderFactory,
{
    type Source = JsonLdQuadSource;

    fn parse(&self, mut data: B) -> Self::Source {
        let mut bytes = vec![];
        if let Err(err) = data.read_to_end(&mut bytes) {
            return JsonLdQuadSource::from_err(err);
        };
        let txt = match String::from_utf8(bytes) {
            Ok(txt) => txt,
            Err(err) => return JsonLdQuadSource::from_err(err),
        };
        self.parse_str(&txt)
    }

    fn parse_str<'t>(&self, txt: &'t str) -> Self::Source
    where
        &'t str: sophia_api::parser::IntoParsable<Target = B>,
    {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Could not build tokio runtime");
        rt.block_on(self.async_parse_str(txt))
    }
}

sophia_api::def_mod_functions_for_bufread_parser!(JsonLdParser, QuadParser);
