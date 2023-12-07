//! Parse a graph or a dataset from the standard input,
//! in the format specified in the first argument,
//! and write it back in [N-Triples]/[N-Quads] to the standard output.
//!
//! Alternatively, the input file name can be provided as a second argument,
//! which will also set the base IRI to the corresponding file: URL.
//!
//! The base IRI can be overridden via the environment variable SOPHIA_BASE.
//!
//! Recognized formats are:
//! - [`ntriples`](https://www.w3.org/TR/n-triples/) (alias `nt`)
//! - [`turtle`](https://www.w3.org/TR/turtle/) (alias `ttl`)
//! - [`nquads`](https://www.w3.org/TR/n-quads/) (alias `nq`)
//! - [`trig`](https://www.w3.org/TR/trig/)
//! - `gnq` (Generalized [N-Quads](https://www.w3.org/TR/n-quads/))
//! - `gtrig` (Generalized [TriG](https://www.w3.org/TR/trig/), default)
//! - [`jsonld`](https://www.w3.org/TR/json-ld11) (if compiled witht the `jsonld` feature)
//! - [`rdfxml`](https://www.w3.org/TR/rdf-syntax-grammar) (if compiled witht the `xml` feature, alias `rdf`)
//!
//! [N-Triples]: https://www.w3.org/TR/n-triples/
//! [N-Quads]: https://www.w3.org/TR/n-quads/

use std::fs::File;
use std::io::{stdin, stdout, BufRead, BufReader, BufWriter, Read, Stdin};

use sophia::api::prelude::*;
use sophia::api::source::StreamError::{SinkError, SourceError};
#[cfg(feature = "jsonld")]
use sophia::jsonld::{JsonLdOptions, JsonLdParser};
use sophia::turtle::parser::{
    gnq::GNQuadsParser, gtrig::GTriGParser, nq::NQuadsParser, nt::NTriplesParser, trig::TriGParser,
    turtle::TurtleParser,
};
use sophia::turtle::serializer::{nq::NqSerializer, nt::NtSerializer};
#[cfg(feature = "xml")]
use sophia::xml::parser::RdfXmlParser;
use sophia_jsonld::loader_factory::{ClosureLoaderFactory, DefaultLoaderFactory, LoaderFactory};

fn main() {
    let format = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "gtrig".to_string());
    let path = std::env::args().nth(2);
    let base = Some(if let Some(iri) = std::env::var_os("SOPHIA_BASE") {
        let iri = iri
            .into_string()
            .expect("Invalid UTF-8 data in SOPHIA_BASE");
        Iri::new(iri).expect("Invalid IRI in SOPHIA_BASE")
    } else if let Some(path) = &path {
        let cwd = std::env::current_dir().expect("No current directory");
        let url = url::Url::from_file_path(cwd.join(path)).expect("Invalid path");
        Iri::new(url.into()).expect("Invalid file: IRI")
    } else {
        Iri::new_unchecked("x-stdin://localhost/".into())
    });
    let input = Input::new(path);
    let res = match &format[..] {
        "ntriples" | "nt" => dump_triples(input, NTriplesParser {}),
        "turtle" | "ttl" => dump_triples(input, TurtleParser { base }),
        "nquads" | "nq" => dump_quads(input, NQuadsParser {}),
        "trig" => dump_quads(input, TriGParser { base }),
        "gnq" => dump_quads(input, GNQuadsParser {}),
        "gtrig" => dump_quads(input, GTriGParser { base }),
        #[cfg(feature = "jsonld")]
        "json-ld" | "jsonld" => {
            let options = JsonLdOptions::new()
                .with_base(base.clone().unwrap().map_unchecked(std::sync::Arc::from));
            let loader_factory =
                DefaultLoaderFactory::<sophia::jsonld::loader::FileUrlLoader>::default();
            #[cfg(feature = "http_client")]
            let loader_factory = ClosureLoaderFactory::new(|| {
                sophia::jsonld::loader::ChainLoader::new(
                    loader_factory.yield_loader(),
                    sophia::jsonld::loader::HttpLoader::default(),
                )
            });
            let options = options.with_document_loader_factory(loader_factory);
            dump_quads(input, JsonLdParser::new_with_options(options))
        }
        #[cfg(feature = "xml")]
        "rdfxml" | "rdf" => dump_triples(input, RdfXmlParser { base }),
        _ => {
            eprintln!("Unrecognized format: {}", format);
            std::process::exit(-1);
        }
    };
    if let Err(msg) = res {
        eprintln!("{}", msg);
        std::process::exit(1);
    }
}

fn dump_triples<P: TripleParser<Input>>(input: Input, p: P) -> Result<(), String> {
    let triple_source = p.parse(input);

    let output = BufWriter::new(stdout());
    let mut ser = NtSerializer::new(output);
    match ser.serialize_triples(triple_source) {
        Ok(_) => Ok(()),
        Err(SourceError(e)) => Err(format!("Error while parsing input: {}", e)),
        Err(SinkError(e)) => Err(format!("Error while writing quads: {}", e)),
    }
}

fn dump_quads<P: QuadParser<Input>>(input: Input, p: P) -> Result<(), String> {
    let quad_source = p.parse(input);

    let output = BufWriter::new(stdout());
    let mut ser = NqSerializer::new(output);
    match ser.serialize_quads(quad_source) {
        Ok(_) => Ok(()),
        Err(SourceError(e)) => Err(format!("Error while parsing input: {}", e)),
        Err(SinkError(e)) => Err(format!("Error while writing quads: {}", e)),
    }
}

enum Input {
    Stdin(BufReader<Stdin>),
    File(BufReader<File>),
}

impl Input {
    fn new(path: Option<String>) -> Self {
        match path {
            None => Self::Stdin(BufReader::new(stdin())),
            Some(path) => Self::File(BufReader::new(File::open(path).expect("Can not open file"))),
        }
    }
}

impl Read for Input {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Input::Stdin(b) => b.read(buf),
            Input::File(b) => b.read(buf),
        }
    }
}

impl BufRead for Input {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        match self {
            Input::Stdin(b) => b.fill_buf(),
            Input::File(b) => b.fill_buf(),
        }
    }

    fn consume(&mut self, amt: usize) {
        match self {
            Input::Stdin(b) => b.consume(amt),
            Input::File(b) => b.consume(amt),
        }
    }
}
