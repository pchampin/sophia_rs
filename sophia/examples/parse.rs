//! Parse a graph or a dataset from the standard input,
//! in the format specified in the first argument,
//! and write it back in [N-Triples]/[N-Quads] to the standard output.
//!
//! Alternatively, the input file name can be provided as a second argument,
//! which will also set the base IRI to the corresponding file: URL.
//!
//! The base IRI can be overridden via the environment variable `SOPHIA_BASE`.
//!
//! Recognized formats are:
//! - [`ntriples`](https://www.w3.org/TR/n-triples/) (alias `nt`)
//! - [`turtle`](https://www.w3.org/TR/turtle/) (alias `ttl`)
//! - [`nquads`](https://www.w3.org/TR/n-quads/) (alias `nq`)
//! - [`trig`](https://www.w3.org/TR/trig/)
//! - `gnq` (Generalized [N-Quads](https://www.w3.org/TR/n-quads/))
//! - `gtrig` (Generalized [TriG](https://www.w3.org/TR/trig/), default)
//! - [`jsonld`](https://www.w3.org/TR/json-ld11) (if compiled with the `jsonld` feature)
//! - [`rdfxml`](https://www.w3.org/TR/rdf11-xml) (if compiled with the `xml` feature, alias `rdf`)
//! - [`guess`] try to guess syntax from filename
//!
//! [N-Triples]: https://www.w3.org/TR/n-triples/
//! [N-Quads]: https://www.w3.org/TR/n-quads/

use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Read, Stdin, stdin, stdout};

use sophia::api::prelude::*;
use sophia::api::source::StreamError::{SinkError, SourceError};
#[cfg(feature = "jsonld")]
use sophia::jsonld::{JsonLdOptions, JsonLdParser};
use sophia::turtle::parser::{
    gnq::GNQuadsParser, gtrig::GTriGParser, nq::NQuadsParser, nt::NTriplesParser, trig::TriGParser,
    turtle::TurtleParser,
};
use sophia::turtle::serializer::{nq::NQuadsSerializer, nt::NTriplesSerializer};
#[cfg(feature = "xml")]
use sophia::xml::parser::RdfXmlParser;

fn main() {
    let mut format = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "guess".to_string());
    let path = std::env::args().nth(2);
    if format == "guess" {
        let Some(filename) = &path else {
            eprintln!("Cannot guess format of stdin");
            std::process::exit(-2);
        };
        format = match filename.rsplit('.').next() {
            Some("nt") => "ntriples",
            Some("nq") => "nquads",
            Some("ttl") => "turtle",
            Some("trig") => "trig",
            Some("jsonld" | "json") => "jsonld",
            Some("rdf" | "xml") => "rdfxml",
            _ => {
                eprintln!("Cannot guess format of {filename}");
                std::process::exit(-3);
            }
        }
        .to_string();
    }

    let base = if let Some(iri) = std::env::var_os("SOPHIA_BASE") {
        let iri = iri
            .into_string()
            .expect("Invalid UTF-8 data in SOPHIA_BASE");
        Iri::new(Box::from(iri)).expect("Invalid IRI in SOPHIA_BASE")
    } else if let Some(path) = &path {
        let cwd = std::env::current_dir().expect("No current directory");
        let url = url::Url::from_file_path(cwd.join(path)).expect("Invalid path");
        Iri::new(url.to_string().into()).expect("Invalid file: IRI")
    } else {
        Iri::new("x-stdin://localhost/".into()).unwrap()
    };
    let input = Input::new(path);
    let res = match &format[..] {
        "ntriples" | "nt" => dump_triples(input, NTriplesParser::new()),
        "turtle" | "ttl" => dump_triples(
            input,
            TurtleParser::new().with_base(Some(base.to_iri_ref().to_base())),
        ),
        "nquads" | "nq" => dump_quads(input, NQuadsParser::new()),
        "trig" => dump_quads(
            input,
            TriGParser::new().with_base(Some(base.to_iri_ref().to_base())),
        ),
        "gnq" => dump_quads(input, GNQuadsParser::new()),
        "gtrig" => dump_quads(
            input,
            GTriGParser::new().with_base(Some(base.to_iri_ref().to_base())),
        ),
        #[cfg(feature = "jsonld")]
        "json-ld" | "jsonld" => {
            let options = JsonLdOptions::new().with_base(base.map_unchecked(std::sync::Arc::from));
            let loader_factory = sophia::jsonld::loader::FileUrlLoader::default;
            #[cfg(feature = "http_client")]
            let loader_factory = || {
                sophia::jsonld::loader::ChainLoader::new(
                    (loader_factory)(),
                    sophia::jsonld::loader::HttpLoader::default(),
                )
            };
            let options = options.with_document_loader_closure(loader_factory);
            dump_quads(input, JsonLdParser::new_with_options(options))
        }
        #[cfg(feature = "xml")]
        "rdfxml" | "rdf" => dump_triples(
            input,
            RdfXmlParser {
                base: Some(base.map_unchecked(String::from)),
            },
        ),
        _ => {
            eprintln!("Unrecognized format: {format}");
            std::process::exit(-1);
        }
    };
    if let Err(msg) = res {
        eprintln!("{msg}");
        std::process::exit(1);
    }
}

fn dump_triples<P: TripleParser<Input>>(input: Input, p: P) -> Result<(), String> {
    let triple_source = p.parse(input);

    let output = BufWriter::new(stdout());
    let mut ser = NTriplesSerializer::new(output);
    match ser.serialize_triples(triple_source) {
        Ok(_) => Ok(()),
        Err(SourceError(e)) => Err(format!("Error while parsing input: {e}")),
        Err(SinkError(e)) => Err(format!("Error while writing quads: {e}")),
    }
}

fn dump_quads<P: QuadParser<Input>>(input: Input, p: P) -> Result<(), String> {
    let quad_source = p.parse(input);

    let output = BufWriter::new(stdout());
    let mut ser = NQuadsSerializer::new(output);
    match ser.serialize_quads(quad_source) {
        Ok(_) => Ok(()),
        Err(SourceError(e)) => Err(format!("Error while parsing input: {e}")),
        Err(SinkError(e)) => Err(format!("Error while writing quads: {e}")),
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
            Self::Stdin(b) => b.read(buf),
            Self::File(b) => b.read(buf),
        }
    }
}

impl BufRead for Input {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        match self {
            Self::Stdin(b) => b.fill_buf(),
            Self::File(b) => b.fill_buf(),
        }
    }

    fn consume(&mut self, amt: usize) {
        match self {
            Self::Stdin(b) => b.consume(amt),
            Self::File(b) => b.consume(amt),
        }
    }
}
