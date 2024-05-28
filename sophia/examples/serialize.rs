//! Read a graph or a dataset from the standard input in [N-Triples]/[N-Quads],
//! and serialize it back to the format specifed in the first argument.
//!
//! Recognized formats are:
//! - [`ntriples`](https://www.w3.org/TR/n-triples/) (alias `nt`)
//! - [`turtle`](https://www.w3.org/TR/turtle/) (alias `ttl`)
//! - [`nquads`](https://www.w3.org/TR/n-quads/) (alias `nq`)
//! - [`trig`](https://www.w3.org/TR/trig/)
//! - [`jsonld`](https://www.w3.org/TR/json-ld11) (if compiled witht the `jsonld` feature)
//! - [`rdfxml`](https://www.w3.org/TR/rdf-syntax-grammar) (if compiled witht the `xml` feature, alias `rdf`)
//!
//! NB: if the input is a dataset with named graphs,
//! and the ouput format is a graph format,
//! then only the default graph is serialized.
//!
//! [N-Triples]: https://www.w3.org/TR/n-triples/
//! [N-Quads]: https://www.w3.org/TR/n-quads/

use std::io::{stdin, stdout, BufReader, BufWriter};

use sophia::api::prelude::*;
use sophia::api::source::StreamError::{SinkError, SourceError};
#[cfg(feature = "jsonld")]
use sophia::jsonld::{serializer::JsonLdSerializer, JsonLdOptions};
use sophia::turtle::parser::gnq;
use sophia::turtle::serializer::{
    nq::NqSerializer,
    nt::NtSerializer,
    trig::{TrigConfig, TrigSerializer},
    turtle::{TurtleConfig, TurtleSerializer},
};
#[cfg(feature = "xml")]
use sophia::xml::serializer::{RdfXmlConfig, RdfXmlSerializer};

fn main() {
    let input = BufReader::new(stdin());
    let quad_source = gnq::parse_bufread(input);
    let out = BufWriter::new(stdout());
    let pretty: bool = std::env::var("SOPHIA_PRETTY")
        .unwrap_or_else(|_| "false".into())
        .parse()
        .unwrap();

    let format = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "trig".to_string());
    let res = match &format[..] {
        "ntriples" | "nt" => serialize_triples(quad_source, NtSerializer::new(out)),
        "turtle" | "ttl" => {
            let config = TurtleConfig::new().with_pretty(pretty);
            let ser = TurtleSerializer::new_with_config(out, config);
            serialize_triples(quad_source, ser)
        }
        "nquads" | "nq" => serialize_quads(quad_source, NqSerializer::new(out)),
        "trig" => {
            let config = TrigConfig::new().with_pretty(pretty);
            let ser = TrigSerializer::new_with_config(out, config);
            serialize_quads(quad_source, ser)
        }
        #[cfg(feature = "jsonld")]
        "json-ld" | "jsonld" => serialize_quads(
            quad_source,
            JsonLdSerializer::new_with_options(out, JsonLdOptions::new().with_spaces(2)),
        ),
        #[cfg(feature = "xml")]
        "rdfxml" | "rdf" => {
            let indent = if pretty { 4 } else { 0 };
            let config = RdfXmlConfig::new().with_identation(indent);
            let ser = RdfXmlSerializer::new_with_config(out, config);
            serialize_triples(quad_source, ser)
        }
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

fn serialize_triples<Q: QuadSource, S: TripleSerializer>(
    quad_source: Q,
    mut ser: S,
) -> Result<(), String> {
    let triple_source = quad_source.filter_quads(|q| q.g().is_none()).to_triples();
    match ser.serialize_triples(triple_source) {
        Ok(_) => Ok(()),
        Err(SourceError(e)) => Err(format!("Error while parsing input: {}", e)),
        Err(SinkError(e)) => Err(format!("Error while serializing triples: {}", e)),
    }
}

fn serialize_quads<Q: QuadSource, S: QuadSerializer>(
    quad_source: Q,
    mut ser: S,
) -> Result<(), String> {
    match ser.serialize_quads(quad_source) {
        Ok(_) => Ok(()),
        Err(SourceError(e)) => Err(format!("Error while parsing input: {}", e)),
        Err(SinkError(e)) => Err(format!("Error while serializing quads: {}", e)),
    }
}
