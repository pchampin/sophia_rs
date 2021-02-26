//! Convert RDF on stdin from one format to another
use resiter::map::Map;
use sophia::serializer::TripleSerializer;
use sophia::term::{BoxTerm, CopyTerm};
use sophia::triple::{stream::TripleSource, Triple};
use std::convert::Infallible;
use std::io;
use std::iter::IntoIterator;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 3 {
        let message = if args.len() < 3 {
            "too few arguments"
        } else {
            "too many arguments"
        };
        exit_printing_usage(&args, message, Status::ArgNumber);
    }
    match args[2].as_str() {
        "nt" => {
            let mut ser = sophia::serializer::nt::NtSerializer::new(io::stdout());
            ser.serialize_triples(get_triples(&args))?;
        }
        "turtle" => {
            let mut ser = sophia::serializer::turtle::TurtleSerializer::new(io::stdout());
            ser.serialize_triples(get_triples(&args))?;
        }
        #[cfg(feature = "xml")]
        "xml" => {
            let mut ser = sophia::serializer::xml::RdfXmlSerializer::new(io::stdout());
            ser.serialize_triples(get_triples(&args))?;
        }
        #[cfg(not(feature = "xml"))]
        "xml" => {
            exit_printing_usage(
                &args,
                "RDF/XML not supported (recompile with --feature xml)",
                Status::UnsupportedXml,
            );
        }
        _ => {
            exit_printing_usage(&args, "unknown output format", Status::InputFormat);
        }
    }
    Ok(())
}

fn get_triples(args: &[String]) -> MyTripleSource {
    let bufread = io::BufReader::new(io::stdin());
    match args[1].as_str() {
        "nt" => wrap(sophia::parser::nt::parse_bufread(bufread)),
        "turtle" => wrap(sophia::parser::turtle::parse_bufread(bufread)),
        #[cfg(feature = "xml")]
        "xml" => wrap(sophia::parser::xml::parse_bufread(bufread)),
        #[cfg(not(feature = "xml"))]
        "xml" => {
            exit_printing_usage(
                &args,
                "RDF/XML not supported (recompile with --feature xml)",
                Status::UnsupportedXml,
            );
        }
        _ => {
            exit_printing_usage(&args, "unknown output format", Status::OutputFormat);
        }
    }
}

fn wrap<TS: TripleSource + 'static>(ts: TS) -> MyTripleSource {
    Box::new(
        ts.map_triples(|t| {
            [
                BoxTerm::copy(t.s()),
                BoxTerm::copy(t.p()),
                BoxTerm::copy(t.o()),
            ]
        })
        .into_iter()
        .map_err(|e| {
            eprintln!("error while parsing: {}", e);
            std::process::exit(Status::ParseError as i32)
        }),
    )
}

type MyTripleSource = Box<dyn Iterator<Item = Result<[BoxTerm; 3], Infallible>>>;

fn exit_printing_usage(args: &[String], message: &str, status: Status) -> ! {
    if message.len() > 0 {
        eprintln!("{}", message);
    }
    eprintln!("usage: {} <format-in> <format-out>", args[0]);
    eprintln!("  convert RDF on STDIN from one format to another,");
    eprintln!("  where format can be:");
    eprintln!("    - nt");
    eprintln!("    - turtle");
    #[cfg(feature = "xml")]
    eprintln!("    - xml");
    std::process::exit(status as i32)
}

enum Status {
    ArgNumber = 1,
    InputFormat = 2,
    OutputFormat = 3,
    ParseError = 4,
    #[allow(dead_code)]
    UnsupportedXml = 5,
}
