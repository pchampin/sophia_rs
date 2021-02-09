//! Convert turtle on stdin into another format
use sophia::serializer::TripleSerializer;
use sophia::triple::stream::TripleSource;
use std::io;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    match args.get(1).map(String::as_str).unwrap_or("") {
        "nt" => {
            let mut ser = sophia::serializer::nt::NtSerializer::new(io::stdout());
            ser.serialize_triples(get_triples())?;
        }
        "turtle" => {
            let mut ser = sophia::serializer::turtle::TurtleSerializer::new(io::stdout());
            ser.serialize_triples(get_triples())?;
        }
        #[cfg(feature = "xml")]
        "xml" => {
            let mut ser = sophia::serializer::xml::RdfXmlSerializer::new(io::stdout());
            ser.serialize_triples(get_triples())?;
        }
        _ => {
            #[cfg(feature = "xml")]
            eprintln!("usage: {} [nt|turtle|xml]", args[0]);
            #[cfg(not(feature = "xml"))]
            eprintln!("usage: {} [nt|turtle]", args[0]);
            std::process::exit(1);
        }
    }
    Ok(())
}

fn get_triples() -> impl TripleSource {
    let bufread = io::BufReader::new(io::stdin());
    sophia::parser::turtle::parse_bufread(bufread)
}
