//! Convert a JSON-LD file to TriG, using an optional external context.
//!
//! usage: cargo run --example jsonld-context <JSON-LD file> [context file]
//!
//! Thanks to Jos van den Oever for this example.

use sophia::{
    api::{
        parser::QuadParser,
        serializer::{QuadSerializer, Stringifier},
        source::QuadSource,
    },
    jsonld::{JsonLdOptions, JsonLdParser, Policy},
    turtle::serializer::trig::{TrigConfig, TrigSerializer},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut args = std::env::args();
    let json_ld_path = args.nth(1).expect("Missing jsonld file.");
    let context_path = args.next();
    if let Some(context_path) = &context_path {
        eprintln!(
            "Loading {} with @context from {}",
            json_ld_path, context_path
        );
    } else {
        eprintln!("Loading {}", json_ld_path);
    }
    let json_str = std::fs::read_to_string(&json_ld_path)
        .unwrap_or_else(|e| panic!("Could not read file {}: {}", json_ld_path, e));

    let mut options = JsonLdOptions::new().with_expansion_policy(Policy::Standard);
    if let Some(context_path) = context_path {
        let context_str = std::fs::read_to_string(&context_path)
            .map_err(|e| format!("Could not read file {}: {}", &context_path, e))?;
        options = options.try_with_expand_context(context_str.as_str())?;
    }
    let parser = JsonLdParser::new_with_options(options);
    let quads = parser.parse_str(&json_str);
    let trig = to_trig(quads)?;
    println!("{}", trig.as_str());
    Ok(())
}

fn to_trig(quads: impl QuadSource) -> Result<String, Box<dyn std::error::Error>> {
    let mut stringifier =
        TrigSerializer::new_stringifier_with_config(TrigConfig::new().with_pretty(true));
    stringifier.serialize_quads(quads)?;
    Ok(stringifier.to_string())
}
