use sophia::api::prelude::*;
use sophia::sparql::SparqlWrapper;

use sophia_api::sparql::SparqlResult;
use sophia_inmem::dataset::LightDataset;

use sophia_turtle::serializer::turtle::{TurtleConfig, TurtleSerializer};

/// Expect a SPARQL query as argument; display its algebra and execute it on an empty dataset.
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let ds = LightDataset::new();
    let sds = SparqlWrapper(&ds);

    let query = sds.prepare_query_with(
        &std::env::args().nth(1).unwrap(),
        Iri::new_unchecked("https://example.org/base/"),
    )?;
    println!("{query:#?}");
    let res = sds.query(&query)?;
    match res {
        SparqlResult::Bindings(bs) => {
            let header = bs.variables().join("\t");
            // header (variable names)
            println!("{header}");
            // results
            for b in bs.into_iter() {
                for bv in b? {
                    print!("{}\t", bv.map(|t| t.to_string()).unwrap_or("_".into()));
                }
                println!();
            }
            // footer (repeat variables, and make it easier to spot an empty line in the results)
            println!("#{header}");
        }
        SparqlResult::Boolean(ans) => println!("{ans}"),
        SparqlResult::Triples(mut triples) => {
            let config = TurtleConfig::new().with_pretty(true);
            let mut serializer = TurtleSerializer::new_with_config(std::io::stdout(), config);
            serializer.serialize_triples(&mut triples)?;
        }
    }
    Ok(())
}
