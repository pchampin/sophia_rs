//! Read a Dataset serialized in [N-Quads] from the standart input,
//! and write back to the standatd output its canonical form,
//! using the [RDFC-1.0] canonicalization algorithm.
//!
//! Parameters of the RDFC-1.0 can be provided via the following environment variables:
//! * SOPHIA_RDFC10_DEPTH_FACTOR
//! * SOPHIA_RDFC10_PERMUTATION_LIMIT
//!
//! [N-Quads]: https://www.w3.org/TR/n-quads/
//! [RDFC-1.0]: https://www.w3.org/TR/rdf-canon/

use std::env::{var, VarError::*};
use std::io::{stdin, stdout, BufReader, BufWriter};

use sophia::api::prelude::*;
use sophia::api::quad::Spog;
use sophia::api::term::SimpleTerm;
use sophia::c14n::rdfc10;
use sophia::turtle::parser::nq;
use sophia_c14n::hash::Sha256;
use sophia_c14n::rdfc10::{DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = BufReader::new(stdin());
    let dataset: MyDataset = nq::parse_bufread(input).collect_quads()?;
    let output = BufWriter::new(stdout());
    let depth_factor = match var("SOPHIA_RDFC10_DEPTH_FACTOR") {
        Ok(txt) => txt
            .parse()
            .expect("SOPHIA_RDFC10_DEPTH_FACTOR is not a valid f32"),
        Err(NotPresent) => DEFAULT_DEPTH_FACTOR,
        Err(other) => return Err(other.into()),
    };
    let permutation_limit = match var("SOPHIA_RDFC10_PERMUTATION_LIMIT") {
        Ok(txt) => txt
            .parse()
            .expect("SOPHIA_RDFC10_PERMUTATION_LIMIT is not a valid usize"),
        Err(NotPresent) => DEFAULT_PERMUTATION_LIMIT,
        Err(other) => return Err(other.into()),
    };
    // TODO make it possible to select another hash function
    rdfc10::normalize_with::<Sha256, _, _>(&dataset, output, depth_factor, permutation_limit)?;
    Ok(())
}

type MyDataset = std::collections::HashSet<Spog<SimpleTerm<'static>>>;
