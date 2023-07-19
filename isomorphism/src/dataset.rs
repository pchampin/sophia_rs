use super::hash::*;
use super::iso_term::*;
use sophia_api::quad::{iter_spog, Quad};
use sophia_api::{
    dataset::{DTerm, Dataset},
    quad::Spog,
    source::{
        StreamError::{SinkError, SourceError},
        StreamResult,
    },
    term::Term,
};
use std::collections::{BTreeSet, HashMap};

/// Computes whether two datasets are isomorphic.
///
/// # Error
/// If an error occurs while traversing `d1`,
/// a [`SourceError`](sophia_api::source::StreamError::SourceError`) is returned.
///
/// /// If an error occurs while traversing `d2`,
/// a [`SinkError`](sophia_api::source::StreamError::SinkError`) is returned.
pub fn isomorphic_datasets<D1, D2>(d1: &D1, d2: &D2) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
{
    let mut d1 = prepare_dataset(d1).map_err(SourceError)?;
    let mut d2 = prepare_dataset(d2).map_err(SinkError)?;

    // Datasets must have the same size
    if d1.len() != d2.len() {
        return Ok(false);
    }
    //println!("=== same length");

    // Datasets must have the same quads (regardless of blank node identity)
    d1.sort_unstable();
    d2.sort_unstable();
    if !d1.iter().zip(d2.iter()).all(cmp_quads) {
        return Ok(false);
    }
    //println!("=== same quads (regardless of bnodes");
    //println!("== d1 {:#?}", &d1);
    //println!("== d2 {:#?}", &d2);

    // Compare bnodes
    let b2q1 = make_b2q_map(&d1);
    let b2q2 = make_b2q_map(&d2);

    // Datasets must have the same number of distinct blank nodes
    if b2q1.len() != b2q2.len() {
        return Ok(false);
    }
    //println!("=== same quads (same number of distinct bnodes)");
    //println!("== b2q1 {:?}", &b2q1);
    //println!("== b2q2 {:?}", &b2q2);

    // Make initial "hashes" based on the number of quads
    // (this might help disambiguate bnodes faster)
    let mut map1: HashMap<_, _> = b2q1
        .iter()
        .map(|(k, v)| (k.as_str(), v.len() as u64))
        .collect();
    let mut map2: HashMap<_, _> = b2q2
        .iter()
        .map(|(k, v)| (k.as_str(), v.len() as u64))
        .collect();
    let mut old1 = 0;
    let mut old2 = 0;
    let mut eqcl1;
    let mut eqcl2;
    loop {
        map1 = make_map(&d1, &b2q1, &map1);
        map2 = make_map(&d2, &b2q2, &map2);
        //println!("== map1 {:?}", &map1);
        //println!("== map2 {:?}", &map2);
        eqcl1 = make_equivalence_classes(&map1);
        eqcl2 = make_equivalence_classes(&map2);
        //println!("== eqcl1 {:?}", &eqcl1);
        //println!("== eqcl2 {:?}", &eqcl2);
        if eqcl1.len() == old1 && eqcl2.len() == old2 {
            break;
        }
        old1 = eqcl1.len();
        old2 = eqcl2.len();
        if old1 == map1.len() && old2 == map2.len() {
            break;
        }
    }
    Ok(eqcl1 == eqcl2)
}

fn prepare_dataset<D: Dataset>(d: &D) -> Result<PreparedDataset<D>, D::Error> {
    d
        .quads()
        .map(|res| res.map(|q| {
            let (spo, g) = q.to_spog();
            (spo.map(IsoTerm), g.map(IsoTerm))
        }))
        .collect()
}

type PreparedDataset<'a, D> = Vec<Spog<IsoTerm<DTerm<'a, D>>>>;

fn make_b2q_map<T: Term>(d: &[Spog<IsoTerm<T>>]) -> HashMap<String, BTreeSet<usize>> {
    let mut ret = HashMap::new();
    for (i, q) in d.quads().enumerate() {
        for t in iter_spog(q.unwrap()) {
            for c in t.constituents() {
                if let Some(bnid) = c.bnode_id() {
                    let s = ret.entry(bnid.to_string()).or_insert_with(BTreeSet::new);
                    s.insert(i);
                }
            }
        }
    }
    ret
}

fn make_map<'a, T: Term>(
    d: &[Spog<IsoTerm<T>>],
    b2q: &'a HashMap<String, BTreeSet<usize>>,
    map: &HashMap<&str, u64>,
) -> HashMap<&'a str, u64> {
    b2q.iter()
        .map(|(bnid, quads)| {
            let mut digest = 0_u64;
            for i in quads {
                digest ^= hash_quad_with(&d[*i], map, bnid);
            }
            (bnid.as_str(), digest)
        })
        .collect()
}

fn make_equivalence_classes(map: &HashMap<&str, u64>) -> HashMap<u64, usize> {
    let mut ret = HashMap::new();
    for (_, v) in map.iter() {
        let n = ret.entry(*v).or_insert(0);
        *n += 1;
    }
    ret
}
