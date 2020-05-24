//! This module implements check for isomorphic blank node equivalence of RDF
//! datasets.
//!
//! It is publicly exported to `sophia::dataset`.

use crate::dataset::{DQuad, DTerm, Dataset};
use crate::graph::{bn_mapper, hash_if_not_bn, match_ignore_bns};
use crate::quad::Quad;
use crate::triple::stream::{
    SinkError, SinkResult as _, SourceError, SourceResult as _, StreamError, StreamResult,
};
use sophia_api::term::matcher::AnyOrExactly;
use sophia_api::term::{TTerm, TermKind};
use sophia_term::RefTerm;
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fmt;
use std::hash::{Hash, Hasher};

/// Maximal steps a dataset is traversed for proofing isomorphism.
/// If this bound is exceeded the algorithm assumes that the datasets are not
/// isomorphic.
pub const MAX_DISTANCE: usize = 8;

/// The hasher used internally for checking isomorphism.
pub type IsoHasher = std::collections::hash_map::DefaultHasher;

#[derive(Debug, Clone, Copy)]
struct AlgorithmFailure;

impl fmt::Display for AlgorithmFailure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to execute the Algorithm")
    }
}

impl Error for AlgorithmFailure {}

/// Checks if both datasets are isomorphic blank node equal.
///
/// According to the [RDF specs](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#graph-isomorphism)
/// this means that a mapping for blank nodes in `d1` exists so that `d1 == d2`.
///
/// The used algorithm was originally implemented for [`Oxigraph`](https://github.com/Tpt/oxigraph)
/// and is extended for the generalized RDF model of `sophia`.
///
/// # Errors
///
/// Both datasets may fail traversing (and this is done several times).
/// Accordingly, a `StreamError` returned where `SourceError`s originate from
/// `d1` and `SinkError`s originate from `d2`
///
/// # Performance
///
/// As this algorithm has to traverse each graph several times the algorithm
/// gets way more expensive with bigger numbers of quads. In the same way
/// the number of blank nodes contributes to the costs.
pub fn isomorphic_datasets<D1, D2>(d1: &D1, d2: &D2) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
    DTerm<D1>: Clone + Eq + Hash,
    DTerm<D2>: Clone + Eq + Hash,
{
    // quick return conditions
    // -----------------------
    let (min1, max1) = d1.quads().size_hint();
    let (min2, max2) = d2.quads().size_hint();
    if let Some(max1) = max1 {
        if max1 < min2 {
            return Ok(false);
        }
    }
    if let Some(max2) = max2 {
        if max2 < min1 {
            return Ok(false);
        }
    }

    // blank nodes in the respective datasets
    let bns1 = d1.bnodes().source_err()?;
    let bns2 = d2.bnodes().sink_err()?;

    if bns1.len() != bns2.len() {
        return Ok(false);
    }

    // check for same quads in both datasets
    // -------------------------------------
    // - regardless of blank nodes
    // - implicitly checks that d1 and d2 have the same length
    let d1_in_d2 = check_for_equal_quads_regardless_bns(d1, d2)?;
    let d2_in_d1 = check_for_equal_quads_regardless_bns(d2, d1).map_err(StreamError::reverse)?;

    if !(d1_in_d2 && d2_in_d1) {
        return Ok(false);
    }

    // Create hashes
    let bn_hashes1 = match calc_bn_hashes::<D1, IsoHasher>(d1) {
        Ok(map) => map,
        Err(SourceError(e)) => return Err(SourceError(e)),
        Err(SinkError(_)) => return Ok(false), // Not the best solution
    };
    let bn_hashes2 = match calc_bn_hashes::<D2, IsoHasher>(d2) {
        Ok(map) => map,
        Err(SourceError(e)) => return Err(SinkError(e)),
        Err(SinkError(_)) => return Ok(false), // Not the best solution
    };

    // Create mapping
    let mut bn_mapping = HashMap::new();
    for (hash, bns1) in bn_hashes1 {
        for bn1 in bns1 {
            let bn2 = match bn_hashes2.get(&hash) {
                Some(bn) => bn,
                None => return Ok(false), // No matching blank node in g2!
            };
            bn_mapping.insert(bn1, bn2);
        }
    }

    // Apply mapping
    isomorphic_datasets_with_mapping(d1, d2, bn_mapping)
}

/// Builds a `GraphNameMatcher` by using the blank node mapping provided.
///
/// If the given term is a blank node the matcher will match all possible
/// mappings for that blank node, i.e. included redundant blank nodes. If the
/// given term is not a blank node the matcher will only match the given term.
fn bn_mapper_for_gname<'m, T1, T2>(
    mapping: &'m HashMap<T1, &Vec<T2>>,
    g: Option<&'m T1>,
) -> Vec<Option<&'m dyn TTerm>>
where
    T1: TTerm + Hash + Eq,
    T2: TTerm + Hash + Eq,
{
    if g.map(TTerm::kind) == Some(TermKind::BlankNode) {
        match mapping.get(g.unwrap()) {
            None => vec![],
            Some(bns) => bns.iter().map(|n| Some(n.as_dyn())).collect(),
        }
    } else {
        vec![g.map(TTerm::as_dyn)]
    }
}

/// Checks for each quad in `d1` with at least one blank node if it is also
/// contained in `d2` if the blank node `mapping` is applied.
fn isomorphic_datasets_with_mapping<D1, D2>(
    d1: &D1,
    d2: &D2,
    mapping: HashMap<DTerm<D1>, &Vec<DTerm<D2>>>,
) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
    DTerm<D1>: Clone + Eq + Hash,
    DTerm<D2>: Clone + Eq + Hash,
{
    for q in d1.quads() {
        let q = q.source_err()?;

        if q.s().kind() == TermKind::BlankNode
            || q.p().kind() == TermKind::BlankNode
            || q.o().kind() == TermKind::BlankNode
            || q.g().map(TTerm::kind) == Some(TermKind::BlankNode)
        {
            let ms = bn_mapper(&mapping, q.s());
            let mp = bn_mapper(&mapping, q.p());
            let mo = bn_mapper(&mapping, q.o());
            let mg = bn_mapper_for_gname(&mapping, q.g());

            if d2.quads_matching(&ms, &mp, &mo, &mg).next().is_none() {
                return Ok(false);
            }
        }
    }

    Ok(true)
}

fn match_gname_ignore_bns<T>(t: Option<&T>) -> AnyOrExactly<Option<RefTerm>>
where
    T: TTerm + ?Sized,
{
    if t.map(TTerm::kind) == Some(TermKind::BlankNode) {
        AnyOrExactly::Any
    } else {
        AnyOrExactly::Exactly(t.map(RefTerm::from))
    }
}

/// Checks if each quad in `d1` is also in `d2` regardless of blank node labels.
fn check_for_equal_quads_regardless_bns<D1, D2>(
    d1: &D1,
    d2: &D2,
) -> StreamResult<bool, D1::Error, D2::Error>
where
    D1: Dataset,
    D2: Dataset,
{
    for q in d1.quads() {
        let q = q.source_err()?;

        let ms = match_ignore_bns(q.s());
        let mp = match_ignore_bns(q.p());
        let mo = match_ignore_bns(q.o());
        let mg = match_gname_ignore_bns(q.g());

        if d2.quads_matching(&ms, &mp, &mo, &mg).next().is_none() {
            return Ok(false);
        }
    }

    Ok(true)
}

/// Calculate a hash for each blank node.
///
/// The hash of a blank node in a dataset is the hash of all terms in the quads
/// in which the blank node occurs. Should this not be enough to create
/// distinct hashes, the dataset is further traversed starting from the initial
/// quads.
///
/// Blank nodes are not included in calculating the hashes.
///
/// The hashes are distinct if every 'bucket', i.e. the `Vec` in the returning
/// `HashMap` has only one element.
///
/// An exception are redundant blank nodes. If the algorithm detects such nodes
/// they will share the same hash.
fn calc_bn_hashes<D, H>(
    d: &D,
) -> StreamResult<HashMap<u64, Vec<DTerm<D>>>, D::Error, AlgorithmFailure>
where
    D: Dataset,
    DTerm<D>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut res_map = HashMap::new();
    let mut unresolved_map = HashMap::new();

    for bn in d.bnodes().source_err()?.into_iter() {
        let (hash, upstream, downstream) = calc_bns_init_hash::<D, H>(&bn, d).source_err()?;
        unresolved_map
            .entry(hash)
            .or_insert_with(Vec::new)
            .push((bn, upstream, downstream));
    }

    let mut last_map = unresolved_map;
    unresolved_map = HashMap::new();

    let mut i = 0;

    while !last_map.is_empty() && i < MAX_DISTANCE {
        for (hash, bns) in last_map.into_iter() {
            if bns.len() == 1 {
                // Distinct hash.
                let (bn, _, _) = bns.into_iter().next().expect("len == 1");
                res_map.insert(hash, vec![bn]);
            } else if bns
                .iter()
                .all(|(_, upstream, downstream)| upstream.is_empty() && downstream.is_empty())
            {
                // Can no longer traverse dataset to distinguish nodes, i.e. they must be redundant.
                let redundants = bns.into_iter().map(|(bn, _, _)| bn).collect();
                res_map.insert(hash, redundants);
            } else {
                // improve hash by further traversing.
                for (bn, upstream, downstream) in bns {
                    let (better_hash, upstream, downstream) =
                        improve_hash_by_increasing_distance::<H, D>(
                            hash,
                            &upstream,
                            &downstream,
                            d,
                        )
                        .source_err()?;
                    unresolved_map
                        .entry(better_hash)
                        .or_insert_with(Vec::new)
                        .push((bn, upstream, downstream));
                }
            }
        }

        last_map = unresolved_map;
        unresolved_map = HashMap::new();
        i += 1;
    }

    if i >= MAX_DISTANCE {
        return Err(AlgorithmFailure).sink_err();
    }

    Ok(res_map)
}

/// Calculate the blank node's initial hash in the dataset.
///
/// Returns the initial hash, the upstream nodes and the downstream nodes.
#[allow(clippy::type_complexity)]
fn calc_bns_init_hash<D, H>(
    bn: &DTerm<D>,
    d: &D,
) -> Result<(u64, Vec<DTerm<D>>, Vec<DTerm<D>>), D::Error>
where
    D: Dataset,
    DTerm<D>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    // for same hashing result we need to order the quads' hashes.
    let mut hashes = BTreeSet::new();

    let mut upstream = vec![];
    let mut downstream = vec![];

    for quad in d.quads() {
        let quad = quad?;
        if quad.s() == bn || quad.p() == bn || quad.o() == bn || quad.g() == Some(bn) {
            hashes.insert(hash_quad_without_bn::<H, DQuad<D>>(&quad));
            if quad.o() != bn {
                upstream.push(quad.o().clone())
            };
            if quad.s() != bn {
                downstream.push(quad.s().clone())
            };
        }
    }

    // hashing
    let mut hasher = H::default();
    hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

/// Improves an existing hash by further traversing the dataset.
#[allow(clippy::type_complexity)]
fn improve_hash_by_increasing_distance<H, D>(
    hash: u64,
    upstream: &[DTerm<D>],
    downstream: &[DTerm<D>],
    d: &D,
) -> Result<(u64, Vec<DTerm<D>>, Vec<DTerm<D>>), D::Error>
where
    D: Dataset,
    DTerm<D>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    // for same hashing result we need to order the quads' hashes.
    let mut hashes = BTreeSet::new();

    let upstream = traverse_from_s_to_o::<H, D>(upstream, d, &mut hashes)?;
    let downstream = traverse_from_o_to_s::<H, D>(downstream, d, &mut hashes)?;

    // hashing
    let mut hasher = H::default();
    // initialize with existing hash.
    hash.hash(&mut hasher);
    hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

fn hash_quad_without_bn<H, Q>(q: &Q) -> u64
where
    H: Hasher + Default,
    Q: Quad,
{
    let mut h = H::default();
    hash_if_not_bn(q.s(), &mut h);
    hash_if_not_bn(q.p(), &mut h);
    hash_if_not_bn(q.o(), &mut h);
    if let Some(g) = q.g() {
        hash_if_not_bn(g, &mut h)
    }
    h.finish()
}

/// Looks for quads where the given terms are objects.
/// Those quads' hashes are inserted into the list and a list of their
/// subjects is returned.
fn traverse_from_o_to_s<H, D>(
    objects: &[DTerm<D>],
    d: &D,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<DTerm<D>>, D::Error>
where
    D: Dataset,
    DTerm<D>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut subjects = vec![];
    for o in objects {
        for quad in d.quads_with_o(o) {
            let quad = quad?;
            hashes.insert(hash_quad_without_bn::<H, DQuad<D>>(&quad));
            subjects.push(quad.s().clone());
        }
    }
    Ok(subjects)
}

/// Looks for quads where the given terms are subjects.
/// Those quads' hashes are inserted into the list and a list of their
/// objects is returned.
fn traverse_from_s_to_o<H, D>(
    subjects: &[DTerm<D>],
    d: &D,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<DTerm<D>>, D::Error>
where
    D: Dataset,
    DTerm<D>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut objects = vec![];
    for s in subjects {
        for quad in d.quads_with_s(s) {
            let quad = quad?;
            hashes.insert(hash_quad_without_bn::<H, DQuad<D>>(&quad));
            objects.push(quad.o().clone());
        }
    }
    Ok(objects)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dataset::inmem::FastDataset;
    use crate::parser::{gtrig, nq};
    use crate::quad::stream::QuadSource;

    #[test]
    fn simple() -> Result<(), Box<dyn Error>> {
        let d1 = r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix dc: <http://purl.org/dc/terms/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            {
                <http://example.org/bob> dc:publisher "Bob" .
                <http://example.org/alice> dc:publisher "Alice" .
            }
            
            <http://example.org/bob>
            {
                _:a foaf:name "Bob" .
                _:a foaf:mbox <mailto:bob@oldcorp.example.org> .
                _:a foaf:knows _:b .
            }
            
            <http://example.org/alice>
            {
                _:b foaf:name "Alice" .
                _:b foaf:mbox <mailto:alice@work.example.org> .
            }
        "#;
        let d2 = r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix dc: <http://purl.org/dc/terms/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            {
                <http://example.org/bob> dc:publisher "Bob" .
                <http://example.org/alice> dc:publisher "Alice" .
            }
            
            <http://example.org/bob>
            {
                _:a2 foaf:name "Bob" .
                _:a2 foaf:mbox <mailto:bob@oldcorp.example.org> .
                _:a2 foaf:knows _:b2 .
            }
            
            <http://example.org/alice>
            {
                _:b2 foaf:name "Alice" .
                _:b2 foaf:mbox <mailto:alice@work.example.org> .
            }
        "#;
        let d3 = r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix dc: <http://purl.org/dc/terms/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            {
                <http://example.org/bob> dc:publisher "Bob" .
                <http://example.org/alice> dc:publisher "Alice" .
            }
            
            <http://example.org/bob>
            {
                _:a3 foaf:name "Bob" .
                _:a3 foaf:mbox <mailto:bob@oldcorp.example.org> .
                _:a3 foaf:knows _:b3 .
            }
            
            <http://example.org/alice>
            {
                _:c3 foaf:name "Alice" .
                _:c3 foaf:mbox <mailto:alice@work.example.org> .
            }
        "#;
        let d1: FastDataset = gtrig::parse_str(d1).collect_quads()?;
        let d2: FastDataset = gtrig::parse_str(d2).collect_quads()?;
        let d3: FastDataset = gtrig::parse_str(d3).collect_quads()?;

        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);
        assert!(!isomorphic_datasets(&d1, &d3)?);
        assert!(!isomorphic_datasets(&d2, &d3)?);

        Ok(())
    }

    #[test]
    fn different_parsers() -> Result<(), Box<dyn Error>> {
        let trig = r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix dc: <http://purl.org/dc/terms/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            {
                <http://example.org/bob> dc:publisher "Bob" .
                <http://example.org/alice> dc:publisher "Alice" .
            }
            
            <http://example.org/bob>
            {
                _:a foaf:name "Bob" .
                _:a foaf:mbox <mailto:bob@oldcorp.example.org> .
                _:a foaf:knows _:b .
            }
            
            <http://example.org/alice>
            {
                _:b foaf:name "Alice" .
                _:b foaf:mbox <mailto:alice@work.example.org> .
            }
        "#;
        let nq = r#"
            <http://example.org/bob> <http://purl.org/dc/terms/publisher> "Bob" .
            <http://example.org/alice> <http://purl.org/dc/terms/publisher> "Alice" .
        
            _:a2 <http://xmlns.com/foaf/0.1/name> "Bob" <http://example.org/bob> .
            _:a2 <http://xmlns.com/foaf/0.1/mbox> <mailto:bob@oldcorp.example.org> <http://example.org/bob> .
            _:a2 <http://xmlns.com/foaf/0.1/knows> _:b2 <http://example.org/bob> .

            _:b2 <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/alice> .
            _:b2 <http://xmlns.com/foaf/0.1/mbox> <mailto:alice@work.example.org> <http://example.org/alice> .
        "#;
        let trig: FastDataset = gtrig::parse_str(trig).collect_quads()?;
        let nq: FastDataset = nq::parse_str(nq).collect_quads()?;

        assert!(isomorphic_datasets(&nq, &trig)?);
        assert!(isomorphic_datasets(&trig, &nq)?);

        Ok(())
    }

    #[test]
    fn bn_names() -> Result<(), Box<dyn Error>> {
        let d1 = r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix dc: <http://purl.org/dc/terms/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            <http://example.org/publishers>
            {
                _:bob dc:publisher "Bob" .
                _:alice dc:publisher "Alice" .
            }
            
            _:bob
            {
                _:a foaf:name "Bob" .
                _:a foaf:mbox <mailto:bob@oldcorp.example.org> .
                _:a foaf:knows _:b .
            }
            
            _:alice
            {
                _:b foaf:name "Alice" .
                _:b foaf:mbox <mailto:alice@work.example.org> .
            }
        "#;
        let d2 = r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix dc: <http://purl.org/dc/terms/> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            
            <http://example.org/publishers>
            {
                _:bob2 dc:publisher "Bob" .
                _:alice2 dc:publisher "Alice" .
            }
            
            _:bob2
            {
                _:a2 foaf:name "Bob" .
                _:a2 foaf:mbox <mailto:bob@oldcorp.example.org> .
                _:a2 foaf:knows _:b2 .
            }
            
            _:alice2
            {
                _:b2 foaf:name "Alice" .
                _:b2 foaf:mbox <mailto:alice@work.example.org> .
            }
        "#;
        let d1: FastDataset = gtrig::parse_str(d1).collect_quads()?;
        let d2: FastDataset = gtrig::parse_str(d2).collect_quads()?;

        assert!(isomorphic_datasets(&d1, &d2)?);
        assert!(isomorphic_datasets(&d2, &d1)?);

        Ok(())
    }
}
