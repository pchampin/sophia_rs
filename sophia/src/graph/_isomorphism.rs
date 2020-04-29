//! This module implements check for isomorphic blank node equivalence of RDF
//! graphs.
//!
//! It is publicly exported to `sophia::graph`.

use crate::graph::{GTerm, GTriple, Graph};
use crate::triple::stream::{
    SinkError, SinkResult as _, SourceError, SourceResult as _, StreamResult,
};
use crate::triple::Triple;
use sophia_term::{RefTerm, Term, TermData};
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fmt;
use std::hash::{Hash, Hasher};

/// Maximal steps a graph is traversed for proofing isomorphism.
/// If this bound is exceeded the algorithm assumes that the graphs are not
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

/// Checks if both graphs are isomorphic blank node equal.
///
/// According to the [RDF specs](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#graph-isomorphism)
/// this means that a mapping for blank nodes in `g1` exists so that `g1 == g2`.
///
/// The used algorithm was originally implemented for [`Oxigraph`](https://github.com/Tpt/oxigraph)
/// and is extended for the generalized RDF model of `sophia`.
///
/// # Errors
///
/// Both graphs can fail traversing (and this is done several times),
/// accordingly, a `StreamError` returned where `SourceError`s originate from
/// `g1` and `SinkError`s originate from `g2`
///
/// _TODO:_ In this case the notion of `source` and `sink` is not fitting.
/// Still this requires two different error types. Maybe we should turn
/// `StreamError` into `EitherError`?
///
/// # Performance
///
/// Gets quickly very expensive.
pub fn isomorphic_graphs<G1, G2, E1, E2>(g1: &G1, g2: &G2) -> StreamResult<bool, E1, E2>
where
    E1: 'static + std::error::Error,
    E2: 'static + std::error::Error,
    G1: Graph<Error = E1>,
    G2: Graph<Error = E2>,
{
    // quick return conditions
    // -----------------------
    let (min1, max1) = g1.triples().size_hint();
    let (min2, max2) = g2.triples().size_hint();
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

    // blank nodes in the respective graph
    let bns1 = g1.bnodes().source_err()?;
    let bns2 = g2.bnodes().sink_err()?;

    if bns1.len() != bns2.len() {
        return Ok(false);
    }

    // check if each triple in g1 is also in g2
    // ----------------------------------------
    // regardless of blank nodes
    if !check_for_equal_triples_regardless_bns(g1, g2)? {
        return Ok(false);
    }

    // Create hashes
    let bn_hashes1 = match calc_bn_hashes::<G1, E1, IsoHasher>(g1) {
        Ok(map) => map,
        Err(SourceError(e)) => return Err(SourceError(e)),
        Err(SinkError(_)) => return Ok(false), // Not the best solution
    };
    let bn_hashes2 = match calc_bn_hashes::<G2, E2, IsoHasher>(g2) {
        Ok(map) => map,
        Err(SourceError(e)) => return Err(SinkError(e)),
        Err(SinkError(_)) => return Ok(false), // Not the best solution
    };

    let mut bn_mapping = HashMap::new();
    for (hash, bn1) in bn_hashes1 {
        let bn2 = match bn_hashes2.get(&hash) {
            Some(bn) => bn,
            None => return Ok(false), // No matching blank node in g2!
        };
        bn_mapping.insert(bn1, bn2);
    }

    isomorphic_graphs_with_mapping(g1, g2, bn_mapping)
}

/// Checks for each triple in `g1` with at least one blank node if it is also
/// contained in `g2` if the blank node `mapping` is applied.
fn isomorphic_graphs_with_mapping<G1, G2, E1, E2>(
    g1: &G1,
    g2: &G2,
    mapping: HashMap<GTerm<G1>, &GTerm<G2>>,
) -> StreamResult<bool, E1, E2>
where
    E1: 'static + std::error::Error,
    E2: 'static + std::error::Error,
    G1: Graph<Error = E1>,
    G2: Graph<Error = E2>,
{
    // each `get()` on map is unwrapped as it is assumed that the mapping is
    // complete.
    for t in g1.triples() {
        let t = t.source_err()?;
        match (t.s(), t.p(), t.o()) {
            (Term::BNode(_), Term::BNode(_), Term::BNode(_)) => {
                if g2
                    .triples_with_spo(
                        mapping.get(t.s()).unwrap(),
                        mapping.get(t.p()).unwrap(),
                        mapping.get(t.o()).unwrap(),
                    )
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            (Term::BNode(_), Term::BNode(_), o) => {
                if g2
                    .triples_with_spo(mapping.get(t.s()).unwrap(), mapping.get(t.p()).unwrap(), o)
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            (s, Term::BNode(_), Term::BNode(_)) => {
                if g2
                    .triples_with_spo(s, mapping.get(t.p()).unwrap(), mapping.get(t.o()).unwrap())
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            (Term::BNode(_), p, Term::BNode(_)) => {
                if g2
                    .triples_with_spo(mapping.get(t.s()).unwrap(), p, mapping.get(t.o()).unwrap())
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            (Term::BNode(_), p, o) => {
                if g2
                    .triples_with_spo(mapping.get(t.s()).unwrap(), p, o)
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            (s, Term::BNode(_), o) => {
                if g2
                    .triples_with_spo(s, mapping.get(t.p()).unwrap(), o)
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            (s, p, Term::BNode(_)) => {
                if g2
                    .triples_with_spo(s, p, mapping.get(t.o()).unwrap())
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            _ => continue,
        }
    }

    Ok(true)
}

/// Checks is each triple in `g1` is also in `g2` regardless of blank node
/// labels.
///
/// # Example
///
/// Assume there are two graphs `g1`:
///
/// ```text
/// _:s1 :p :o .
/// _:s1 :p 42 .
/// ```
///
/// and `g2`:
///
/// ```text
/// _:s2 :p :o .
/// _:s2 :p 21 .
/// ```
///
/// The first triple passes the check as it searches for a triples with
/// predicate `:p` and object `:o` in `g2`. The second triple fails.
///
/// However, this is still not enough to proof isomorphism.
fn check_for_equal_triples_regardless_bns<G1, G2, E1, E2>(
    g1: &G1,
    g2: &G2,
) -> StreamResult<bool, E1, E2>
where
    E1: 'static + std::error::Error,
    E2: 'static + std::error::Error,
    G1: Graph<Error = E1>,
    G2: Graph<Error = E2>,
{
    for t in g1.triples() {
        let t = t.source_err()?;
        match (t.s(), t.p(), t.o()) {
            (Term::BNode(_), Term::BNode(_), Term::BNode(_)) => {
                let check_bn = |t: &RefTerm| matches!(t, Term::BNode(_));
                if g2
                    .triples_matching(&check_bn, &check_bn, &check_bn)
                    .next()
                    .is_none()
                {
                    return Ok(false);
                }
            }
            (Term::BNode(_), Term::BNode(_), o) => {
                if g2.triples_with_o(o).next().is_none() {
                    return Ok(false);
                }
            }
            (s, Term::BNode(_), Term::BNode(_)) => {
                if g2.triples_with_s(s).next().is_none() {
                    return Ok(false);
                }
            }
            (Term::BNode(_), p, Term::BNode(_)) => {
                if g2.triples_with_p(p).next().is_none() {
                    return Ok(false);
                }
            }
            (Term::BNode(_), p, o) => {
                if g2.triples_with_po(p, o).next().is_none() {
                    return Ok(false);
                }
            }
            (s, Term::BNode(_), o) => {
                if g2.triples_with_so(s, o).next().is_none() {
                    return Ok(false);
                }
            }
            (s, p, Term::BNode(_)) => {
                if g2.triples_with_sp(s, p).next().is_none() {
                    return Ok(false);
                }
            }
            (s, p, o) => {
                if !g2.contains(s, p, o).sink_err()? {
                    return Ok(false);
                }
            }
        }
    }

    Ok(true)
}

/// Calculate a hash for each blank node.
///
/// The hash of a blank node in a graph with `distance == 0` is the hash of all
/// terms in the triples in which the blank node occurs. Should this not be
/// enough to create distinct hashes, one can increase the distance. Increasing
/// distance means that beginning from the blank nodes triples the graph is
/// traversed up and down to add further triples to the hash calculation.
///
/// Blank nodes are not included in calculating the hashes.
///
/// The hashes are distinct if every 'bucket', i.e. the `Vec` in the returning
/// `HashMap` has only one element.
fn calc_bn_hashes<'g, G, E, H>(
    g: &'g G,
) -> StreamResult<HashMap<u64, GTerm<G>>, E, AlgorithmFailure>
where
    E: 'static + std::error::Error,
    G: Graph<Error = E>,
    H: Hasher + Default,
{
    let mut res_map = HashMap::new();
    let mut unresolved_map = HashMap::new();

    for bn in g.bnodes().source_err()?.into_iter() {
        let (hash, upstream, downstream) = calc_bns_init_hash::<G, E, H>(&bn, g).source_err()?;
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
                let (bn, _, _) = bns.into_iter().next().expect("len == 1");
                res_map.insert(hash, bn);
            } else {
                for (bn, upstream, downstream) in bns {
                    let (better_hash, upstream, downstream) =
                        improve_hash_by_increasing_distance::<H, G, E>(
                            hash,
                            &upstream,
                            &downstream,
                            g,
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

/// Calculate the blank node's initial hash in the graph, i.e. for distance 0.
///
/// Returns the initial hash, the upstream nodes and the downstream nodes.
fn calc_bns_init_hash<'g, G, E, H>(
    bn: &'g GTerm<G>,
    g: &G,
) -> Result<(u64, Vec<GTerm<G>>, Vec<GTerm<G>>), E>
where
    E: 'static + std::error::Error,
    G: Graph<Error = E>,
    H: Hasher + Default,
{
    // for same hashing result we need to order the triples' hashes.
    let mut triple_hashes = BTreeSet::new();

    let mut upstream = traverse_upstream::<H, G, E>(&[bn.clone()], g, &mut triple_hashes)?;
    let mut downstream = traverse_downstream::<H, G, E>(&[bn.clone()], g, &mut triple_hashes)?;

    // required for the generalized model of sophia.
    for tri in g.triples_with_p(bn) {
        let tri = tri?;
        triple_hashes.insert(hash_triple_without_bn::<H, GTriple<G>>(&tri));
        upstream.push(tri.s().clone());
        downstream.push(tri.o().clone());
    }

    // hashing
    let mut hasher = H::default();
    triple_hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

/// Improves an existing hash by further traversing the graph.
fn improve_hash_by_increasing_distance<H, G, E>(
    hash: u64,
    upstream: &[GTerm<G>],
    downstream: &[GTerm<G>],
    g: &G,
) -> Result<(u64, Vec<GTerm<G>>, Vec<GTerm<G>>), E>
where
    H: Hasher + Default,
    G: Graph<Error = E>,
    E: 'static + std::error::Error,
{
    // for same hashing result we need to order the triples' hashes.
    let mut triple_hashes = BTreeSet::new();

    let upstream = traverse_upstream::<H, G, E>(upstream, g, &mut triple_hashes)?;
    let downstream = traverse_downstream::<H, G, E>(downstream, g, &mut triple_hashes)?;

    // hashing
    let mut hasher = H::default();
    // initialize with existing hash.
    hash.hash(&mut hasher);
    triple_hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

// utility
fn hash_if_not_bn<TD, H>(t: &Term<TD>, h: &mut H)
where
    TD: TermData,
    H: Hasher,
{
    if !matches!(t, Term::BNode(_)) {
        t.hash(h)
    }
}

fn hash_triple_without_bn<H, T>(t: &T) -> u64
where
    H: Hasher + Default,
    T: Triple,
{
    let mut h = H::default();
    hash_if_not_bn(t.s(), &mut h);
    hash_if_not_bn(t.p(), &mut h);
    hash_if_not_bn(t.o(), &mut h);
    h.finish()
}

/// Looks for triples where the given terms are objects.
/// Those triples' hashes are inserted into the list and a list of their
/// subjects is returned.
fn traverse_upstream<H, G, E>(
    upstream: &[GTerm<G>],
    g: &G,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<GTerm<G>>, E>
where
    H: Hasher + Default,
    E: 'static + std::error::Error,
    G: Graph<Error = E>,
{
    let mut subjects = vec![];
    for o in upstream {
        for tri in g.triples_with_o(o) {
            let tri = tri?;
            hashes.insert(hash_triple_without_bn::<H, GTriple<G>>(&tri));
            subjects.push(tri.s().clone());
        }
    }
    Ok(subjects)
}

/// Looks for triples where the given terms are subjects.
/// Those triples' hashes are inserted into the list and a list of their
/// objects is returned.
fn traverse_downstream<H, G, E>(
    downstream: &[GTerm<G>],
    g: &G,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<GTerm<G>>, E>
where
    H: Hasher + Default,
    E: 'static + std::error::Error,
    G: Graph<Error = E>,
{
    let mut objects = vec![];
    for s in downstream {
        for tri in g.triples_with_s(s) {
            let tri = tri?;
            hashes.insert(hash_triple_without_bn::<H, GTriple<G>>(&tri));
            objects.push(tri.o().clone());
        }
    }
    Ok(objects)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::graph::inmem::FastGraph;
    use crate::parser::turtle;
    use crate::triple::stream::TripleSource;

    #[test]
    fn simple_iso() -> Result<(), Box<dyn Error>> {
        let g1 = r#"
            @prefix foaf: <http://xmlns.com/foaf/0.1/>.

            _:alice foaf:name "Alice";
                   foaf:mbox <mailto:alice@work.example> ;
                   foaf:knows _:bob .
            _:bob foaf:name "Bob".
        "#;
        let g2 = r#"
            @prefix foaf: <http://xmlns.com/foaf/0.1/>.

            _:a foaf:name "Alice";
                   foaf:mbox <mailto:alice@work.example> ;
                   foaf:knows _:b .
            _:b foaf:name "Bob".
        "#;
        let g3 = r#"
            @prefix foaf: <http://xmlns.com/foaf/0.1/>.

            _:a foaf:name "Alice";
                   foaf:mbox <mailto:alice@work.example> ;
                   foaf:knows _:b .
            _:c foaf:name "Bob".
        "#;
        let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
        let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;
        let g3: FastGraph = turtle::parse_str(g3).collect_triples()?;
        
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);
        assert!(!isomorphic_graphs(&g1, &g3)?);
        assert!(!isomorphic_graphs(&g2, &g3)?);

        Ok(())
    }

    // TODO: Add test with bigger and more complex graphs
}