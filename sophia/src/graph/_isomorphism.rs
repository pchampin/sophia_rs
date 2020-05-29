//! This module implements check for isomorphic blank node equivalence of RDF
//! graphs.
//!
//! It is publicly exported to `sophia::graph`.

use crate::graph::{GTerm, GTriple, Graph};
use crate::triple::stream::{
    SinkError, SinkResult as _, SourceError, SourceResult as _, StreamError, StreamResult,
};
use crate::triple::Triple;
use sophia_api::term::matcher::AnyOrExactlyRef;
use sophia_api::term::{term_hash, TTerm, TermKind};
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
/// Both graphs may fail traversing (and this is done several times).
/// Accordingly, a `StreamError` returned where `SourceError`s originate from
/// `g1` and `SinkError`s originate from `g2`
///
/// # Performance
///
/// As this algorithm has to traverse each graph several times the algorithm
/// gets way more expensive with bigger numbers of triples. In the same way
/// the number of blank nodes contributes to the costs.
pub fn isomorphic_graphs<G1, G2>(g1: &G1, g2: &G2) -> StreamResult<bool, G1::Error, G2::Error>
where
    G1: Graph,
    G2: Graph,
    GTerm<G1>: Clone + Eq + Hash,
    GTerm<G2>: Clone + Eq + Hash,
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

    // check for same triples in both graphs
    // -------------------------------------
    // - regardless of blank nodes
    // - implicitly checks that g1 and g2 have the same length
    let g1_in_g2 = check_for_equal_triples_regardless_bns(g1, g2)?;
    let g2_in_g1 = check_for_equal_triples_regardless_bns(g2, g1).map_err(StreamError::reverse)?;

    if !(g1_in_g2 && g2_in_g1) {
        return Ok(false);
    }

    // Create hashes
    let bn_hashes1 = match calc_bn_hashes::<G1, IsoHasher>(g1) {
        Ok(map) => map,
        Err(SourceError(e)) => return Err(SourceError(e)),
        Err(SinkError(_)) => return Ok(false), // Not the best solution
    };
    let bn_hashes2 = match calc_bn_hashes::<G2, IsoHasher>(g2) {
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
    isomorphic_graphs_with_mapping(g1, g2, bn_mapping)
}

/// Builds a `TermMatcher` by using the blank node mapping provided.
///
/// If the given term is a blank node the matcher will match all possible
/// mappings for that blank node, i.e. included redundant blank nodes. If the
/// given term is not a blank node the matcher will only match the given term.
///
/// This aligns with the description of bijection _M_ described in the
/// [RDF specs](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#graph-isomorphism).
pub(crate) fn bn_mapper<'m, T1, T2>(
    mapping: &'m HashMap<T1, &Vec<T2>>,
    t: &'m T1,
) -> Vec<&'m dyn TTerm>
where
    T1: TTerm + Hash + Eq,
    T2: TTerm + Hash + Eq,
{
    if t.kind() == TermKind::BlankNode {
        match mapping.get(t) {
            None => vec![],
            Some(bns) => bns.iter().map(TTerm::as_dyn).collect(),
        }
    } else {
        vec![t.as_dyn()]
    }
}

/// Checks for each triple in `g1` with at least one blank node if it is also
/// contained in `g2` if the blank node `mapping` is applied.
fn isomorphic_graphs_with_mapping<G1, G2, E1, E2>(
    g1: &G1,
    g2: &G2,
    mapping: HashMap<GTerm<G1>, &Vec<GTerm<G2>>>,
) -> StreamResult<bool, E1, E2>
where
    E1: 'static + Error,
    E2: 'static + Error,
    G1: Graph<Error = E1>,
    G2: Graph<Error = E2>,
    GTerm<G1>: Clone + Eq + Hash,
    GTerm<G2>: Clone + Eq + Hash,
{
    for t in g1.triples() {
        let t = t.source_err()?;

        if t.s().kind() == TermKind::BlankNode
            || t.p().kind() == TermKind::BlankNode
            || t.o().kind() == TermKind::BlankNode
        {
            let ms = bn_mapper(&mapping, t.s());
            let mp = bn_mapper(&mapping, t.p());
            let mo = bn_mapper(&mapping, t.o());

            if g2.triples_matching(&ms, &mp, &mo).next().is_none() {
                return Ok(false);
            }
        }
    }

    Ok(true)
}

pub(crate) fn match_ignore_bns<T>(t: &T) -> AnyOrExactlyRef<&T>
where
    T: TTerm + ?Sized,
{
    if t.kind() == TermKind::BlankNode {
        AnyOrExactlyRef::Any
    } else {
        AnyOrExactlyRef::Exactly(t)
    }
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
    E1: 'static + Error,
    E2: 'static + Error,
    G1: Graph<Error = E1>,
    G2: Graph<Error = E2>,
{
    for t in g1.triples() {
        let t = t.source_err()?;

        let ms = match_ignore_bns(t.s());
        let mp = match_ignore_bns(t.p());
        let mo = match_ignore_bns(t.o());

        if g2.triples_matching(&ms, &mp, &mo).next().is_none() {
            return Ok(false);
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
///
/// An exception are redundant blank nodes. If the algorithm detects such nodes
/// they will share the same hash.
fn calc_bn_hashes<G, H>(
    g: &G,
) -> StreamResult<HashMap<u64, Vec<GTerm<G>>>, G::Error, AlgorithmFailure>
where
    G: Graph,
    GTerm<G>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut res_map = HashMap::new();
    let mut unresolved_map = HashMap::new();

    for bn in g.bnodes().source_err()?.into_iter() {
        let (hash, upstream, downstream) = calc_bns_init_hash::<G, H>(&bn, g).source_err()?;
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
                // Can no longer traverse graph to distinguish nodes, i.e. they must be redundant.
                let redundants = bns.into_iter().map(|(bn, _, _)| bn).collect();
                res_map.insert(hash, redundants);
            } else {
                // improve hash by further traversing.
                for (bn, upstream, downstream) in bns {
                    let (better_hash, upstream, downstream) =
                        improve_hash_by_increasing_distance::<H, G>(
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
#[allow(clippy::type_complexity)]
fn calc_bns_init_hash<G, H>(
    bn: &GTerm<G>,
    g: &G,
) -> Result<(u64, Vec<GTerm<G>>, Vec<GTerm<G>>), G::Error>
where
    G: Graph,
    GTerm<G>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    // for same hashing result we need to order the triples' hashes.
    let mut triple_hashes = BTreeSet::new();

    let mut upstream = vec![];
    let mut downstream = vec![];

    for tri in g.triples() {
        let tri = tri?;
        if tri.s() == bn || tri.p() == bn || tri.o() == bn {
            triple_hashes.insert(hash_triple_without_bn::<H, GTriple<G>>(&tri));
            if tri.o() != bn {
                upstream.push(tri.o().clone())
            };
            if tri.s() != bn {
                downstream.push(tri.s().clone())
            };
        }
    }

    // hashing
    let mut hasher = H::default();
    triple_hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

/// Improves an existing hash by further traversing the graph.
#[allow(clippy::type_complexity)]
fn improve_hash_by_increasing_distance<H, G>(
    hash: u64,
    upstream: &[GTerm<G>],
    downstream: &[GTerm<G>],
    g: &G,
) -> Result<(u64, Vec<GTerm<G>>, Vec<GTerm<G>>), G::Error>
where
    G: Graph,
    GTerm<G>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    // for same hashing result we need to order the triples' hashes.
    let mut triple_hashes = BTreeSet::new();

    let upstream = traverse_from_s_to_o::<H, G>(upstream, g, &mut triple_hashes)?;
    let downstream = traverse_from_o_to_s::<H, G>(downstream, g, &mut triple_hashes)?;

    // hashing
    let mut hasher = H::default();
    // initialize with existing hash.
    hash.hash(&mut hasher);
    triple_hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), upstream, downstream))
}

// utility
pub(crate) fn hash_if_not_bn<T, H>(t: &T, h: &mut H)
where
    T: TTerm + ?Sized,
    H: Hasher,
{
    if t.kind() != TermKind::BlankNode {
        term_hash(t, h)
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
fn traverse_from_o_to_s<H, G>(
    upstream: &[GTerm<G>],
    g: &G,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<GTerm<G>>, G::Error>
where
    G: Graph,
    GTerm<G>: Clone + Eq + Hash,
    H: Hasher + Default,
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
fn traverse_from_s_to_o<H, G>(
    downstream: &[GTerm<G>],
    g: &G,
    hashes: &mut BTreeSet<u64>,
) -> Result<Vec<GTerm<G>>, G::Error>
where
    G: Graph,
    GTerm<G>: Clone + Eq + Hash,
    H: Hasher + Default,
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
    use crate::parser::{nt, turtle};
    use crate::triple::stream::TripleSource;

    #[test]
    fn simple() -> Result<(), Box<dyn Error>> {
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

    #[test]
    fn different_parsers() -> Result<(), Box<dyn Error>> {
        let ttl = r#"
            @prefix foaf: <http://xmlns.com/foaf/0.1/>.

            [] foaf:name "Alice";
                   foaf:mbox <mailto:alice@work.example> ;
                   foaf:knows [foaf:name "Bob"] .
        "#;
        let nt = r#"
            _:alice <http://xmlns.com/foaf/0.1/name> "Alice".
            _:alice <http://xmlns.com/foaf/0.1/mbox> <mailto:alice@work.example>.
            _:alice <http://xmlns.com/foaf/0.1/knows> _:bob.
            _:bob <http://xmlns.com/foaf/0.1/name> "Bob".
        "#;
        let ttl: FastGraph = turtle::parse_str(ttl).collect_triples()?;
        let nt: FastGraph = nt::parse_str(nt).collect_triples()?;

        assert!(isomorphic_graphs(&nt, &ttl)?);
        assert!(isomorphic_graphs(&ttl, &nt)?);

        Ok(())
    }

    /// Every subject and object is a blank node with the a different predicate.
    #[test]
    fn heterogeneous_grid() -> Result<(), Box<dyn Error>> {
        let g1 = r#"
            @prefix : <http://example.org/>.

            _:a :p1 _:b, _:d .
            _:c :p2 _:b, _:f .
            _:e :p3 _:b, _:d, _:f, _:h .
            _:g :p4 _:d, _:h .
            _:i :p5 _:f, _:h .
        "#;
        let g2 = r#"
            @prefix : <http://example.org/>.

            _:a2 :p1 _:b2, _:d2 .
            _:c2 :p2 _:b2, _:f2 .
            _:e2 :p3 _:b2, _:d2, _:f2, _:h2 .
            _:g2 :p4 _:d2, _:h2 .
            _:i2 :p5 _:f2, _:h2 .
        "#;
        let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
        let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        Ok(())
    }

    /// Every subject and object is a blank node with the same predicate.
    /// Source of test: http://aidanhogan.com/docs/rdf-canonicalisation.pdf
    #[test]
    fn homogeneous_grid() -> Result<(), Box<dyn Error>> {
        let g1 = r#"
            @prefix : <http://example.org/>.

            _:a :p _:b, _:d .
            _:c :p _:b, _:f .
            _:e :p _:b, _:d, _:f, _:h .
            _:g :p _:d, _:h .
            _:i :p _:f, _:h .
        "#;
        let g2 = r#"
            @prefix : <http://example.org/>.

            _:a2 :p _:b2, _:d2 .
            _:c2 :p _:b2, _:f2 .
            _:e2 :p _:b2, _:d2, _:f2, _:h2 .
            _:g2 :p _:d2, _:h2 .
            _:i2 :p _:f2, _:h2 .
        "#;
        let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
        let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        Ok(())
    }

    /// Like homogeneous grid but with redundant nodes removed in the second graph.
    #[test]
    fn truncated_grid() -> Result<(), Box<dyn Error>> {
        let g1 = r#"
            @prefix : <http://example.org/>.

            _:a :p _:b, _:d .
            _:c :p _:b, _:f .
            _:e :p _:b, _:d, _:f, _:h .
            _:g :p _:d, _:h .
            _:i :p _:f, _:h .
        "#;
        let g2 = r#"
            @prefix : <http://example.org/>.

            _:a2 :p _:b2 .
        "#;
        let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
        let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

        assert!(!isomorphic_graphs(&g1, &g2)?);
        assert!(!isomorphic_graphs(&g2, &g1)?);

        Ok(())
    }
    /// Source of test: http://aidanhogan.com/docs/rdf-canonicalisation.pdf
    #[test]
    fn spider_like() -> Result<(), Box<dyn Error>> {
        let g1 = r#"
            @prefix : <http://example.org/>.

            :Chile :cabinet _:b1, [
                :members 23
              ], [
                :members 23
              ], _:b4 ;
              :presidency _:a1, _:a2, _:a3, _:a4 .
            
            _:a1 :next _:a2 .
            _:a2 :next _:a3 ;
              :president :MBachelet .
            _:a3 :next _:a4.
            _:a4 :president :MBachelet .

            :MBachelet :spouse _:c .
        "#;
        let g2 = r#"
            @prefix : <http://example.org/>.

            :Chile :cabinet _:b12, [
                :members 23
            ], [
                :members 23
            ], _:b42 ;
            :presidency _:a12, _:a22, _:a32, _:a42 .
            
            _:a12 :next _:a22 .
            _:a22 :next _:a32 ;
            :president :MBachelet .
            _:a32 :next _:a42.
            _:a42 :president :MBachelet .

            :MBachelet :spouse _:c2 .
        "#;
        let g1: FastGraph = turtle::parse_str(g1).collect_triples()?;
        let g2: FastGraph = turtle::parse_str(g2).collect_triples()?;

        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        Ok(())
    }
}
