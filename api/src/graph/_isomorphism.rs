//! This module implements check for isomorphic blank node equivalence of RDF
//! graphs.
//!
//! Its public members are transparently re-exported by its [parent module](../index.html).

use crate::graph::{GTerm, GTriple, Graph};
use crate::term::matcher::AnyOrExactlyRef;
use crate::term::{term_hash, TTerm, TermKind};
use crate::triple::stream::{
    SinkError, SinkResult as _, SourceError, SourceResult as _, StreamError, StreamResult,
};
use crate::triple::Triple;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::hash::{Hash, Hasher};

/// Maximal steps a graph is traversed for proofing isomorphism.
/// If this bound is exceeded the algorithm assumes that the graphs are not
/// isomorphic.
pub const MAX_DISTANCE: usize = 8;

/// The hasher used internally for checking isomorphism.
pub type IsoHasher = std::collections::hash_map::DefaultHasher;

/// Checks if both graphs are isomorphic blank node equal.
///
/// According to the [RDF specs](https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#graph-isomorphism)
/// this means that a mapping for blank nodes in `g1` exists so that `g1 == g2`.
///
/// The algorithm is inspired from a similar one in [`Oxigraph`](https://github.com/Tpt/oxigraph)
/// and is extended for the generalized RDF model of `sophia`.
///
/// # Errors
///
/// Both graphs may fail traversing (and this is done several times).
/// Accordingly, a `StreamError` returned,
/// where `SourceError`s originate from `g1`
/// and `SinkError`s originate from `g2`
///
/// # Performance
///
/// As this algorithm has to enumerates the triples of each graph several times,
/// the algorithm gets more expensive with bigger numbers of triples.
/// In the same way the number of blank nodes contributes to the cost.
///
/// Note however that the algorithm uses some heuristics,
/// to avoid the combinatorial explosion of trying every possible bnode-pairing.
/// As a result, it is not 100% accurate (see below).
///
/// # Accuracy
///
/// If `g1` and `g2` are isomorphic, the function will always return `true`.
///
/// If they are not isomorphic, the function will generally return `false`,
/// but a few pathological cases may be falses positives
/// (*i.e.* recognized as isomorphic while they are not).
///
/// For example, the graph:
///
/// ```turtle
///     _:a :rel _:b.
///     _:b :rel _:a.
///     _:c :rel _:c.
/// ```
///
/// and the graph:
///
/// ```turtle
///     _:a :rel _:b.
///     _:b :rel _:c.
///     _:c :rel _:a.
/// ```
///
/// are considered isomorphic by this algorithm,
/// because they have the same number of blank nodes and arcs,
/// and all of their blank nodes are locally indistinguisable
/// (same number of incoming and outgoinc arcs,
/// linking them to undistinguishable blank nodes).
///
/// Correctly answering in this kind of pathological case requires a combinatorial exploration
/// of all possible bnode-pairings, which would make the algorithm very slow in the worst case.
///
/// The choice has been made to accept this flaw,
/// as such undistinguishable blank nodes are very rare in real data,
/// and not particularly useful.
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
    if !check_for_equal_triples_regardless_bns(g1, g2)? {
        return Ok(false);
    }
    if !check_for_equal_triples_regardless_bns(g2, g1).map_err(StreamError::reverse)? {
        return Ok(false);
    }

    // Create hashes
    let bn_hashes1 = match calc_bn_hashes::<G1, IsoHasher>(g1, bns1) {
        Ok(map) => map,
        Err(e) => return Err(SourceError(e)),
    };
    let bn_hashes2 = match calc_bn_hashes::<G2, IsoHasher>(g2, bns2) {
        Ok(map) => map,
        Err(e) => return Err(SinkError(e)),
    };

    // Check that, for each hash, there are the same number of bnodes in each graph.
    for (hash, bns1) in bn_hashes1 {
        let bns1_len = bns1.len();
        let bns2_len = bn_hashes2.get(&hash).map(|x| x.len()).unwrap_or(0);
        if bns1_len != bns2_len {
            return Ok(false); // Not the same number of "equivalent" bnodes
        }
    }
    Ok(true) // heuristically
             // At this point, we are *almost* certain the graphs are isomorphic
             // (see section 'accuracy' in function documentation).
             // To be 100% certain,
             // we would need to try every possible 1-1 mapping of compatible bnodes
             // (i.e. bnodes with the same hash),
             // and test every arc against that mapping.
             /*
             for bn_mapping in make_all_possible_mappings(bns1, bns2) {
                 if isomorphic_graphs_with_mapping(g1, g2, bn_mapping {
                     Ok(true)
                 }
             }
             Ok(false)
             */
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

/// Checks is each triple in `g1` is also in `g2`
/// regardless of blank node labels.
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

#[allow(dead_code)]
fn dbg_map<G, T>(map: &HashMap<u64, Vec<(GTerm<G>, T)>>)
where
    G: Graph,
    GTerm<G>: Sized,
{
    for (hash, bns) in map {
        print!("=== {:8x} ", hash);
        for (bn, _) in bns {
            print!("{} ", bn.value());
        }
        println!();
    }
    println!("=== ---");
}

/// Calculate a hash for each blank node.
///
/// We first compute a hash based on all adjacent triples, ignoring bnodes.
///
/// If several blank nodes have the same hash,
/// we modify their hash with the hash of their adjacent blank nodes.
/// We repeat this step until either
/// - we reached a point where each blank node has a unique hash, or
/// - the last step didn't change the number of distinct hash.
/// At this point, if several blank nodes share the same hash,
/// they must be absolutely redundant.
fn calc_bn_hashes<G, H>(
    g: &G,
    bnodes: HashSet<GTerm<G>>,
) -> Result<HashMap<u64, Vec<GTerm<G>>>, G::Error>
where
    G: Graph,
    GTerm<G>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut n2h = HashMap::new();
    let mut map = HashMap::new();

    let n_bnodes = bnodes.len();
    for bn in bnodes {
        let (hash, related) = calc_bns_init_hash::<G, H>(&bn, g)?;
        n2h.insert(bn.clone(), hash);
        map.entry(hash).or_insert_with(Vec::new).push((bn, related));
    }
    //dbg_map::<G, _>(&map);

    let mut len_old_map = 0;

    while map.len() < n_bnodes && map.len() != len_old_map {
        len_old_map = map.len();
        let last_map = map;
        map = HashMap::new();
        let last_n2h = n2h.clone();

        for (hash, bns) in last_map {
            if bns.len() == 1 {
                map.insert(hash, bns);
            } else {
                for (bn, related) in bns {
                    let mut hasher = H::default();
                    hash.hash(&mut hasher);

                    let mut modifiers = Vec::new();
                    for (role, other) in related.iter() {
                        modifiers.push((role, last_n2h[other]));
                    }
                    modifiers.sort_unstable(); // to ensure reproducibility
                    for (role, hash) in modifiers {
                        role.hash(&mut hasher);
                        hash.hash(&mut hasher);
                    }
                    let new_hash = hasher.finish();
                    *n2h.get_mut(&bn).unwrap() = new_hash;
                    map.entry(new_hash)
                        .or_insert_with(Vec::new)
                        .push((bn, related));
                }
            }
        }
        //dbg_map::<G, _>(&map);
    }
    let mut ret = HashMap::with_capacity(map.len());
    for (hash, bns) in map {
        let v = bns.into_iter().map(|(bn, _)| bn).collect();
        ret.insert(hash, v);
    }
    Ok(ret)
}

/// Calculate the blank node's initial hash in the graph, i.e. for distance 0.
///
/// Returns the initial hash, and a vec of related blank node
/// (associated with an opaque role identifier )
#[allow(clippy::type_complexity)]
fn calc_bns_init_hash<G, H>(bn: &GTerm<G>, g: &G) -> Result<(u64, Vec<(u8, GTerm<G>)>), G::Error>
where
    G: Graph,
    GTerm<G>: Clone + Eq + Hash,
    H: Hasher + Default,
{
    let mut triple_hashes = Vec::new();
    let mut related = vec![];

    for tri in g.triples_with_s(bn) {
        let tri = tri?;
        triple_hashes.push(hash_triple_without_bn::<H, GTriple<G>>(&tri));
        let p = tri.p();
        if p.kind() == TermKind::BlankNode && p != bn {
            related.push((0, p.clone()));
        }
        let o = tri.o();
        if o.kind() == TermKind::BlankNode && o != bn {
            related.push((1, o.clone()));
        }
    }
    for tri in g.triples_with_p(bn) {
        let tri = tri?;
        triple_hashes.push(hash_triple_without_bn::<H, GTriple<G>>(&tri));
        let s = tri.s();
        if s.kind() == TermKind::BlankNode && s != bn {
            related.push((2, s.clone()));
        }
        let o = tri.o();
        if o.kind() == TermKind::BlankNode && o != bn {
            related.push((3, o.clone()));
        }
    }
    for tri in g.triples_with_o(bn) {
        let tri = tri?;
        triple_hashes.push(hash_triple_without_bn::<H, GTriple<G>>(&tri));
        let s = tri.s();
        if s.kind() == TermKind::BlankNode && s != bn {
            related.push((4, s.clone()));
        }
        let p = tri.p();
        if p.kind() == TermKind::BlankNode && p != bn {
            related.push((5, p.clone()));
        }
    }

    triple_hashes.sort_unstable(); // to ensure reproducibility

    // hashing
    let mut hasher = H::default();
    triple_hashes.into_iter().for_each(|h| h.hash(&mut hasher));

    Ok((hasher.finish(), related))
}

// utility
pub(crate) fn hash_if_not_bn<T, H>(t: &T, role: u8, h: &mut H)
where
    T: TTerm + ?Sized,
    H: Hasher,
{
    if t.kind() != TermKind::BlankNode {
        term_hash(t, h)
    } else {
        role.hash(h)
    }
}

fn hash_triple_without_bn<H, T>(t: &T) -> u64
where
    H: Hasher + Default,
    T: Triple,
{
    let mut h = H::default();
    hash_if_not_bn(t.s(), 0, &mut h);
    hash_if_not_bn(t.p(), 1, &mut h);
    hash_if_not_bn(t.o(), 2, &mut h);
    h.finish()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ns::xsd;
    use crate::term::test::TestTerm;

    type StaticTerm = TestTerm<&'static str>;

    #[test]
    fn simple() -> Result<(), Box<dyn Error>> {
        let foaf = "http://xmlns.com/foaf/0.1/";
        let foaf_knows = StaticTerm::iri2(foaf, "knows");
        let foaf_mbox = StaticTerm::iri2(foaf, "mbox");
        let foaf_name = StaticTerm::iri2(foaf, "name");
        let mbox_alice = StaticTerm::iri("mailto:alice@work.example");
        let lit_alice = StaticTerm::lit_dt("alice", xsd::string);
        let lit_bob = StaticTerm::lit_dt("bob", xsd::string);

        let make_graph = |b1: &'static str, b2: &'static str| -> Vec<[StaticTerm; 3]> {
            let b1 = StaticTerm::bnode(b1);
            let b2 = StaticTerm::bnode(b2);
            vec![
                [b1, foaf_name, lit_alice],
                [b1, foaf_mbox, mbox_alice],
                [b1, foaf_knows, b2],
                [b2, foaf_name, lit_bob],
            ]
        };
        let g1 = make_graph("alice", "bob");
        assert!(isomorphic_graphs(&g1, &g1)?);

        let g2 = make_graph("a", "b");
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        let g3 = make_graph("b", "a");
        assert!(isomorphic_graphs(&g2, &g3)?);
        assert!(isomorphic_graphs(&g1, &g3)?);

        let b1 = StaticTerm::bnode("alice");
        let g4 = vec![
            [b1, foaf_name, lit_alice],
            [b1, foaf_mbox, mbox_alice],
            [b1, foaf_knows, StaticTerm::bnode("bob")],
            [StaticTerm::bnode("bobby"), foaf_name, lit_bob],
        ];
        assert!(!isomorphic_graphs(&g1, &g4)?);
        assert!(!isomorphic_graphs(&g4, &g1)?);

        Ok(())
    }

    fn make_chain(ids: &'static str) -> Vec<[StaticTerm; 3]> {
        let rel = StaticTerm::iri("tag:rel");
        let nodes: Vec<_> = (0..ids.len())
            .map(|i| StaticTerm::bnode(&ids[i..i + 1]))
            .collect();
        let mut graph = Vec::with_capacity(ids.len() - 1);
        for i in 1..nodes.len() {
            graph.push([nodes[i - 1], rel, nodes[i]]);
        }
        graph
    }

    #[test]
    fn chain() -> Result<(), Box<dyn Error>> {
        let g1 = make_chain("abcdefghij");
        assert!(isomorphic_graphs(&g1, &g1)?);
        let g2 = make_chain("jihgfedcba");
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        let g3 = make_chain("abcdefghijk");
        assert!(!isomorphic_graphs(&g1, &g3)?);
        Ok(())
    }

    #[test]
    fn cycle2() -> Result<(), Box<dyn Error>> {
        let g1 = make_chain("aba");
        assert!(isomorphic_graphs(&g1, &g1)?);
        let g2 = make_chain("ABA");
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);
        Ok(())
    }

    #[test]
    fn cycle_long() -> Result<(), Box<dyn Error>> {
        let g1 = make_chain("abcdefghia");
        assert!(isomorphic_graphs(&g1, &g1)?);
        let g2 = make_chain("jihgfedcbj");
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        let g3 = make_chain("abcdefghija");
        assert!(!isomorphic_graphs(&g1, &g3)?);
        Ok(())
    }

    #[test]
    #[ignore]
    fn cycle_pathological() -> Result<(), Box<dyn Error>> {
        // This case is tricky (and does not work with the current implementation).
        // Both graphs contain the same number of (blank nodes) and the same number of arcs.
        // All blank nodes are locally undistinguishable from each other:
        // - they have exactly 1 incoming arc and 1 outgoing arc,
        // - both linking them to a blank node that are themselves undistinguisgable.
        let mut g1 = make_chain("abca");
        let mut g1b = make_chain("defgd");
        g1.append(&mut g1b);

        let g2 = make_chain("abcdefga");
        assert!(!isomorphic_graphs(&g1, &g2)?);
        Ok(())
    }

    #[test]
    fn cycle_almost_pathological() -> Result<(), Box<dyn Error>> {
        // This is uses the same graphs as above (cycle_pathological),
        // but *one* of the blank nodes is distinguished by an additional property,
        // which breaks symmetry and allow the algorithm to give the correct answer.
        //
        // This illustrate why the pathological case is not too bad:
        // in real data, *most* be nodes will be distinguisgable like that.
        let typ = StaticTerm::iri("tag:type");
        let dist = StaticTerm::iri("tag:Distinguished");

        let mut g1 = make_chain("abca");
        let mut g1b = make_chain("defgd");
        g1.append(&mut g1b);
        g1.push([g1[0][0], typ, dist]);

        let mut g2 = make_chain("abcdefga");
        g2.push([g2[0][0], typ, dist]);
        assert!(!isomorphic_graphs(&g1, &g2)?);
        Ok(())
    }

    fn make_clique(ids: &'static str) -> Vec<[StaticTerm; 3]> {
        let rel = StaticTerm::iri("tag:rel");
        let nodes: Vec<_> = (0..ids.len())
            .map(|i| StaticTerm::bnode(&ids[i..i + 1]))
            .collect();
        let mut graph = Vec::with_capacity(ids.len() * ids.len());
        for n1 in nodes.iter() {
            for n2 in nodes.iter() {
                graph.push([*n1, rel, *n2]);
            }
        }
        graph
    }

    #[test]
    fn clique() -> Result<(), Box<dyn Error>> {
        let g1 = make_clique("abcde");
        assert!(isomorphic_graphs(&g1, &g1)?);

        let g2 = make_clique("ABCDE");
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        let g3 = make_clique("abcd");
        assert!(!isomorphic_graphs(&g1, &g3)?);
        Ok(())
    }

    fn make_tree(ids: &'static str) -> Vec<[StaticTerm; 3]> {
        let rel = StaticTerm::iri("tag:rel");
        let nodes: Vec<_> = (0..ids.len())
            .map(|i| StaticTerm::bnode(&ids[i..i + 1]))
            .collect();
        let mut graph = Vec::with_capacity(ids.len() * ids.len());
        let mut i = 0;
        while 2 * i < nodes.len() {
            graph.push([nodes[i], rel, nodes[2 * i]]);
            if 2 * i + 1 < nodes.len() {
                graph.push([nodes[i], rel, nodes[2 * i + 1]]);
            }
            i += 1;
        }
        graph
    }

    #[test]
    fn tree() -> Result<(), Box<dyn Error>> {
        let g1 = make_tree("abcdefghij");
        assert!(isomorphic_graphs(&g1, &g1)?);

        let g2 = make_tree("ABCDEFGHIJ");
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        let g3 = make_tree("abcdefghijk");
        assert!(!isomorphic_graphs(&g1, &g3)?);
        Ok(())
    }

    #[test]
    fn predicate() -> Result<(), Box<dyn Error>> {
        let rel = StaticTerm::iri("tag:rel");
        let b1 = StaticTerm::bnode("b1");
        let b2 = StaticTerm::bnode("b2");
        let b3 = StaticTerm::bnode("b3");
        let b4 = StaticTerm::bnode("b4");

        let g1 = vec![[b1, rel, b2], [b2, rel, b3], [rel, b1, b4]];
        assert!(isomorphic_graphs(&g1, &g1)?);

        let g2 = vec![[b2, rel, b3], [b3, rel, b4], [rel, b2, b1]];
        assert!(isomorphic_graphs(&g1, &g2)?);
        assert!(isomorphic_graphs(&g2, &g1)?);

        let g3 = vec![[b1, rel, b2], [b2, rel, b3], [rel, b2, b4]];
        assert!(!isomorphic_graphs(&g2, &g3)?);
        assert!(!isomorphic_graphs(&g1, &g3)?);

        Ok(())
    }
}
