//! I provide the implementation of the RDFC-1.0 algorithm described at
//! <https://www.w3.org/TR/rdf-canon/>

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt::Write;
use std::io;
use std::rc::Rc;

use sophia_api::dataset::{DTerm, SetDataset};
use sophia_api::quad::{Quad, Spog, iter_spog};
use sophia_api::term::{BnodeId, Term};

use crate::_c14n_term::{C14nTerm, cmp_c14n_terms};
use crate::_cnq::nq;
use crate::_permutations::for_each_permutation_of;
use crate::C14nError;
use crate::hash::{HashFunction, Sha256, Sha384};

/// Write into `w` a canonical N-quads representation of `d`, where
/// + blank nodes are canonically [relabelled](relabel) with
///   - the [SHA-256](Sha256) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`];
/// + quads are sorted in codepoint order.
///
/// See also [`normalize_with`].
pub fn normalize<D: SetDataset, W: io::Write>(d: &D, w: W) -> Result<(), C14nError<D::Error>> {
    normalize_with::<Sha256, D, W, true>(d, w, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Write into `w` a canonical N-quads representation of `d`, where
/// + blank nodes are canonically [relabelled](relabel_sha384) with
///   - the [SHA-384](Sha384) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`];
/// + quads are sorted in codepoint order.
///
/// See also [`normalize_with`].
pub fn normalize_sha384<D: SetDataset, W: io::Write>(
    d: &D,
    w: W,
) -> Result<(), C14nError<D::Error>> {
    normalize_with::<Sha384, D, W, true>(d, w, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Write into `w` a canonical N-quads representation of `d`, where
/// + blank nodes are canonically [relabelled](relabel_with) with
///   - the [hash function](HashFunction) `H`,
///   - the given `depth_factor`,
///   - the given `permutation_limit`;
/// + quads are sorted in codepoint order;
/// + if the generic parameter `S` (strict) is false,
///   a (non-standard) generalized version of RDFC1.0 is used,
///   that supports RDF 1.2 and Sophia's generalized RDF.
///
/// See also [`normalize`].
pub fn normalize_with<H: HashFunction, D: SetDataset, W: io::Write, const S: bool>(
    d: &D,
    mut w: W,
    depth_factor: f32,
    permutation_limit: usize,
) -> Result<(), C14nError<D::Error>> {
    let (mut quads, _) = relabel_with::<H, D, S>(d, depth_factor, permutation_limit)?;
    let mut buf1 = String::new();
    let mut buf2 = String::new();
    // we sort the quads, but comparing the terms based on their NQ serialization,
    // which amounts to sorting the N-Quads lines without materializing them
    quads.sort_unstable_by(|q1, q2| {
        for (t1, t2) in iter_spog_opt(q1.spog()).zip(iter_spog_opt(q2.spog())) {
            buf1.clear();
            buf2.clear();
            let o = cmp_c14n_terms(t1, t2, &mut buf1, &mut buf2);
            if o != Ordering::Equal {
                return o;
            }
        }
        Ordering::Equal
    });
    for quad in quads {
        buf1.clear();
        nq(quad.s(), &mut buf1);
        nq(quad.p(), &mut buf1);
        nq(quad.o(), &mut buf1);
        if let Some(gn) = quad.g() {
            nq(gn, &mut buf1);
        }
        w.write_all(buf1.as_bytes()).map_err(C14nError::Io)?;
        w.write_all(b".\n").map_err(C14nError::Io)?;
    }
    Ok(())
}

/// Return a [`Dataset`](sophia_api::dataset::Dataset) isomorphic to `d`,
/// paired with a mapping from original blank node labels to canonical ones.
///
/// This calls [`relabel_with`] with
///   - the [SHA-256](Sha256) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`].
///
/// Implements <https://www.w3.org/TR/rdf-canon/#canon-algorithm>
///
/// See also [`normalize`].
pub fn relabel<D: SetDataset>(d: &D) -> Result<(C14nQuads<'_, D>, C14nIdMap), C14nError<D::Error>> {
    relabel_with::<Sha256, D, true>(d, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Return a [`Dataset`](sophia_api::dataset::Dataset) isomorphic to `d`,
/// paired with a mapping from original blank node labels to canonical ones.
///
/// This calls [`relabel_with`] with
///   - the [SHA-384](Sha384) hash function,
///   - the [`DEFAULT_DEPTH_FACTOR`],
///   - the [`DEFAULT_PERMUTATION_LIMIT`].
///
/// Implements <https://www.w3.org/TR/rdf-canon/#canon-algorithm>
///
/// See also [`normalize`].
pub fn relabel_sha384<D: SetDataset>(
    d: &D,
) -> Result<(C14nQuads<'_, D>, C14nIdMap), C14nError<D::Error>> {
    relabel_with::<Sha384, D, true>(d, DEFAULT_DEPTH_FACTOR, DEFAULT_PERMUTATION_LIMIT)
}

/// Return a [`Dataset`](sophia_api::dataset::Dataset) isomorphic to `d`,
/// paired with a mapping from original blank node labels to canonical ones.
///
/// The generic parameter `H` determines which [hash function](HashFunction)
/// the algorithm should use internally
/// (RDFC-1.0 uses [SHA-256](Sha256) by default).
///
/// The parameters `depth_factor` and `permutation_limit`
/// are used to stop the algorithm if the computation becomes too complex,
/// in order to secure it against [dataset poisoning](https://www.w3.org/TR/rdf-canon/#dataset-poisoning).
/// The default values ([`DEFAULT_DEPTH_FACTOR`]) and [`DEFAULT_PERMUTATION_LIMIT`])
/// are expected to work with any "realistic" dataset.
///
/// More preciselity:
/// * the algorithm will not recurse more deeply than`depth_factor`*N,
///   where N is the total number of blank nodes in the dataset;
/// * the algorithl will not try to disambiguate more than
///   `permutation_limit` undistinguishable blank nodes
///   (blank nodes with the same immediate neighbourhood).
///
/// Implements <https://www.w3.org/TR/rdf-canon/#canon-algorithm>;
/// if the generic parameter `S` (strict) is false,
/// that algorithm is extended to supports RDF 1.2 and Sophia's generalized RDF.
///
/// See also [`relabel`], [`normalize_with`].
pub fn relabel_with<'a, H: HashFunction, D: SetDataset, const S: bool>(
    d: &'a D,
    depth_factor: f32,
    permutation_limit: usize,
) -> Result<(C14nQuads<'a, D>, C14nIdMap), C14nError<D::Error>> {
    let quads: Result<Vec<Spog<DTerm<'a, D>>>, _> =
        d.quads().map(|res| res.map(Quad::to_spog)).collect();
    let quads = quads?;
    // Step 1
    let mut state = C14nState::<H, _, S>::new(depth_factor, permutation_limit);
    // Step 2
    for quad in &quads {
        if S && quad.p().is_blank_node() {
            return Err(C14nError::Unsupported(
                "RDFC-1.0 does not support blank node as predicate".to_string(),
            ));
        }
        for component in iter_spog(quad.spog()) {
            if component.is_triple() || component.is_variable() {
                return Err(C14nError::Unsupported(
                    "RDFC-1.0 does not support variables nor quoted triples".to_string(),
                ));
            }
            if let Some(bnid) = component.bnode_id() {
                state
                    .b2q
                    .entry(Rc::from(bnid.as_str()))
                    .or_default()
                    .push(quad);
            }
        }
    }
    // Step 3
    for (bnid, quads) in &state.b2q {
        let hash = hash_first_degree_quads::<H, _>(bnid, &quads[..]);
        state.h2b.entry(hash).or_default().push(bnid.clone());
        state.b2h.insert(bnid.clone(), hash);
    }
    // Step 4
    // NB: we are relying on the fact that BTreeMap's elements are sorted
    let mut next_h2b = BTreeMap::new();
    // TODO once BTreeMap::extract_if is stabilize,
    // use it in the loop below instead of reinserting elements into a new map
    for (hash, bnids) in state.h2b {
        debug_assert!(!bnids.is_empty());
        if bnids.len() > 1 {
            next_h2b.insert(hash, bnids);
        } else {
            state.canonical.issue(&bnids[0]);
        }
    }
    state.h2b = next_h2b;
    // Step 5
    for identifier_list in state.h2b.values() {
        let mut hash_path_list = vec![];
        // Step 5.2
        for i in identifier_list {
            let mut issuer = BnodeIssuer::new(BnodeId::new_unchecked("b"));
            issuer.issue(i);
            hash_path_list.push(state.hash_n_degree_quads(i, &issuer, 0)?);
        }
        // Step 5.3
        hash_path_list.sort_unstable_by_key(|p| p.0);
        for (_, issuer) in hash_path_list {
            for bnid in issuer.issued_order {
                state.canonical.issue(&bnid);
            }
        }
    }
    // Step 6
    let issued = state.canonical.issued;
    let quads = quads
        .into_iter()
        .map(|q| {
            let (spo, g) = q;
            let convert = |t: DTerm<'a, D>| {
                if let Some(bnid) = t.bnode_id() {
                    let canon_id = issued.get(bnid.as_str()).unwrap();
                    return C14nTerm::Blank(canon_id.clone());
                }
                C14nTerm::Other(t)
            };
            let spo = spo.map(convert);
            let g = g.map(convert);
            (spo, g)
        })
        .collect();
    Ok((quads, issued))
}

/// The default value of `depth_factor` in [`normalize`] and [`relabel`].
pub const DEFAULT_DEPTH_FACTOR: f32 = 1.0;

/// The default value of `permutation_limit` in [`normalize`] and [`relabel`].
pub const DEFAULT_PERMUTATION_LIMIT: usize = 6;

/// An impl of [`Dataset`](sophia_api::dataset::Dataset)
/// that contains canonical labels for blank nodes.
pub type C14nQuads<'a, D> = Vec<Spog<C14nTerm<DTerm<'a, D>>>>;

/// An identifier map as returned by [`relabel`] and [`relabel_with`]
pub type C14nIdMap = BTreeMap<Rc<str>, BnodeId<Rc<str>>>;

#[derive(Clone, Debug)]
struct C14nState<'a, H: HashFunction, T: Term, const S: bool> {
    b2q: BTreeMap<Rc<str>, Vec<&'a Spog<T>>>,
    h2b: BTreeMap<H::Output, Vec<Rc<str>>>,
    canonical: BnodeIssuer,
    /// Not specified in the spec: memozing the results of hash 1st degree
    b2h: BTreeMap<Rc<str>, H::Output>,
    /// Not specified in the spec: maximum recursion factor in `hash_n_degree_quads`
    depth_factor: f32,
    /// Not specified in the spec: maximum number of nodes on which permutations will be computed
    permutation_limit: usize,
}

impl<H: HashFunction, T: Term, const S: bool> C14nState<'_, H, T, S> {
    fn new(depth_factor: f32, permutation_limit: usize) -> Self {
        C14nState {
            b2q: BTreeMap::new(),
            h2b: BTreeMap::new(),
            canonical: BnodeIssuer::new(BnodeId::new_unchecked("c14n")),
            b2h: BTreeMap::new(),
            depth_factor,
            permutation_limit,
        }
    }

    /// Implements <https://www.w3.org/TR/rdf-canon/#hash-related-blank-node>
    fn hash_related_bnode(
        &self,
        related: &str,
        quad: &Spog<T>,
        issuer: &BnodeIssuer,
        position: &str,
    ) -> H::Output {
        let mut input = H::initialize();
        input.update(position.as_bytes());
        if position != "g" {
            input.update(b"<");
            input.update(quad.p().iri().unwrap().as_bytes());
            input.update(b">");
        }
        self.hash_related_bnode_steps_3_4(related, issuer, &mut input);
        input.finalize()
    }

    fn hash_related_bnode_steps_3_4(
        &self,
        related: &str,
        issuer: &BnodeIssuer,
        input: &mut H,
    ) {
        if let Some(canon_id) = self.canonical.issued.get(related) {
            input.update(b"_:");
            input.update(canon_id.as_bytes());
        } else if let Some(temp_id) = issuer.issued.get(related) {
            input.update("_:");
            input.update(temp_id.as_bytes());
        } else {
            // retrieved memoized value of hash_first_degree_quads for this blank node
            let h1d = self.b2h.get(related).unwrap();
            input.update(hex(h1d).as_bytes());
        }
    }

    /// Implements <https://www.w3.org/TR/rdf-canon/#hash-nd-quads>
    fn hash_n_degree_quads<E: std::error::Error + Send + Sync + 'static>(
        &self,
        identifier: &str,
        issuer: &BnodeIssuer,
        depth: usize,
    ) -> Result<(H::Output, BnodeIssuer), C14nError<E>> {
        if depth as f32 > self.depth_factor * self.b2q.len() as f32 {
            return Err(C14nError::ToxicGraph(format!(
                "too many recursions (limit={} per bnode)",
                self.depth_factor
            )));
        }
        // Step 1
        let mut hn = BTreeMap::<H::Output, Vec<Box<str>>>::new();
        // Step 2
        let quads = self.b2q.get(identifier).unwrap();
        // Step 3
        for quad in quads {
            for (component, position) in iter_spog(quad.spog()).zip(["s", "p", "o", "g"].iter()) {
                assert!(!component.is_triple() && !component.is_variable());
                if let Some(bnid) = component.bnode_id() {
                    if &bnid == identifier {
                        continue;
                    }
                    let hash = self.hash_related_bnode(&bnid, quad, issuer, position);
                    let bnid = Box::from(bnid.as_str());
                    hn.entry(hash).or_default().push(bnid);
                }
            }
        }
        // Step 4
        let mut data_to_hash = H::initialize();
        // Step 5
        let mut ret_issuer: Option<BnodeIssuer> = None;
        for (related_hash, mut blank_node) in hn {
            data_to_hash.update(hex(&related_hash));
            let mut chosen_path = String::new();
            let mut chosen_issuer: Option<BnodeIssuer> = None;
            // Step 5.4
            if blank_node.len() > self.permutation_limit {
                return Err(C14nError::ToxicGraph(format!(
                    "Too many permutations ({} nodes, limit set to {})",
                    blank_node.len(),
                    self.permutation_limit,
                )));
            }
            for_each_permutation_of(&mut blank_node, |p| -> Result<(), C14nError<_>> {
                let mut issuer_copy = ret_issuer.as_ref().unwrap_or(issuer).clone();
                let mut path = String::new();
                let mut recursion_list = vec![];
                // Step 5.4.4
                for related in p {
                    if let Some(canon_id) = self.canonical.issued.get(related.as_ref()) {
                        path.push_str("_:");
                        path.push_str(canon_id);
                    } else {
                        let (id, new) = issuer_copy.issue(related);
                        if new {
                            recursion_list.push(related.as_ref());
                        }
                        path.push_str("_:");
                        path.push_str(id);
                    }
                }
                if !chosen_path.is_empty() && smaller_path(&chosen_path, &path) {
                    return Ok(()); // skip to the next permutation
                }
                // Step 5.4.5
                for related in recursion_list {
                    let result = self.hash_n_degree_quads(related, &issuer_copy, depth + 1)?;
                    let (id, _) = issuer_copy.issue(related);
                    path.push_str("_:");
                    path.push_str(id);
                    path.push('<');
                    path.push_str(&hex(&result.0));
                    path.push('>');
                    issuer_copy = result.1;
                    if !chosen_path.is_empty() && smaller_path(&chosen_path, &path) {
                        return Ok(()); // skip to the next permutation
                    }
                }
                // Step 5.4.6
                if chosen_path.is_empty() || path < chosen_path {
                    chosen_path = path;
                    chosen_issuer = Some(issuer_copy);
                }
                Ok(())
            })?;
            data_to_hash.update(chosen_path.as_bytes());
            ret_issuer = chosen_issuer;
        }
        let ret = (
            data_to_hash.finalize(),
            ret_issuer.unwrap_or_else(|| issuer.clone()),
        );
        debug_assert!({
            log::trace!(
                "hash-n-degree({}, {})\n-> {}",
                identifier,
                depth,
                hex(&ret.0)
            );
            true
        });
        Ok(ret)
    }
}

#[derive(Clone, Debug)]
struct BnodeIssuer {
    prefix: BnodeId<&'static str>,
    //counter: usize, // use issued_order.len() instead
    issued: C14nIdMap,
    // Not specified in the spec: allows to keep the order in which identifiers were issued
    issued_order: Vec<Rc<str>>,
}

impl BnodeIssuer {
    const fn new(prefix: BnodeId<&'static str>) -> Self {
        Self {
            prefix,
            issued: BTreeMap::new(),
            issued_order: vec![],
        }
    }

    /// Implements <https://www.w3.org/TR/rdf-canon/#issue-identifier>
    /// modified to also return a boolean indicating whether the issued identifier
    /// was newly created (true) or if it existed before (false)
    fn issue(&mut self, bnid: &str) -> (&str, bool) {
        let key = Rc::from(bnid);
        let key2 = Rc::clone(&key);
        let mut new = false;
        let ret = self.issued.entry(key).or_insert_with(|| {
            new = true;
            let counter = self.issued_order.len();
            self.issued_order.push(key2);
            BnodeId::new_unchecked(format!("{}{}", self.prefix.as_str(), counter).into())
        });
        (ret.as_str(), new)
    }
}

/// Implements <https://www.w3.org/TR/rdf-canon/#hash-1d-quads>
/// with the difference that the C14n state is not passed;
/// instead, the quad list corresponding to bnid is passed directly
fn hash_first_degree_quads<H: HashFunction, Q: Quad>(bnid: &str, quads: &[&Q]) -> H::Output {
    let mut nquads: Vec<_> = quads
        .iter()
        .map(|q| {
            let mut line = String::new();
            nq_for_hash(q.s(), &mut line, bnid);
            nq_for_hash(q.p(), &mut line, bnid);
            nq_for_hash(q.o(), &mut line, bnid);
            if let Some(gn) = q.g() {
                nq_for_hash(gn, &mut line, bnid);
            }
            line.push_str(".\n");
            line
        })
        .collect();
    nquads.sort_unstable();
    let mut hasher = H::initialize();
    for line in nquads {
        hasher.update(&line);
    }
    let ret = hasher.finalize();
    debug_assert!({
        log::trace!("hash-first-degree({})\n-> {}", bnid, hex(&ret));
        true
    });
    ret
}

fn nq_for_hash<T: Term>(term: T, buffer: &mut String, ref_bnid: &str) {
    if let Some(bnid) = term.bnode_id() {
        if &bnid == ref_bnid {
            buffer.push_str("_:a ");
        } else {
            buffer.push_str("_:z ");
        }
    } else {
        nq(term.borrow_term(), buffer);
    }
}

fn hex(hash: &impl AsRef<[u8]>) -> String {
    let mut digest = String::with_capacity(64);
    for b in hash.as_ref() {
        write!(&mut digest, "{b:02x}").unwrap();
    }
    digest
}

fn smaller_path(path1: &str, path2: &str) -> bool {
    use std::cmp::Ordering::{Equal, Greater, Less};
    match Ord::cmp(&path1.len(), &path2.len()) {
        Less => true,
        Equal => path1 < path2,
        Greater => false,
    }
}

/// Iter over all the components of a [`Quad`] as Option.
///
/// Compared to [`iter_spog`], this function always return 4 components.
fn iter_spog_opt<T: Quad>(q: T) -> impl Iterator<Item = Option<T::Term>> {
    let (spo, g) = q.to_spog();
    spo.into_iter().map(Some).chain(std::iter::once(g))
}

#[cfg(test)]
mod test {
    use super::*;
    use sophia_api::term::{LanguageTag, SimpleTerm, VarName};

    #[test]
    fn example2() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "<http://example.com/#p> <http://example.com/#q> _:e0 .",
            "<http://example.com/#p> <http://example.com/#r> _:e1 .",
            "_:e0 <http://example.com/#s> <http://example.com/#u> .",
            "_:e1 <http://example.com/#t> <http://example.com/#u> .",
        ]);
        let exp = r"<http://example.com/#p> <http://example.com/#q> _:c14n0 .
<http://example.com/#p> <http://example.com/#r> _:c14n1 .
_:c14n0 <http://example.com/#s> <http://example.com/#u> .
_:c14n1 <http://example.com/#t> <http://example.com/#u> .
";
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    #[test]
    fn example3() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "<http://example.com/#p> <http://example.com/#q> _:e0 .",
            "<http://example.com/#p> <http://example.com/#q> _:e1 .",
            "_:e0 <http://example.com/#p> _:e2 .",
            "_:e1 <http://example.com/#p> _:e3 .",
            "_:e2 <http://example.com/#r> _:e3 .",
        ]);
        let exp = r"<http://example.com/#p> <http://example.com/#q> _:c14n2 .
<http://example.com/#p> <http://example.com/#q> _:c14n3 .
_:c14n0 <http://example.com/#r> _:c14n1 .
_:c14n2 <http://example.com/#p> _:c14n1 .
_:c14n3 <http://example.com/#p> _:c14n0 .
";
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    #[test]
    fn cycle5() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "_:e0 <http://example.com/#p> _:e1 .",
            "_:e1 <http://example.com/#p> _:e2 .",
            "_:e2 <http://example.com/#p> _:e3 .",
            "_:e3 <http://example.com/#p> _:e4 .",
            "_:e4 <http://example.com/#p> _:e0 .",
        ]);
        let exp = r"_:c14n0 <http://example.com/#p> _:c14n4 .
_:c14n1 <http://example.com/#p> _:c14n0 .
_:c14n2 <http://example.com/#p> _:c14n1 .
_:c14n3 <http://example.com/#p> _:c14n2 .
_:c14n4 <http://example.com/#p> _:c14n3 .
";
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    #[test]
    fn cycle5_toxic() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "_:e0 <http://example.com/#p> _:e1 .",
            "_:e1 <http://example.com/#p> _:e2 .",
            "_:e2 <http://example.com/#p> _:e3 .",
            "_:e3 <http://example.com/#p> _:e4 .",
            "_:e4 <http://example.com/#p> _:e0 .",
        ]);
        let mut output = Vec::<u8>::new();
        // set depth_factor too low for this graph
        let res = normalize_with::<Sha256, _, _, true>(
            &dataset,
            &mut output,
            0.5,
            2 * DEFAULT_PERMUTATION_LIMIT,
        );
        assert!(matches!(res, Err(C14nError::ToxicGraph(_))));
    }

    #[test]
    fn clique5() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "_:e0 <http://example.com/#p> _:e1 .",
            "_:e0 <http://example.com/#p> _:e2 .",
            "_:e0 <http://example.com/#p> _:e3 .",
            "_:e0 <http://example.com/#p> _:e4 .",
            "_:e1 <http://example.com/#p> _:e0 .",
            "_:e1 <http://example.com/#p> _:e2 .",
            "_:e1 <http://example.com/#p> _:e3 .",
            "_:e1 <http://example.com/#p> _:e4 .",
            "_:e2 <http://example.com/#p> _:e0 .",
            "_:e2 <http://example.com/#p> _:e1 .",
            "_:e2 <http://example.com/#p> _:e3 .",
            "_:e2 <http://example.com/#p> _:e4 .",
            "_:e3 <http://example.com/#p> _:e0 .",
            "_:e3 <http://example.com/#p> _:e1 .",
            "_:e3 <http://example.com/#p> _:e2 .",
            "_:e3 <http://example.com/#p> _:e4 .",
            "_:e4 <http://example.com/#p> _:e0 .",
            "_:e4 <http://example.com/#p> _:e1 .",
            "_:e4 <http://example.com/#p> _:e2 .",
            "_:e4 <http://example.com/#p> _:e3 .",
        ]);
        let exp = r"_:c14n0 <http://example.com/#p> _:c14n1 .
_:c14n0 <http://example.com/#p> _:c14n2 .
_:c14n0 <http://example.com/#p> _:c14n3 .
_:c14n0 <http://example.com/#p> _:c14n4 .
_:c14n1 <http://example.com/#p> _:c14n0 .
_:c14n1 <http://example.com/#p> _:c14n2 .
_:c14n1 <http://example.com/#p> _:c14n3 .
_:c14n1 <http://example.com/#p> _:c14n4 .
_:c14n2 <http://example.com/#p> _:c14n0 .
_:c14n2 <http://example.com/#p> _:c14n1 .
_:c14n2 <http://example.com/#p> _:c14n3 .
_:c14n2 <http://example.com/#p> _:c14n4 .
_:c14n3 <http://example.com/#p> _:c14n0 .
_:c14n3 <http://example.com/#p> _:c14n1 .
_:c14n3 <http://example.com/#p> _:c14n2 .
_:c14n3 <http://example.com/#p> _:c14n4 .
_:c14n4 <http://example.com/#p> _:c14n0 .
_:c14n4 <http://example.com/#p> _:c14n1 .
_:c14n4 <http://example.com/#p> _:c14n2 .
_:c14n4 <http://example.com/#p> _:c14n3 .
";
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    #[test]
    fn clique5_toxic() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "_:e0 <http://example.com/#p> _:e1 .",
            "_:e0 <http://example.com/#p> _:e2 .",
            "_:e0 <http://example.com/#p> _:e3 .",
            "_:e0 <http://example.com/#p> _:e4 .",
            "_:e1 <http://example.com/#p> _:e0 .",
            "_:e1 <http://example.com/#p> _:e2 .",
            "_:e1 <http://example.com/#p> _:e3 .",
            "_:e1 <http://example.com/#p> _:e4 .",
            "_:e2 <http://example.com/#p> _:e0 .",
            "_:e2 <http://example.com/#p> _:e1 .",
            "_:e2 <http://example.com/#p> _:e3 .",
            "_:e2 <http://example.com/#p> _:e4 .",
            "_:e3 <http://example.com/#p> _:e0 .",
            "_:e3 <http://example.com/#p> _:e1 .",
            "_:e3 <http://example.com/#p> _:e2 .",
            "_:e3 <http://example.com/#p> _:e4 .",
            "_:e4 <http://example.com/#p> _:e0 .",
            "_:e4 <http://example.com/#p> _:e1 .",
            "_:e4 <http://example.com/#p> _:e2 .",
            "_:e4 <http://example.com/#p> _:e3 .",
        ]);
        let mut output = Vec::<u8>::new();
        // set permutation limit too low for this graph
        let res =
            normalize_with::<Sha256, _, _, true>(&dataset, &mut output, 2.0 * DEFAULT_DEPTH_FACTOR, 3);
        assert!(matches!(res, Err(C14nError::ToxicGraph(_))));
    }

    #[test]
    fn cycle2plus3() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "_:e0 <http://example.com/#p> _:e1 .",
            "_:e1 <http://example.com/#p> _:e0 .",
            "_:e2 <http://example.com/#p> _:e3 .",
            "_:e3 <http://example.com/#p> _:e4 .",
            "_:e4 <http://example.com/#p> _:e2 .",
        ]);
        let exp = r"_:c14n0 <http://example.com/#p> _:c14n1 .
_:c14n1 <http://example.com/#p> _:c14n0 .
_:c14n2 <http://example.com/#p> _:c14n4 .
_:c14n3 <http://example.com/#p> _:c14n2 .
_:c14n4 <http://example.com/#p> _:c14n3 .
";
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    #[test]
    fn tricky_order() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "<tag:a> <tag:p> _:a .",
            "<tag:a> <tag:p> <tag:a> .",
            "<tag:a> <tag:p> 'a' .",
            "<tag:a> <tag:p> 'a!' .",
            "<tag:a9> <tag:p> 'a!' .",
        ]);
        let exp = r#"<tag:a9> <tag:p> "a!" .
<tag:a> <tag:p> "a!" .
<tag:a> <tag:p> "a" .
<tag:a> <tag:p> <tag:a> .
<tag:a> <tag:p> _:c14n0 .
"#;
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }

    pub fn c14n_nquads<D: SetDataset>(d: &D) -> Result<String, C14nError<D::Error>> {
        let mut output = Vec::<u8>::new();
        normalize(d, &mut output)?;
        Ok(unsafe { String::from_utf8_unchecked(output) })
    }

    /// Simplistic Quad parser, useful for writing test cases.
    /// It is based on `eq_quad` below.
    fn ez_quads<'a>(lines: &[&'a str]) -> std::collections::HashSet<Spog<SimpleTerm<'a>>> {
        lines.iter().map(|line| ez_quad(line)).collect()
    }

    /// Simplistic Quad parser, useful for writing test cases.
    /// The syntax is a subset of N-Quads-star,
    /// where spaces are not allowed in literals, and a space is required before the ending '.'.
    fn ez_quad(txt: &str) -> Spog<SimpleTerm<'_>> {
        let mut tokens: Vec<_> = txt.split(' ').collect();
        assert!(tokens.len() == 4 || tokens.len() == 5);
        assert!(tokens.pop().unwrap() == ".");
        let g = if tokens.len() == 4 {
            tokens.pop().map(ez_term)
        } else {
            None
        };
        let o = ez_term(tokens.pop().unwrap());
        let p = ez_term(tokens.pop().unwrap());
        let s = ez_term(tokens.pop().unwrap());
        ([s, p, o], g)
    }

    /// Simplistic Term parser, useful for writing test cases.
    /// The syntax is a subset of Turtle 1.2
    /// (with the caveat that triple terms still use the Turtle-star syntax,
    ///  i.e. << ... >> instead of <<( ... )>> )
    fn ez_term(txt: &str) -> SimpleTerm<'_> {
        use sophia_iri::IriRef;
        match txt.as_bytes() {
            [b'<', b'<', .., b'>', b'>'] => {
                let subterms: Vec<&str> = txt[2..txt.len() - 2].split(' ').collect();
                assert_eq!(subterms.len(), 3);
                SimpleTerm::Triple(Box::new([
                    ez_term(subterms[0]),
                    ez_term(subterms[1]),
                    ez_term(subterms[2]),
                ]))
            }
            [b'<', .., b'>'] => IriRef::new_unchecked(&txt[1..txt.len() - 1]).into_term(),
            [b':', ..] => {
                let iri = format!("tag:{}", &txt[1..]);
                SimpleTerm::Iri(IriRef::new_unchecked(iri.into()))
            }
            [b'_', b':', ..] => BnodeId::new_unchecked(&txt[2..]).into_term(),
            [b'\'', .., b'\''] => (&txt[1..txt.len() - 1]).into_term(),
            [b'\'', .., b'\'', b'@', _, _] => SimpleTerm::LiteralLanguage(
                (&txt[1..txt.len() - 4]).into(),
                LanguageTag::new_unchecked(txt[txt.len() - 2..].into()),
                None,
            ),
            [b'\'', .., b'\'', b'@', _, _, b'-', b'-', _, _, _] => SimpleTerm::LiteralLanguage(
                (&txt[1..txt.len() - 5]).into(),
                LanguageTag::new_unchecked(txt[txt.len() - 7..txt.len() - 5].into()),
                Some(txt[txt.len() - 3..].parse().unwrap()),
            ),
            [c, ..] if c.is_ascii_digit() => txt.parse::<i32>().unwrap().into_term(),
            [b'?', ..] => VarName::new_unchecked(&txt[1..]).into_term(),
            _ => panic!("ez_term can not parse this"),
        }
    }

    #[test]
    fn example2_sha384() {
        crate::test_setup();

        let dataset = ez_quads(&[
            "<http://example.com/#p> <http://example.com/#q> _:e0 .",
            "<http://example.com/#p> <http://example.com/#r> _:e1 .",
            "_:e0 <http://example.com/#s> <http://example.com/#u> .",
            "_:e1 <http://example.com/#t> <http://example.com/#u> .",
        ]);
        let exp = r"<http://example.com/#p> <http://example.com/#q> _:c14n1 .
<http://example.com/#p> <http://example.com/#r> _:c14n0 .
_:c14n0 <http://example.com/#t> <http://example.com/#u> .
_:c14n1 <http://example.com/#s> <http://example.com/#u> .
";
        let mut got = Vec::<u8>::new();
        normalize_sha384(&dataset, &mut got).unwrap();
        let got = unsafe { String::from_utf8_unchecked(got) };
        println!(">>>> GOT\n{got}>>>> EXPECTED\n{exp}<<<<");
        assert!(got == exp);
    }
}
