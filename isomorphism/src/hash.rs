//! I define parameterized hashing function,
//! where blank nodes are hashed according to a given map.

use sophia_api::quad::Quad;
use sophia_api::term::{Term, TermKind};
use sophia_api::triple::Triple;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

/// Hash this term, using a blank-node map for hashing blank nodes
pub fn hash_term_with<T, H>(t: T, map: &HashMap<&str, u64>, context: (&str, char), state: &mut H)
where
    T: Term,
    H: Hasher,
{
    if let Some(bnid) = t.bnode_id() {
        if &bnid == context.0 {
            context.1.hash(state);
        }
        map.get(bnid.as_str()).unwrap().hash(state);
    } else if let Some(tr) = t.triple() {
        TermKind::Triple.hash(state);
        hash_triple_with(&tr, map, context.0, state);
    } else {
        t.hash(state);
    }
}

/// Hash a triple, using a blank-node map for hashinh blank nodes
pub fn hash_triple_with<T, H>(t: &T, map: &HashMap<&str, u64>, context: &str, state: &mut H)
where
    T: Triple,
    H: Hasher,
{
    let [s, p, o] = t.spo();
    hash_term_with(s, map, (context, 's'), state);
    hash_term_with(p, map, (context, 'p'), state);
    hash_term_with(o, map, (context, 'o'), state);
}

/// Hash a triple, using a blank-node map for hashinh blank nodes
pub fn hash_quad_with<Q>(q: &Q, map: &HashMap<&str, u64>, context: &str) -> u64
where
    Q: Quad,
{
    let ([s, p, o], g) = q.spog();
    let mut state = std::collections::hash_map::DefaultHasher::new();
    hash_term_with(s, map, (context, 's'), &mut state);
    hash_term_with(p, map, (context, 'p'), &mut state);
    hash_term_with(o, map, (context, 'o'), &mut state);
    match g {
        None => (None as Option<i32>).hash(&mut state),
        Some(g) => hash_term_with(g, map, (context, 'g'), &mut state),
    }
    state.finish()
}
