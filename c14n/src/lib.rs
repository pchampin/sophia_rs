//! This crate is part of [Sophia],
//! an [RDF] and [Linked Data] toolkit in Rust.
//!
//! This crate provides function to canonicalize graphs and datasets,
//! following the algorithm described at:
//!
//!   https://www.w3.org/TR/rdf-canon/
//!
//! For the moment, the algorithms in this module *panic*
//! when faced with RDF-star or variables.
use std::borrow::Borrow;
use std::collections::btree_map::Entry::*;
use std::collections::BTreeMap;
use std::rc::Rc;

use sophia_api::dataset::{DTerm, Dataset};
use sophia_api::quad::{iter_spog, Quad, Spog};
use sophia_api::term::{BnodeId, Term, TermKind};

mod c14n_term;
use c14n_term::C14nTerm;

/// Return the sorted n-quad using canonical blank node labels for `d`.
///
pub fn c14n_nquads<D: Dataset>(d: &D) -> Result<String, D::Error> {
    let mut lines: Vec<_> = c14n_dataset(d)?
        .iter()
        .map(|q| {
            let mut line = String::new();
            nq(q.s(), &mut line);
            nq(q.p(), &mut line);
            nq(q.o(), &mut line);
            if let Some(gn) = q.g() {
                nq(gn, &mut line);
            }
            line.push_str(".\n");
            line
        })
        .collect();
    lines.sort_unstable(); // TODO check that UTF-8-sort is indeed equivalent to code point order
    let mut output = String::new();
    for line in lines {
        output.push_str(&line);
    }
    Ok(output)
}

/// Return a canonical form of the dataset `d`.
///
/// Implements https://www.w3.org/TR/rdf-canon/#canon-algorithm
pub fn c14n_dataset<'a, D: Dataset>(
    d: &'a D,
) -> Result<C14nQuads<'a, D>, D::Error> {
    let quads: Result<Vec<Spog<DTerm<'a, D>>>, _> =
        d.quads().map(|res| res.map(Quad::to_spog)).collect();
    let quads = quads?;
    // Step 1
    let mut state = C14nState::new();
    // Step 2
    for quad in &quads {
        for term in iter_spog(quad.spog()) {
            assert!(!term.is_triple() && !term.is_variable());
            if let Some(bnid) = term.bnode_id() {
                match state.b2q.entry(Rc::from(bnid.as_str())) {
                    Vacant(e) => {
                        e.insert(vec![quad]);
                    }
                    Occupied(mut e) => e.get_mut().push(quad),
                }
            }
        }
    }
    // Step 3
    for (bnid, quads) in state.b2q.iter() {
        let hash = hash_first_degree_quads(bnid, &quads[..]);
        let bnid = Rc::clone(bnid);
        match state.h2b.entry(hash) {
            Vacant(e) => {
                e.insert(vec![bnid]);
            }
            Occupied(mut e) => e.get_mut().push(bnid),
        }
    }
    // Step 4
    // NB: we are relying on the fact that BTreeMap's elements are sorted
    let mut next_h2b = BTreeMap::new();
    // TODO later, this should be using drain_filter instead of relying on next_h2b/
    for (hash, bnids) in state.h2b.into_iter() {
        debug_assert!(!bnids.is_empty());
        if bnids.len() > 1 {
            next_h2b.insert(hash, bnids);
        } else {
            state.canonical.issue(&bnids[0]);
        }
    }
    state.h2b = next_h2b;
    // Step 5
    for (_hash, _bnids) in state.h2b {
        panic!("I only support very simple cases at the moment");
    }
    // Step 6
    let issued = state.canonical.issued;
    Ok(quads
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
        .collect())
}

type C14nQuads<'a, D> = Vec<Spog<C14nTerm<DTerm<'a, D>>>>;

#[derive(Clone, Debug)]
struct C14nState<'a, T: Term> {
    b2q: BTreeMap<Rc<str>, Vec<&'a Spog<T>>>,
    h2b: BTreeMap<Hash, Vec<Rc<str>>>,
    canonical: BnodeIssuer<&'static str>,
}

impl<'a, T: Term> C14nState<'a, T> {
    fn new() -> Self {
        C14nState {
            b2q: BTreeMap::new(),
            h2b: BTreeMap::new(),
            canonical: BnodeIssuer::new(BnodeId::new_unchecked("c14n")),
        }
    }
}

#[derive(Clone, Debug)]
struct BnodeIssuer<T: Borrow<str>> {
    prefix: BnodeId<T>,
    counter: usize,
    issued: BTreeMap<Rc<str>, BnodeId<Rc<str>>>,
}

impl<T> BnodeIssuer<T>
where
    T: Borrow<str>,
{
    fn new(prefix: BnodeId<T>) -> Self {
        BnodeIssuer {
            prefix,
            counter: 0,
            issued: BTreeMap::new(),
        }
    }

    /// Implements https://www.w3.org/TR/rdf-canon/#issue-identifier
    /// except that it does not return the issued identifier (which is never used)
    fn issue(&mut self, bnid: &str) {
        match self.issued.entry(Rc::from(bnid)) {
            Occupied(_) => (),
            Vacant(e) => {
                e.insert(BnodeId::new_unchecked(
                    format!("{}{}", self.prefix.as_str(), self.counter).into(),
                ));
                self.counter += 1;
            }
        }
    }
}

type Hash = [u8; 32];

/// Implements https://www.w3.org/TR/rdf-canon/#hash-1d-quads
/// with the difference that the C14n state is not passed;
/// instead, the quad list corresponding to bnid is passed directly
fn hash_first_degree_quads<T: Quad>(bnid: &str, quads: &[&T]) -> Hash {
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
    nquads.sort_unstable(); // TODO check that UTF-8-sort is indeed equivalent to code point order
    let mut hasher = hmac_sha256::Hash::new();
    for line in nquads.into_iter() {
        hasher.update(&line);
    }
    let ret = hasher.finalize();
    debug_assert!({
        use std::fmt::Write;
        let mut digest = String::with_capacity(64);
        for b in ret {
            write!(&mut digest, "{:02x}", b).unwrap();
        }
        println!("hash-firt-degree({}) -> {}", bnid, digest);
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

fn nq<T: Term>(term: T, buffer: &mut String) {
    match term.kind() {
        TermKind::Iri => {
            buffer.push('<');
            buffer.push_str(&term.iri().unwrap());
            buffer.push('>');
        }
        TermKind::Literal => {
            buffer.push('"');
            for c in term.lexical_form().unwrap().chars() {
                match c {
                    '"' => buffer.push_str("\\\""),
                    '\\' => buffer.push_str("\\\\"),
                    '\n' => buffer.push_str("\\n"),
                    '\r' => buffer.push_str("\\r"),
                    _ => buffer.push(c),
                }
            }
            buffer.push('"');
            if let Some(tag) = term.language_tag() {
                buffer.push('@');
                buffer.push_str(&tag);
            } else {
                buffer.push_str("^^");
                nq(term.datatype().unwrap(), buffer);
            }
        }
        TermKind::BlankNode => {
            buffer.push_str("_:");
            buffer.push_str(&term.bnode_id().unwrap());
        }
        TermKind::Triple => panic!("quoted triples not supported by c14n"),
        TermKind::Variable => panic!("variables not supported by c14n"),
    }
    buffer.push(' ');
}

#[cfg(test)]
mod test {
    use sophia_api::term::{LanguageTag, SimpleTerm, VarName};

    use super::*;

    #[test]
    fn example2() {
        let dataset = ez_quads(&[
            "<http://example.com/#p> <http://example.com/#q> _:e0 .",
            "<http://example.com/#p> <http://example.com/#r> _:e1 .",
            "_:e0 <http://example.com/#s> <http://example.com/#u> .",
            "_:e1 <http://example.com/#t> <http://example.com/#u> .",
        ]);
        let exp = r#"<http://example.com/#p> <http://example.com/#q> _:c14n0 .
<http://example.com/#p> <http://example.com/#r> _:c14n1 .
_:c14n0 <http://example.com/#s> <http://example.com/#u> .
_:c14n1 <http://example.com/#t> <http://example.com/#u> .
"#;
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>>\n{}====\n{}<<<<", got, exp);
        assert!(got == exp);
    }

    #[test]
    fn example3() {
        let dataset = ez_quads(&[
            "<http://example.com/#p> <http://example.com/#q> _:e0 .",
            "<http://example.com/#p> <http://example.com/#q> _:e1 .",
            "_:e0 <http://example.com/#p> _:e2 .",
            "_:e1 <http://example.com/#p> _:e3 .",
            "_:e2 <http://example.com/#r> _:e3 .",
        ]);
        let exp = r#"<http://example.com/#p> <http://example.com/#q> _:c14n2 .
<http://example.com/#p> <http://example.com/#q> _:c14n3 .
_:c14n0 <http://example.com/#r> _:c14n1 .
_:c14n2 <http://example.com/#p> _:c14n1 .
_:c14n3 <http://example.com/#p> _:c14n0 .
"#;
        let got = c14n_nquads(&dataset).unwrap();
        println!(">>>>\n{}====\n{}<<<<", got, exp);
        assert!(got == exp);
    }

    /// Simplisitic Quad parser, useful for writing test cases.
    /// It is based on eq_quad below.
    fn ez_quads<'a>(lines: &[&'a str]) -> Vec<Spog<SimpleTerm<'a>>> {
        lines.iter().map(|line| ez_quad(line)).collect()
    }

    /// Simplisitic Quad parser, useful for writing test cases.
    /// The syntax is a subset of N-Quads-star,
    /// where spaces are not allowed in literals, and a space is required before the ending '.'.
    fn ez_quad(txt: &str) -> Spog<SimpleTerm> {
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
    /// The syntax is a subset of Turtle-star.
    fn ez_term(txt: &str) -> SimpleTerm {
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
            ),
            [c, ..] if c.is_ascii_digit() => txt.parse::<i32>().unwrap().into_term(),
            [b'?', ..] => VarName::new_unchecked(&txt[1..]).into_term(),
            _ => panic!("Unable to parse term"),
        }
    }
}
