//! Utility code for pretty-printing Turtle.

use super::TurtleConfig;
use sophia_api::dataset::adapter::DatasetGraph;
use sophia_api::graph::Graph;
use sophia_api::ns::rdf;
use sophia_api::term::{CopiableTerm, TTerm, TermKind};
use sophia_api::triple::Triple;
use sophia_inmem::dataset::FastDataset;
use sophia_term::RcTerm;
use std::collections::{HashMap, HashSet};
use std::io;

pub type PrettifiableGraph<'a> = DatasetGraph<FastDataset, &'a FastDataset, Option<&'a RcTerm>>;

/// Serialize `graph` in pretty turtle on `write`, using the given `config`.
/// `anon_blacklist` is a set of blank nodes labels that must not be hidden in ANON or lists
/// (because ther are used otherwise -- e.g. in another named graph, if this is used for TriG).
/// `base_indent` is the base indentation to be used for the whole graph.
pub fn prettify<W>(
    graph: PrettifiableGraph<'_>,
    write: W,
    config: &TurtleConfig,
    anon_blacklist: &HashSet<RcTerm>,
    base_indent: &str,
) -> io::Result<()>
where
    W: io::Write,
{
    assert!(base_indent.chars().all(char::is_whitespace));
    let subjects = graph.subjects().unwrap();
    let mut roots = Vec::new();
    let mut anons = HashSet::new();
    for subject in subjects {
        let (anon, root) = check_anon_root(&graph, &subject, anon_blacklist);
        if anon {
            anons.insert(subject.clone());
        }
        if root {
            roots.push(subject);
        }
    }

    let lists = build_lists(&graph, &mut anons);

    let mut p = Prettifier {
        write,
        indent: base_indent.to_string(),
        config,
        anons,
    };

    let gd = GraphData {
        graph,
        roots,
        lists,
    };
    p.write_all(&gd)
}

fn check_anon_root(
    g: &PrettifiableGraph<'_>,
    n: &RcTerm,
    blacklist: &HashSet<RcTerm>,
) -> (bool, bool) {
    let indeg = g.triples_with_o(n).take(2).count();
    let anon = n.kind() == TermKind::BlankNode
        && indeg <= 1
        && g.triples_with_p(n).take(1).count() == 0
        && !blacklist.contains(n);
    let root = !anon || indeg == 0;
    (anon, root)
}

fn build_lists(
    g: &PrettifiableGraph<'_>,
    anons: &mut HashSet<RcTerm>,
) -> HashMap<RcTerm, Vec<RcTerm>> {
    let mut lists = HashMap::new();
    for t in g.triples_with_po(&rdf::rest, &rdf::nil).map(Result::unwrap) {
        let n = t.s();
        if !anons.contains(n) {
            continue;
        }
        if let Some((item, opt_pred)) = list_link(g, n) {
            let mut items = vec![item];
            let head = build_list(g, t.s().clone(), opt_pred, &mut items, anons);
            items.reverse();
            lists.insert(head, items);
        }
    }
    lists
}

/// n is a valid list link in g,
/// opt_pred is its rdf:rest-predecessor (if any), and
/// items contains (in reverse order) the items of that list.
/// return the furthest rdf:rest-ancestor of n (possibly n itself)
/// that is a valid list link, and update itemsPN accordingly
fn build_list(
    g: &PrettifiableGraph<'_>,
    n: RcTerm,
    opt_pred: Option<RcTerm>,
    items: &mut Vec<RcTerm>,
    anons: &mut HashSet<RcTerm>,
) -> RcTerm {
    anons.remove(&n);
    match opt_pred {
        None => n,
        Some(pred) => {
            if !anons.contains(&pred) {
                n
            } else {
                match list_link(g, &pred) {
                    None => n,
                    Some((item, opt_pred2)) => {
                        items.push(item);
                        build_list(g, pred, opt_pred2, items, anons)
                    }
                }
            }
        }
    }
}

/// n is a blank node and the rdf:rest-predecessor of a valid list link.
/// if this blank node is itself a valid list link
/// (i.e. it has exactly one rdf:first and one rdf:rest outgoing arc,
/// and at most one extra arc, incoming or outgoing)
/// then return Some(first_value, opt_pred),
/// whered opt_pred is the rdf:rest-predecessor, if any;
/// otherwise return None.
fn list_link(g: &PrettifiableGraph<'_>, n: &RcTerm) -> Option<(RcTerm, Option<RcTerm>)> {
    let mut item = None;
    let mut rest = false;
    let mut extra = false;
    for t in g.triples_with_s(n).map(Result::unwrap) {
        if t.p() == &rdf::first {
            if item.is_none() {
                item = Some(t.o().clone());
            } else {
                return None;
            }
        } else if t.p() == &rdf::rest {
            if !rest {
                rest = true;
            } else {
                return None;
            }
        } else if !extra {
            extra = true;
        } else {
            return None;
        }
    }
    let mut pred = None;
    for t in g.triples_with_o(n).map(Result::unwrap) {
        if extra {
            return None;
        }
        extra = true;
        if t.p() == &rdf::rest {
            pred = Some(t.s().clone())
        }
    }
    Some((item.unwrap(), pred))
}

struct GraphData<'a> {
    graph: PrettifiableGraph<'a>,
    roots: Vec<RcTerm>,
    lists: HashMap<RcTerm, Vec<RcTerm>>,
}

struct Prettifier<'a, W> {
    write: W,
    indent: String,
    config: &'a TurtleConfig,
    anons: HashSet<RcTerm>,
}

impl<'a, W: io::Write> Prettifier<'a, W> {
    fn write_all(&mut self, gd: &GraphData) -> io::Result<()> {
        for root in &gd.roots {
            self.write_root(gd, root)?;
        }
        // break blank node cycles that are still in anons
        while let Some(bnode) = self.anons.iter().next().cloned() {
            self.anons.remove(&bnode);
            self.write_root(gd, &bnode)?;
        }
        self.write_bytes(b"\n")?;
        Ok(())
    }

    fn write_root(&mut self, gd: &GraphData, root: &RcTerm) -> io::Result<()> {
        self.write_newline()?;
        self.write_term(gd, root, true)?;
        self.write_bytes(b" ")?;
        self.write_properties(gd, root, gd.lists.get(root).is_some())?;
        self.write_bytes(b".\n")
    }

    fn write_term(&mut self, gd: &GraphData, node: &RcTerm, root: bool) -> io::Result<()> {
        use TermKind::*;
        if node.kind() == BlankNode {
            self.write_bnode(gd, node, root)
        } else {
            super::write_term(&mut self.write, node, self.config, false)
        }
    }

    fn write_properties(
        &mut self,
        gd: &GraphData,
        node: &RcTerm,
        skip_list_predicates: bool,
    ) -> io::Result<()> {
        self.indent(); // to predicate-level
        let mut predicate = None;
        let types: Vec<_> = gd
            .graph
            .triples_with_sp(node, &rdf::type_)
            .map(Result::unwrap)
            .map(|t| t.o().clone())
            .collect();
        if !types.is_empty() {
            predicate = Some(rdf::type_.copied());
            self.write_bytes(b"a ")?;
            self.indent(); // to object-level
            self.write_objects(gd, &types)?;
        }
        // NB: we know that PrettifiableGraph<'_> iterates triples grouped by predicate
        // (it is based on FastDataset, which uses GSPO indexes)
        for t in gd.graph.triples_with_s(node).map(Result::unwrap) {
            let p = t.p();
            if p == &rdf::type_ || skip_list_predicates && (p == &rdf::first || p == &rdf::rest) {
                continue;
            }
            if Some(t.p()) != predicate.as_ref() {
                if predicate.is_some() {
                    self.write_bytes(b";")?;
                    self.unindent(); // back to predicate-level
                }
                predicate = Some(t.p().clone());
                self.write_newline()?;
                self.write_term(gd, predicate.as_ref().unwrap(), false)?;
                self.write_bytes(b" ")?;
                self.indent(); // to object-level
            } else {
                self.write_bytes(b",")?;
                self.write_newline()?;
            }
            self.write_term(gd, t.o(), false)?;
        }
        if predicate.is_some() {
            self.unindent(); // back to predicate-level
        }
        self.unindent(); // back to original level
        Ok(())
    }

    fn write_objects(&mut self, gd: &GraphData, objects: &[RcTerm]) -> io::Result<()> {
        self.write_term(gd, &objects[0], false)?;
        for obj in &objects[1..] {
            self.write_bytes(b",")?;
            self.write_newline()?;
            self.write_term(gd, obj, false)?;
        }
        Ok(())
    }

    fn write_bnode(&mut self, gd: &GraphData, bn: &RcTerm, root: bool) -> io::Result<()> {
        if let Some(items) = gd.lists.get(bn) {
            self.write_bytes(b"(")?;
            self.indent();
            for item in items {
                self.write_newline()?;
                self.write_term(gd, item, false)?;
            }
            self.unindent();
            self.write_newline()?;
            self.write_bytes(b")")
        } else if self.anons.remove(bn) {
            if root {
                self.write_bytes(b"[]")
            } else {
                self.write_bytes(b"[ ")?;
                self.write_properties(gd, bn, false)?;
                self.write_bytes(b" ]")
            }
        } else {
            write!(self.write, "_:{}", bn.value_raw().0)
        }
    }

    fn write_newline(&mut self) -> io::Result<()> {
        self.write_bytes(b"\n")?;
        self.write.write_all(self.indent.as_bytes())
    }

    fn write_bytes(&mut self, bytes: &[u8]) -> io::Result<()> {
        self.write.write_all(bytes)
    }

    fn indent(&mut self) {
        self.indent.push_str(self.config.indentation());
    }

    fn unindent(&mut self) {
        let ilen = self.config.indentation().len();
        self.indent.truncate(self.indent.len() - ilen);
    }
}
