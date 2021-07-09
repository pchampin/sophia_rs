//! Utility code for pretty-printing Turtle.

use super::TurtleConfig;
use crate::dataset::inmem::FastDataset;
use regex::Regex;
use sophia_api::dataset::adapter::DatasetGraph;
use sophia_api::graph::Graph;
use sophia_api::ns::{rdf, xsd};
use sophia_api::prefix::PrefixMap;
use sophia_api::term::{CopiableTerm, TTerm, TermKind};
use sophia_api::triple::Triple;
use sophia_term::RcTerm;
use std::collections::{HashMap, HashSet};
use std::io;

lazy_static::lazy_static! {
    /// Match an absolute IRI reference.
    pub(crate) static ref PN_LOCAL: Regex = Regex::new(r"(?x)^
        #(PN_CHARS_U | ':' | [0-9] | PLX)
        (
            [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_:0-9]
            # | PLX
            | \\ [_~.!$&'()*+,;=/?\#@%-]
            | % [0-9A-Fa-f]{2}
        )
        # ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
        (
            (
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}.:-]
                | \\ [_~.!$&'()*+,;=/?\#@%-]
                | % [0-9A-Fa-f]{2}
            )*
            (
                [A-Za-z\u{00C0}-\u{00D6}\u{00D8}-\u{00F6}\u{00F8}-\u{02FF}\u{0370}-\u{037D}\u{037F}-\u{1FFF}\u{200C}-\u{200D}\u{2070}-\u{218F}\u{2C00}-\u{2FEF}\u{3001}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFFD}\u{10000}-\u{EFFFF}_0-9\u{00B7}\u{0300}-\u{036F}\u{203F}-\u{2040}:-]
                | \\ [_~.!$&'()*+,;=/?\#@%-]
                | % [0-9A-Fa-f]{2}
            )
        )?
    $").unwrap();
    pub(crate) static ref INTEGER: Regex = Regex::new(r"^[+-]?[0-9]+$").unwrap();
    pub(crate) static ref DECIMAL: Regex = Regex::new(r"^[+-]?[0-9]*.[0-9]+$").unwrap();
    pub(crate) static ref DOUBLE: Regex = Regex::new(r"(?x)^
      [+-]? ( [0-9]+ ( . [0-9]* )? | . [0-9]+ ) [eE] [+-]? [0-9]+
    $").unwrap();
    pub(crate) static ref BOOLEAN: Regex = Regex::new(r"^(true|false)$").unwrap();
}

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
        if is_anon(&graph, &subject) && !anon_blacklist.contains(&subject) {
            anons.insert(subject);
        } else {
            roots.push(subject);
        }
    }

    let lists = build_lists(&graph, anon_blacklist);

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

fn is_anon(g: &PrettifiableGraph<'_>, n: &RcTerm) -> bool {
    n.kind() == TermKind::BlankNode
        && g.triples_with_o(n).take(2).count() == 1
        && g.triples_with_p(n).take(1).count() == 0
}

fn is_anon_root(g: &PrettifiableGraph<'_>, n: &RcTerm) -> bool {
    g.triples_with_o(n).take(1).count() == 0 && g.triples_with_p(n).take(1).count() == 0
}

fn build_lists(
    g: &PrettifiableGraph<'_>,
    blacklist: &HashSet<RcTerm>,
) -> HashMap<RcTerm, Vec<RcTerm>> {
    let mut lists = HashMap::new();
    for t in g.triples_with_po(&rdf::rest, &rdf::nil).map(Result::unwrap) {
        let n = t.s();
        if n.kind() != TermKind::BlankNode || blacklist.contains(n) {
            continue;
        }
        if let Some((item, opt_pred)) = list_link(g, n) {
            let mut items = vec![item];
            let head = build_list(g, t.s().clone(), opt_pred, &mut items, &blacklist);
            items.reverse();
            lists.insert(head.clone(), items);
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
    blacklist: &HashSet<RcTerm>,
) -> RcTerm {
    match opt_pred {
        None => n,
        Some(pred) => {
            if blacklist.contains(&pred) {
                n
            } else {
                match list_link(g, &pred) {
                    None => n,
                    Some((item, opt_pred2)) => {
                        items.push(item);
                        build_list(g, pred, opt_pred2, items, blacklist)
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
            if list_link(&gd.graph, &bnode).is_none() {
                self.write_root(gd, &bnode)?;
            }
        }
        self.write_bytes(b"\n")?;
        Ok(())
    }

    fn write_root(&mut self, gd: &GraphData, root: &RcTerm) -> io::Result<()> {
        self.write_newline()?;
        self.write_term(gd, root)?;
        self.write_bytes(b" ")?;
        self.write_properties(gd, root, gd.lists.get(root).is_some())?;
        self.write_bytes(b".\n")
    }

    fn write_term(&mut self, gd: &GraphData, node: &RcTerm) -> io::Result<()> {
        use TermKind::*;
        match node.kind() {
            Iri => self.write_iri(node),
            BlankNode => self.write_bnode(gd, node),
            Literal => self.write_literal(node),
            Variable => {
                write!(self.write, "?{}", node.value_raw().0)
            }
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
                self.write_term(gd, predicate.as_ref().unwrap())?;
                self.write_bytes(b" ")?;
                self.indent(); // to object-level
            } else {
                self.write_bytes(b",")?;
                self.write_newline()?;
            }
            self.write_term(gd, t.o())?;
        }
        if predicate.is_some() {
            self.unindent(); // back to predicate-level
        }
        self.unindent(); // back to original level
        Ok(())
    }

    fn write_objects(&mut self, gd: &GraphData, objects: &[RcTerm]) -> io::Result<()> {
        self.write_term(gd, &objects[0])?;
        for obj in &objects[1..] {
            self.write_bytes(b",")?;
            self.write_newline()?;
            self.write_term(gd, &obj)?;
        }
        Ok(())
    }

    fn write_iri<T: TTerm>(&mut self, iri: &T) -> io::Result<()> {
        if &rdf::nil == iri {
            return self.write_bytes(b"()");
        }
        match self
            .config
            .prefix_map
            .get_checked_prefixed_pair(iri, |txt| PN_LOCAL.is_match(txt))
        {
            Some((pre, suf)) => {
                write!(self.write, "{}:{}", pre.as_ref(), suf)
            }
            None => {
                let raw = iri.value_raw();
                write!(self.write, "<{}{}>", raw.0, raw.1.unwrap_or(""))
            }
        }
    }

    fn write_bnode(&mut self, gd: &GraphData, bn: &RcTerm) -> io::Result<()> {
        if let Some(items) = gd.lists.get(bn) {
            self.write_bytes(b"(")?;
            self.indent();
            for item in items {
                self.write_newline()?;
                self.write_term(gd, item)?;
            }
            self.unindent();
            self.write_newline()?;
            self.write_bytes(b")")
        } else if self.anons.remove(bn) {
            self.write_bytes(b"[ ")?;
            self.write_properties(gd, bn, false)?;
            self.write_bytes(b" ]")
        } else if is_anon_root(&gd.graph, bn) {
            self.write_bytes(b"[]")
        } else {
            write!(self.write, "_:{}", bn.value_raw().0)
        }
    }

    fn write_literal(&mut self, lit: &RcTerm) -> io::Result<()> {
        let datatype = lit.datatype().unwrap();
        let value = lit.value_raw().0;
        if datatype == xsd::integer && INTEGER.is_match(value)
            || datatype == xsd::decimal && DECIMAL.is_match(value)
            || datatype == xsd::double && DOUBLE.is_match(value)
            || datatype == xsd::boolean && BOOLEAN.is_match(value)
        {
            self.write_bytes(value.as_bytes())
        } else {
            self.write_bytes(b"\"")?;
            crate::serializer::nt::quoted_string(&mut self.write, value.as_bytes())?;
            self.write_bytes(b"\"")?;
            if let Some(tag) = lit.language() {
                write!(self.write, "@{}", tag)
            } else {
                if datatype != xsd::string {
                    self.write_bytes(b"^^")?;
                    self.write_iri(&datatype)?;
                }
                Ok(())
            }
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

#[cfg(test)]
mod test {
    use super::super::*;
    use super::*;
    use crate::graph::inmem::FastGraph;
    use sophia_api::triple::stream::TripleSource;
    use std::error::Error;

    #[test]
    fn pn_local() {
        for positive in ["a", "aBc", "éàï", ":::", "123", "%20%21%22", "\\%\\?\\&"] {
            assert!(PN_LOCAL.is_match(positive), "{}", positive);
        }
        for negative in [" ", ".a", "a."] {
            assert!(!PN_LOCAL.is_match(negative), "{}", negative);
        }
    }

    #[test]
    fn double() {
        for positive in [
            "3.14e0",
            "+3.14e0",
            "-3.14e0",
            "3.14e+0",
            "3.14e-0",
            "0000e0000",
            ".1E0",
            "1.e+3",
            "1E-3",
        ] {
            assert!(DOUBLE.is_match(positive), "{}", positive);
        }
    }

    #[test]
    fn roundtrip() -> Result<(), Box<dyn Error>> {
        for ttl in [
            "#empty ttl",
            r#"# simple triple
                PREFIX : <http://example.org/ns/>
                :alice a :Person; :name "Alice"; :age 42.
                :bob a :Person, :Man; :nick "bob"@fr, "bobby"@en; :admin true.
            "#,
            r#"# lists
                <tag:alice> <tag:likes> ( 1 2 ( 3 4 ) 5 6 ), ("a" "b").
            "#,
            r#"# subject lists
                (1 2 3) a <tag:List>.
            "#,
            r#"# malformed list
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                _:a rdf:first 42, 43; rdf:rest (44 45).
                _:b rdf:first 42; rdf:rest (43), (44).
            "#,
            /* currently fails, probably due to a bug in isomorphism algo
            r#"# bnode cycles
            PREFIX : <http://example.org/ns/>
            _:a :n "a"; :p [ :q [ :r _:a ]].
            _:b :n "b"; :s [ :s _:b ].
            "#,
            */
        ] {
            println!("==========\n{}\n----------", ttl);
            let g1: FastGraph = crate::parser::turtle::parse_str(ttl).collect_triples()?;

            let mut out = Vec::<u8>::new();
            let config = TurtleConfig::new().with_pretty(true);
            let mut ser = TurtleSerializer::new_with_config(&mut out, config);
            ser.serialize_triples(g1.triples())?;
            let out = String::from_utf8(out)?;
            println!("{}", &out);

            let g2: FastGraph = crate::parser::turtle::parse_str(&out).collect_triples()?;

            assert!(sophia_api::graph::isomorphic_graphs(&g1, &g2)?);
        }
        Ok(())
    }
}
