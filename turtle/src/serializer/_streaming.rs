use sophia_api::ns::rdf;
use sophia_api::term::{SimpleTerm, Term};
use sophia_api::triple::Triple;
use std::io;

use crate::_stash::StringStash;
use crate::_term::{IndexedTerm, StashedTerm};
use crate::serializer::_common::{write_iri, write_language_string, write_typed_literal};
use crate::serializer::turtle::TurtleConfig;

pub(crate) struct StreamingSerializerState<'a, W: io::Write> {
    write: &'a mut W,
    config: &'a TurtleConfig,
    terms: Vec<IndexedTerm>,
    buffers: StringStash,
    last_seen: [Option<usize>; 3],
}

impl<'a, W: io::Write> StreamingSerializerState<'a, W> {
    pub fn new(write: &'a mut W, config: &'a TurtleConfig) -> Self {
        Self {
            write,
            config,
            terms: vec![],
            buffers: StringStash::default(),
            last_seen: [None; 3],
        }
    }

    pub fn write_all(&mut self, data: &[u8]) -> io::Result<()> {
        self.write.write_all(data)
    }

    pub fn same_term<T: Term>(&self, idx: usize, t: T) -> bool {
        StashedTerm {
            terms: &self.terms,
            buffers: &self.buffers,
            idx,
        }
        .eq(t)
    }

    pub fn copy_term<T: Term>(&mut self, t: T) -> usize {
        IndexedTerm::copy_term(t, &mut self.terms, &mut self.buffers)
    }

    fn copy_s<T: Term>(&mut self, t: T) {
        debug_assert!(matches!(self.last_seen, [None, None, None]));
        self.last_seen[0] = Some(self.copy_term(t));
    }

    fn copy_p<T: Term>(&mut self, t: T) {
        debug_assert!(matches!(self.last_seen, [Some(_), None, None]));
        self.last_seen[1] = Some(self.copy_term(t));
    }

    fn copy_o<T: Term>(&mut self, t: T) {
        debug_assert!(matches!(self.last_seen, [Some(_), Some(_), None]));
        self.last_seen[2] = Some(self.copy_term(t));
    }

    fn copy_triple<T: Triple>(&mut self, t: T) {
        debug_assert!(matches!(self.last_seen, [None, None, None]));
        self.last_seen[0] = Some(self.copy_term(t.s()));
        self.last_seen[1] = Some(self.copy_term(t.p()));
        self.last_seen[2] = Some(self.copy_term(t.o()));
    }

    pub fn pop_all(&mut self) {
        self.terms.truncate(0);
        self.buffers.empty();
        self.last_seen = [None; 3]
    }

    pub fn has_s(&mut self) -> bool {
        self.last_seen[0].is_some()
    }

    #[expect(dead_code)]
    fn pop_s(&mut self) {
        debug_assert!(matches!(self.last_seen, [Some(_), None, None]));
        IndexedTerm::pop(&mut self.terms, &mut self.buffers);
        self.last_seen[0] = None;
    }

    fn pop_p(&mut self) {
        debug_assert!(matches!(self.last_seen, [Some(_), Some(_), None]));
        IndexedTerm::pop(&mut self.terms, &mut self.buffers);
        self.last_seen[1] = None;
    }

    fn pop_o(&mut self) {
        debug_assert!(matches!(self.last_seen, [Some(_), Some(_), Some(_)]));
        IndexedTerm::pop(&mut self.terms, &mut self.buffers);
        self.last_seen[2] = None;
    }

    pub fn write_asserted_triple<T: Triple>(&mut self, t: T) -> std::io::Result<()> {
        match self.last_seen {
            [None, None, None] => {
                self.write_whole_asserted_triple(t)?;
            }
            [Some(is), Some(ip), Some(_)] => {
                if self.same_term(is, t.s()) {
                    self.pop_o();
                    if self.same_term(ip, t.p()) {
                        self.write_all(b",\n\t\t")?;
                    } else {
                        self.pop_p();
                        self.write_all(b";\n\t")?;
                        self.write_predicate(t.p())?;
                        self.write_all(b" ")?;
                        self.copy_p(t.p());
                    }
                    self.write_node(t.o())?;
                    self.copy_o(t.o());
                } else {
                    self.pop_all();
                    self.write_all(b".\n")?;
                    self.write_whole_asserted_triple(t)?;
                }
            }
            [Some(is), None, None] => {
                if self.same_term(is, t.s()) {
                    self.write_predicate(t.p())?;
                    self.write_all(b" ")?;
                    self.write_node(t.o())?;
                    self.copy_p(t.p());
                    self.copy_o(t.o());
                } else {
                    self.pop_all();
                    self.write_all(b".\n")?;
                    self.write_whole_asserted_triple(t)?;
                }
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn write_whole_asserted_triple<T: Triple>(&mut self, t: T) -> std::io::Result<()> {
        debug_assert!(matches!(self.last_seen, [None, None, None]));
        if let Some(tt) = t.o().triple()
            && rdf::reifies == t.p()
        {
            self.write_all(b"<< ")?;
            self.write_triple(tt)?;
            self.write_all(b" ~ ")?;
            self.write_node(t.s())?;
            self.write_all(b" >> ")?;
            self.copy_s(t.s());
        } else {
            self.write_triple(&t)?;
            self.copy_triple(t);
        }
        Ok(())
    }

    fn write_triple<T: Triple>(&mut self, t: T) -> std::io::Result<()> {
        self.write_node(t.s())?;
        self.write_all(b" ")?;
        self.write_predicate(t.p())?;
        self.write_all(b" ")?;
        self.write_node(t.o())?;
        Ok(())
    }

    pub fn write_node<T: Term>(&mut self, t: T) -> std::io::Result<()> {
        use SimpleTerm::*;
        match t.as_simple() {
            Iri(iri_ref) => write_iri(&iri_ref, self.write, &self.config.prefix_map),
            BlankNode(bnode_id) => write!(self.write, "_:{}", bnode_id.as_str()),
            LiteralDatatype(lex, dt) => {
                write_typed_literal(&lex, &dt, self.write, &self.config.prefix_map)
            }
            LiteralLanguage(lex, tag, dir) => write_language_string(&lex, &tag, dir, self.write),
            Triple(t) => {
                self.write_all(b"<<( ")?;
                self.write_node(t.s())?;
                self.write_all(b" ")?;
                self.write_predicate(t.p())?;
                self.write_all(b" ")?;
                self.write_node(t.o())?;
                self.write_all(b" )>>")
            }
            Variable(var_name) => write!(self.write, "?{}", var_name.as_str()),
        }
    }

    fn write_predicate<T: Term>(&mut self, t: T) -> std::io::Result<()> {
        if rdf::type_ == t {
            self.write_all(b"a")
        } else {
            self.write_node(t)
        }
    }
}
