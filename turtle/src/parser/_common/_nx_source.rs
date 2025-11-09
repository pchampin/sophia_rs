use sophia_api::{source::StreamResult, term::IriRef};
use sophia_iri::Iri;

use crate::_term::IndexedTerm;
use crate::parser::_error::AdjustCol;
use crate::parser::{_common::GenericSource, _error::ResultExt, Error, ErrorKind};

/// Common trait of `TripleSource`/`QuadSource` for N-type formats (N-Triples/N-Quads)
pub(crate) trait NxSource: GenericSource {
    fn parse_line<C, E>(&mut self, line: &str, mut callback: C) -> StreamResult<(), Error, E>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static,
    {
        debug_assert_eq!(self.inner().col, 0);
        self.inner_mut().col += self.ws(line);
        if self.eol(&line[self.inner().col..]) {
            return Ok(());
        }

        self.inner_mut().col += self.parse_tokens(&line[self.inner().col..], &mut callback)?;
        if cfg!(test) {
            self.debug_dump("after parse_tokens", &line[self.inner().col..]);
        }

        self.inner_mut().col += self.ws(&line[self.inner().col..]);
        if !self.eol(&line[self.inner().col..]) {
            return Err(ErrorKind::Expected("end of line".into())).wrap_in(self);
        }
        Ok(())
    }

    /// Format-specific parsing of a line,
    /// assuming that leading white spaces have been trimmed.
    fn parse_tokens<C, E>(&mut self, txt: &str, callback: &mut C) -> StreamResult<usize, Error, E>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E>,
        E: std::error::Error + Send + Sync + 'static;

    // non-terminal production

    fn enter_subject_or_version<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if cfg!(test) {
            self.debug_dump("enter_subject_or_version", txt)
        }

        match txt.as_bytes()[0] {
            b'<' => self.iriref(txt),
            b'_' => self.blank_node_label(txt),
            b'V' if txt[1..].starts_with("ERSION") => Ok("VERSION".len()),
            _ => Err(ErrorKind::Expected("'VERSION' or subject".into())).wrap_in(self),
        }
    }

    fn enter_predicate<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if cfg!(test) {
            self.debug_dump("enter_predicate", txt)
        }

        if txt.starts_with('<') {
            self.iriref(txt)
        } else {
            Err(ErrorKind::Expected("IRIREF as predicate".into())).wrap_in(self)
        }
    }

    fn enter_object<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if cfg!(test) {
            self.debug_dump("enter_object", txt)
        }

        match txt.as_bytes()[0] {
            b'<' => {
                if txt[1..].starts_with("<(") {
                    self.enter_triple_term(txt)
                } else {
                    self.iriref(txt)
                }
            }
            b'_' => self.blank_node_label(txt),
            b'"' => {
                let mut col = 1;
                col += self.in_string_literal_short(&txt[1..], b'"')?;
                col += self.ws(&txt[col..]);
                match txt.as_bytes()[col] {
                    b'^' if txt[col + 1..].starts_with('^') => {
                        col += 2 + self.ws(&txt[col + 2..]);
                        if txt[col..].starts_with('<') {
                            col += self.iriref(&txt[col..]).adjust_col(col)?;
                            let Some(IndexedTerm::Iri(idx)) = self.inner_mut().terms.pop() else {
                                unreachable!(); // both self.iriref and self.prefixed_name push a IndexedTerm::Iri
                            };
                            self.check_invalid_literal(idx).adjust_col(col)?;
                            self.inner_mut()
                                .terms
                                .push(IndexedTerm::TypedLiteral(idx - 1, idx));
                            Ok(col)
                        } else {
                            Err(ErrorKind::Expected("IRIREF".into())).wrap_in_at(self, col)
                        }
                    }
                    b'@' => self
                        .lang_dir(&txt[col..])
                        .map(|offset| col + offset)
                        .adjust_col(col),
                    _ => {
                        let idx = self.inner().buffers.len() - 1;
                        let dt = IriRef::new_unchecked("#string");
                        self.inner_mut()
                            .terms
                            .push(IndexedTerm::XsdLiteral(idx, dt));
                        Ok(col)
                    }
                }
            }
            _ => Err(ErrorKind::Expected("object".into())).wrap_in(self),
        }
    }

    fn enter_triple_term<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if cfg!(test) {
            self.debug_dump("enter_triple_term", txt)
        }

        let mut col = 3;
        col += self.ws(&txt[col..]);
        col += self.enter_subject(&txt[col..]).adjust_col(col)?;
        col += self.ws(&txt[col..]);
        col += self.enter_predicate(&txt[col..]).adjust_col(col)?;
        col += self.ws(&txt[col..]);
        col += self.enter_object(&txt[col..]).adjust_col(col)?;
        col += self.ws(&txt[col..]);
        if txt[col..].starts_with(")>>") {
            self.inner_mut().terms.push(IndexedTerm::TripleTerm);
            Ok(col + 3)
        } else {
            Err(ErrorKind::Expected(")>>".into())).wrap_in_at(self, col)
        }
    }

    fn enter_subject<E>(&mut self, txt: &str) -> StreamResult<usize, Error, E>
    where
        E: std::error::Error + Send + Sync + 'static,
    {
        if cfg!(test) {
            self.debug_dump("enter_subject", txt)
        }

        match txt.as_bytes()[0] {
            b'<' => self.iriref(txt),
            b'_' => self.blank_node_label(txt),
            _ => Err(ErrorKind::Expected("subject".into())).wrap_in(self),
        }
    }

    fn debug_dump(&self, context: &str, txt: &str) {
        println!(
            "  {context}:\n  “˄{txt}” ({},{})\n    {:?}\n    {:?}",
            self.inner().line,
            self.inner().col,
            self.inner().terms,
            self.inner().buffers
        );
    }

    // terminal production

    /// Handle the whole production https://www.w3.org/TR/rdf12-n-triples/#grammar-production-IRIREF
    /// (or the equivalent in N-Quads)
    /// assuming the leading '<'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting with '<'.
    fn iriref<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        let buffer_idx = self.inner().buffers.len();
        let ret = self.iriref_raw(txt)?;
        if let Err(err) = Iri::new(self.inner_mut().buffers.top()) {
            // .wrap_in_at can not be directly applied to the result of Iri::new
            // because .self is mutably borrowed via buf
            return Err(err).wrap_in_at(self, 1);
        }
        self.inner_mut().terms.push(IndexedTerm::Iri(buffer_idx));
        Ok(ret)
    }

    /// Handle the whole production https://www.w3.org/TR/rdf12-n-triples/#grammar-production-BLANK_NODE_LABEL
    /// (or the equivalent in N-Quads)
    /// assuming the leading '_'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting with '_'.
    fn blank_node_label<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        self.blank_node_label_raw(txt)
    }

    /// Handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-LANG_DIR
    /// assuming the leading '@'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting with '@'.
    fn lang_dir<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        self.lang_dir_raw(txt)
    }

    /// Consume horizontal whitespaces
    ///
    /// `txt` is (the remaining of) a line of text as read by [`BufRead::read_line`].
    fn ws(&mut self, txt: &str) -> usize {
        txt.bytes()
            .take_while(|b| *b == b' ' || *b == b'\t')
            .count()
    }

    /// Indicate whether the end-of-line or a comment has been reached
    ///
    /// `txt` is (the remaining of) a line of text as read by [`BufRead::read_line`].
    fn eol(&mut self, txt: &str) -> bool {
        txt.is_empty() || txt.starts_with(['\n', '\r', '#'])
    }
}
