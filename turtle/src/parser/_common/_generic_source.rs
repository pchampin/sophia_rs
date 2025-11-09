use sophia_api::{source::StreamResult, term::BaseDirection};

use crate::_term::IndexedTerm;
use crate::lazy_regex;
use crate::parser::_common::{Inner, unescape_iri, unescape_literal};
use crate::parser::_error::ResultExt;
use crate::parser::{Error, ErrorKind};

/// Common super-trait of `TripleSource`/`QuadSource` for all formats.
pub(crate) trait GenericSource {
    /// The type of tuple this source outputs
    type Output<'x>;

    /// Get the common inner attributes of the source
    fn inner(&self) -> &Inner;

    /// Get mutably the common inner attributes of the source
    fn inner_mut(&mut self) -> &mut Inner;

    /// Feed `callback` with the triple/quad last parsed
    ///
    /// Precondition: `source` is in an appropriate state
    fn emit_tuple<C, E, E2>(&self, callback: &mut C, offset: usize) -> StreamResult<usize, E, E2>
    where
        C: FnMut(Self::Output<'_>) -> Result<(), E2>,
        E: std::error::Error,
        E2: std::error::Error;

    // terminal productions

    /// Partially handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-IRIREF
    /// (or the equivalent in N-Triples, N-Quads, TriG)
    /// assuming the leading '<'.
    ///
    /// This parses the text until the closing '>', and allocates it in the buffer stash.
    /// Note however that this method DOES NOT push anything in the term stack,
    /// because how it is done exactly depends on the parser
    /// (Turtle and TriG may resolve it against the base IRI,
    /// while N-Triples and N-Quads always expect an absolute IRI).
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting with '<'.
    fn iriref_raw<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(txt.starts_with('<'));

        let buf = self.inner_mut().buffers.push();
        let txtb = txt.as_bytes();
        let mut i = 1;
        loop {
            match txt[i..].find(['\n', '\r', '\\', '>']).map(|x| x + i) {
                Some(j) if txtb[j] == b'\\' => {
                    buf.push_str(&txt[i..j]);
                    if let Err(err) = unescape_iri(&txt[j + 1..], &mut i, buf, j + 1) {
                        return Err(err).wrap_in_at(self, j);
                    }
                }
                Some(j) if txtb[j] == b'>' => {
                    buf.push_str(&txt[i..j]);
                    i = j;
                    break;
                }
                opt => {
                    let j = opt.unwrap_or(txt.len());
                    debug_assert!(j >= txtb.len() || txtb[j] == b'\n' || txtb[j] == b'\r');
                    return Err(ErrorKind::Expected("matching closing '>'".into())).wrap_in(self);
                }
            }
        }
        debug_assert!(txtb[i] == b'>');
        Ok(i + 1)
    }

    /// Partially handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-BLANK_NODE_LABEL
    /// (or the equivalent in N-Triples, N-Quads, TriG)
    /// assuming the leading '_'.
    ///
    /// Syntaxes that mint bnode labels (Turtle, TriG)
    /// must additionally avoid conflict with their own generated labels.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting with '_'.
    fn blank_node_label_raw<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(txt.starts_with('_'));

        lazy_regex!(
            LABEL = r#"(?x) ^
            # (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
            [
             A-Z a-z \xC0-\xD6 \xD8-\xF6 \xF8-\u{02FF} \u{0370}-\u{037D}
             \u{037F}-\u{1FFF} \u{200C}-\u{200D} \u{2070}-\u{218F} \u{2C00}-\u{2FEF}
             \u{3001}-\u{D7FF} \u{F900}-\u{FDCF} \u{FDF0}-\u{FFFD} \u{10000}-\u{EFFFF}
             _
             0-9
            ]
            (?:
                [
                 A-Z a-z \xC0-\xD6 \xD8-\xF6 \xF8-\u{02FF} \u{0370}-\u{037D}
                 \u{037F}-\u{1FFF} \u{200C}-\u{200D} \u{2070}-\u{218F} \u{2C00}-\u{2FEF}
                 \u{3001}-\u{D7FF} \u{F900}-\u{FDCF} \u{FDF0}-\u{FFFD} \u{10000}-\u{EFFFF}
                 _
                 \- 0-9 \xB7 \u{0300}-\u{036F} \u{203F}-\u{2040}
                 .
                ]*
                [
                 A-Z a-z \xC0-\xD6 \xD8-\xF6 \xF8-\u{02FF} \u{0370}-\u{037D}
                 \u{037F}-\u{1FFF} \u{200C}-\u{200D} \u{2070}-\u{218F} \u{2C00}-\u{2FEF}
                 \u{3001}-\u{D7FF} \u{F900}-\u{FDCF} \u{FDF0}-\u{FFFD} \u{10000}-\u{EFFFF}
                 _
                 \- 0-9 \xB7 \u{0300}-\u{036F} \u{203F}-\u{2040}
                ]
            )?
        "#
        );

        let buffer_idx = self.inner().buffers.len();
        if !txt[1..].starts_with(':') {
            Err(ErrorKind::Expected("':'".into())).wrap_in_at(self, 1)
        } else if let Some(cap) = LABEL.find(&txt[2..]) {
            self.inner_mut().buffers.push().push_str(cap.as_str());
            self.inner_mut()
                .terms
                .push(IndexedTerm::BlankNode(buffer_idx));
            Ok(2 + cap.len())
        } else {
            Err(ErrorKind::Bnode).wrap_in_at(self, 2)
        }
    }

    /// Expect production https://www.w3.org/TR/rdf12-turtle/#grammar-production-STRING_LITERAL_QUOTE
    /// or production https://www.w3.org/TR/rdf12-turtle/#grammar-production-STRING_SINGLE_LITERAL_QUOTE
    /// (or the equivalent in N-Triples, N-Quads or TriG)
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting at self.col.
    fn in_string_literal_short<E2>(
        &mut self,
        txt: &str,
        quote: u8,
    ) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(!txt.is_empty());

        let buf = self.inner_mut().buffers.push();
        let txtb = txt.as_bytes();
        let mut i = 0;
        loop {
            match txt[i..]
                .find(['\n', '\r', '\\', quote as char])
                .map(|x| x + i)
            {
                Some(j) if txtb[j] == b'\\' => {
                    buf.push_str(&txt[i..j]);
                    if let Err(err) = unescape_literal(&txt[j + 1..], &mut i, buf, j + 1) {
                        // .wrap_in_at can not be directly applied to the result of unescape_literal
                        // because .self is mutably borrowed via buf
                        return Err(err).wrap_in_at(self, j + 1);
                    }
                }
                Some(j) if txtb[j] == quote => {
                    buf.push_str(&txt[i..j]);
                    return Ok(j + 1);
                }
                opt => {
                    let j = opt.unwrap_or(txt.len());
                    debug_assert!(j >= txtb.len() || txtb[j] == b'\n' || txtb[j] == b'\r');
                    return Err(ErrorKind::Expected(format!(
                        "closing quote {:?}",
                        quote as char
                    )))
                    .wrap_in_at(self, j + 1);
                }
            }
        }
    }

    /// Partially handle the whole production https://www.w3.org/TR/rdf12-turtle/#grammar-production-LANG_DIR
    /// assuming the leading '@'.
    ///
    /// `txt` is the remaining of a line of text as read by [`BufRead::read_line`],
    /// starting with '@'.
    fn lang_dir_raw<E2>(&mut self, txt: &str) -> StreamResult<usize, Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        debug_assert!(txt.starts_with('@'));

        lazy_regex!(LANG = r"(?x) ^ [a-zA-Z]+ (?: - [a-zA-Z0-9]+ )*");
        lazy_regex!(DIR = r"(?x) ^ -- [a-zA-Z]+");
        match LANG.find(&txt[1..]) {
            None => Err(ErrorKind::Expected("language tag".into())).wrap_in_at(self, 1),
            Some(m1) => {
                let idx = self.inner().buffers.len();
                sophia_api::term::LanguageTag::new(m1.as_str())
                    .map_err(|_| ErrorKind::Expected("valid language tag".into()))
                    .wrap_in_at(self, 1)?;
                self.inner_mut().buffers.push().push_str(m1.as_str());
                let base_dir = DIR
                    .find(&txt[1 + m1.end()..])
                    .map(|m2| match &m2.as_str()[2..] {
                        "ltr" => Ok(BaseDirection::Ltr) as StreamResult<BaseDirection, Error, E2>,
                        "rtl" => Ok(BaseDirection::Rtl),
                        _ => Err(ErrorKind::Expected("'ltr' or 'rtl'".into()))
                            .wrap_in_at(self, 1 + m1.end() + m2.start() + 2)?,
                    })
                    .transpose()?;
                let dir_len = if base_dir.is_some() { 5 } else { 0 };
                self.inner_mut()
                    .terms
                    .push(IndexedTerm::LangString(idx - 1, idx, base_dir));
                Ok(1 + m1.len() + dir_len)
            }
        }
    }

    // other methods

    fn check_invalid_literal<E2>(&self, idx: usize) -> StreamResult<(), Error, E2>
    where
        E2: std::error::Error + Send + Sync + 'static,
    {
        lazy_regex!(
            LANG_STRING = r"^http://www.w3.org/1999/02/22-rdf-syntax-ns#(dirL|l)angString$"
        );
        if LANG_STRING.find(&self.inner().buffers[idx]).is_some() {
            Err(ErrorKind::InvalidLiteral).wrap_in(self)
        } else {
            Ok(())
        }
    }

    fn pop_term(&mut self) {
        debug_assert!(!self.inner().terms.is_empty());
        let inner = self.inner_mut();
        IndexedTerm::pop(&mut inner.terms, &mut inner.buffers);
    }
}
