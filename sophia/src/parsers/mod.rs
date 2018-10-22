//! This module defines parsers for standard RDF syntaxes,
//! as well as tools for building new parsers.
//! 
//! Each parser module defines a `Config` type, that must
//! - implement `Default`,
//! - have three methods `parse_bufread`, `parse_read` and `parse_str`,
//!   accepting `io::BufRead`, `io::Read` and `&str` respectively,
//!   and all returning a `TripleSource`(TODO link).
//! 
//! The module must also three functions
//! `parse_bufread`, `parse_read` and `parse_str`,
//! calling the corresponding methods from the default `Config`.

use std::io;
use pest;

#[macro_use]
pub mod common;
pub mod nt;

#[derive(Debug)]
pub enum Error {
    IO{
        err: io::Error,
        lineno: usize,
    },
    Parsing{
        message: String,
        lineno: usize,
        start: usize,
        end: Option<usize>,
    }
}

impl Error {
    pub(crate) fn from_io(err: io::Error, lineno: usize) -> Self {
        Error::IO { err, lineno }
    }

    pub(crate) fn from_pest<R: pest::RuleType> (err: pest::Error<R>, lineno: usize) -> Self {
        let msg;
        let start;
        let end;
        use pest::Error::*;
        match err {
            ParsingError{positives, negatives, pos} => {
                msg = format!("expected: {:?}\nunexpected: {:?}", positives, negatives);
                start = pos.pos();
                end = None;
            }
            CustomErrorPos{message, pos} => {
                msg = message;
                start = pos.pos();
                end = None;
            }
            CustomErrorSpan{message, span} => {
                msg = message;
                start = span.start();
                end = Some(span.end());
            }
        }
        Error::Parsing{ message: msg, lineno, start, end }
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        use self::Error::*;
        match (self, other) {
            (IO{err: e1, lineno: l1}, IO{err: e2, lineno: l2}) => {
                l1 == l2 && format!("{:?}", e1) == format!("{:?}", e2)
            }
            (Parsing{message: m1, lineno: l1, start: s1, end: e1}, Parsing{message: m2, lineno: l2, start: s2, end: e2}) => {
                l1 == l2 && s1 == s2 && e1 == e2 && m1 == m2
            }
            _ => false
        }
    }
}
