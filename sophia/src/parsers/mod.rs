use std::io;

use pest;

pub mod common;
pub mod nt;

#[derive(Debug)]
pub enum Error {
    IO{
        err: io::Error,
        lineno: usize,
        already_added: usize,
    },
    Parsing{
        message: String,
        lineno: usize,
        start: usize,
        end: Option<usize>,
        already_added: usize,
    }
}

impl Error {
    pub(crate) fn from_io(err: io::Error, lineno: usize, already_added: usize) -> Self {
        Error::IO { err, lineno, already_added }
    }

    pub(crate) fn from_pest<R: pest::RuleType> (err: pest::Error<R>, lineno: usize, already_added: usize) -> Self {
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
        Error::Parsing{ message: msg, lineno, start, end, already_added }
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        use self::Error::*;
        match (self, other) {
            (IO{err: e1, lineno: l1, already_added: a1}, IO{err: e2, lineno: l2, already_added: a2}) => {
                l1 == l2 && a1 == a2 && format!("{:?}", e1) == format!("{:?}", e2)
            }
            (Parsing{message: m1, lineno: l1, start: s1, end: e1, already_added: a1}, Parsing{message: m2, lineno: l2, start: s2, end: e2, already_added: a2}) => {
                l1 == l2 && s1 == s2 && e1 == e2 && a1 == a2 && m1 == m2
            }
            _ => false
        }
    }
}
