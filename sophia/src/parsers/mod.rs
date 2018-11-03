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

#[macro_use]
pub mod common;
pub mod nt;
