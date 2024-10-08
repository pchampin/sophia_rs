//! This benchmark is used to compare the time it takes to create
//! * borrowing `MownStr`'s vs. standard &str references
//! * owning `MownStr`'s vs. Strings
//!
//! The results of `borrowed_mownstr` should therefore be compared to `refs`,
//! and that of `owned_mownstr` should be compared to `strings`.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use sophia_iri::resolve::{BaseIri, BaseIriRef};

fn parse(c: &mut Criterion) {
    c.bench_with_input(
        BenchmarkId::new("parse", ""),
        black_box(&(POSITIVE_IRIS, NEGATIVE_IRIS, RELATIVE_IRIS)),
        |b, &i| {
            b.iter(|| {
                for _ in 0..42 {
                    for iri in i.0 {
                        black_box(BaseIriRef::new(*iri).is_ok());
                    }
                    for iri in i.1 {
                        black_box(BaseIriRef::new(*iri).is_err());
                    }
                    for (iri, _) in i.2 {
                        black_box(BaseIriRef::new(*iri).is_ok());
                    }
                }
            });
        },
    );
}

fn resolve_from_scratch(c: &mut Criterion) {
    c.bench_with_input(
        BenchmarkId::new("resolve from scratch", ""),
        black_box(&RELATIVE_IRIS),
        |b, &i| {
            b.iter(|| {
                for _ in 0..42 {
                    for (rel, _) in i {
                        let base = BaseIriRef::new("http://a/b/c/d;p?q").unwrap();
                        black_box(&base.resolve(*rel).unwrap());
                    }
                }
            });
        },
    );
}

fn resolve_mutualized(c: &mut Criterion) {
    c.bench_with_input(
        BenchmarkId::new("resolve mutualized", ""),
        black_box(&(BaseIri::new("http://a/b/c/d;p?q").unwrap(), RELATIVE_IRIS)),
        |b, i| {
            b.iter(|| {
                for _ in 0..42 {
                    for (rel, _) in i.1 {
                        black_box(i.0.resolve(*rel).unwrap());
                    }
                }
            });
        },
    );
}

criterion_group!(benches, parse, resolve_from_scratch, resolve_mutualized);
criterion_main!(benches);

/// An array of invalid IRI references.
pub const POSITIVE_IRIS: &[&str] = &[
    "http:",
    "http://example.org",
    "http://127.0.0.1",
    "http://[::]",
    "http://%0D",
    "http://example.org/",
    "http://éxample.org/",
    "http://user:pw@example.org:1234/",
    "http://example.org/foo/bar/baz",
    "http://example.org/foo/bar/",
    "http://example.org/foo/bar/bàz",
    "http://example.org/foo/.././/bar",
    "http://example.org/!$&'()*+,=:@/foo%0D",
    "http://example.org/?abc",
    "http://example.org/?!$&'()*+,=:@/?\u{E000}",
    "http://example.org/#def",
    "http://example.org/?abc#def",
    "tag:abc/def",
    "tag:",
    "foo",
    "..",
    "//example.org",
    "?",
    "#",
    "?#",
    "http://example.org/#Andr%C3%A9",
    "http://example.org/?Andr%C3%A9",
    "?Andr%C3%A9#Andr%C3%A9",
];

/// An array of invalid IRI references.
pub const NEGATIVE_IRIS: &[&str] = &[
    "http://[/",
    "http://a/[",
    "http://a/]",
    "http://a/|",
    "http://a/ ",
    "http://a/\u{E000}",
    "[",
    "]",
    "|",
    " ",
    "\u{E000}",
];

/// An array of relative IRI references and their absolute counterpart.
///
/// The base IRI used for resolution is `http://a/b/c/d;p?q`.
/// Most examples are drawn from
/// <https://tools.ietf.org/html/rfc3986#section-5.4.1>
/// and
/// <https://tools.ietf.org/html/rfc3986#section-5.4.2>.
pub const RELATIVE_IRIS: &[(&str, &str)] = &[
    // normal examples from https://tools.ietf.org/html/rfc3986#section-5.4.1
    ("g:h", "g:h"),
    ("g", "http://a/b/c/g"),
    ("./g", "http://a/b/c/g"),
    ("g/", "http://a/b/c/g/"),
    ("/g", "http://a/g"),
    ("//g", "http://g"),
    ("?y", "http://a/b/c/d;p?y"),
    ("g?y", "http://a/b/c/g?y"),
    ("#s", "http://a/b/c/d;p?q#s"),
    ("g#s", "http://a/b/c/g#s"),
    ("g?y#s", "http://a/b/c/g?y#s"),
    (";x", "http://a/b/c/;x"),
    ("g;x", "http://a/b/c/g;x"),
    ("g;x?y#s", "http://a/b/c/g;x?y#s"),
    ("", "http://a/b/c/d;p?q"),
    (".", "http://a/b/c/"),
    ("./", "http://a/b/c/"),
    ("..", "http://a/b/"),
    ("../", "http://a/b/"),
    ("../g", "http://a/b/g"),
    ("../..", "http://a/"),
    ("../../", "http://a/"),
    ("../../g", "http://a/g"),
    // abnormal example from https://tools.ietf.org/html/rfc3986#section-5.4.2
    ("../../../g", "http://a/g"),
    ("../../../../g", "http://a/g"),
    ("/./g", "http://a/g"),
    ("/../g", "http://a/g"),
    ("g.", "http://a/b/c/g."),
    (".g", "http://a/b/c/.g"),
    ("g..", "http://a/b/c/g.."),
    ("..g", "http://a/b/c/..g"),
    ("./../g", "http://a/b/g"),
    ("./g/.", "http://a/b/c/g/"),
    ("g/./h", "http://a/b/c/g/h"),
    ("g/../h", "http://a/b/c/h"),
    ("g;x=1/./y", "http://a/b/c/g;x=1/y"),
    ("g;x=1/../y", "http://a/b/c/y"),
    ("g?y/./x", "http://a/b/c/g?y/./x"),
    ("g?y/../x", "http://a/b/c/g?y/../x"),
    ("g#s/./x", "http://a/b/c/g#s/./x"),
    ("g#s/../x", "http://a/b/c/g#s/../x"),
    // other examples (maybe one day)
];
