//! Test module for `sophia_iri`.
//!
//! If the feature `test_data` is enabled,
//! this module publicly exports arrays of test data,
//! for the benefit of other crates.

/// Structure of an IRI referece.
/// See [`POSITIVIE_IRIS`](constant.POSITIVE_IRIS.html) for more details.
pub type IriRefStructure<'a> = (
    bool,
    Option<&'a str>,
    Option<&'a str>,
    &'a [&'a str],
    Option<&'a str>,
    Option<&'a str>,
);

/// An array of valid IRI references.
///
/// Each element is a pair containing
/// * the IRI references, and
/// * a tuple made of
///   - a `bool` indicating whether it is absolute or not,
///   - an `Option<&str>` containing this IRI's scheme (if any),
///   - an `Option<&str>` containing this IRI's authority (if any),
///   - a `&[&str]` containing this IRI's path elements,
///   - an `Option<&str>` containing this IRI's query string (if any),
///   - an `Option<&str>` containing this IRI's fragment identifier (if any).
pub const POSITIVE_IRIS: &[(&str, IriRefStructure)] = &[
    ("http:", (true, Some("http"), None, &[], None, None)),
    (
        "http://example.org",
        (true, Some("http"), Some("example.org"), &[], None, None),
    ),
    (
        "http://127.0.0.1",
        (true, Some("http"), Some("127.0.0.1"), &[], None, None),
    ),
    (
        "http://[::]",
        (true, Some("http"), Some("[::]"), &[], None, None),
    ),
    (
        "http://%0D",
        (true, Some("http"), Some("%0D"), &[], None, None),
    ),
    (
        "http://example.org/",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", ""],
            None,
            None,
        ),
    ),
    (
        "http://éxample.org/",
        (
            true,
            Some("http"),
            Some("éxample.org"),
            &["", ""],
            None,
            None,
        ),
    ),
    (
        "http://user:pw@example.org:1234/",
        (
            true,
            Some("http"),
            Some("user:pw@example.org:1234"),
            &["", ""],
            None,
            None,
        ),
    ),
    (
        "http://example.org/foo/bar/baz",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", "foo", "bar", "baz"],
            None,
            None,
        ),
    ),
    (
        "http://example.org/foo/bar/",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", "foo", "bar", ""],
            None,
            None,
        ),
    ),
    (
        "http://example.org/foo/bar/bàz",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", "foo", "bar", "bàz"],
            None,
            None,
        ),
    ),
    (
        "http://example.org/foo/.././/bar",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", "foo", "..", ".", "", "bar"],
            None,
            None,
        ),
    ),
    (
        "http://example.org/!$&'()*+,=:@/foo%0D",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", "!$&'()*+,=:@", "foo%0D"],
            None,
            None,
        ),
    ),
    (
        "http://example.org/?abc",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", ""],
            Some("abc"),
            None,
        ),
    ),
    (
        "http://example.org/?!$&'()*+,=:@/?\u{E000}",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", ""],
            Some("!$&'()*+,=:@/?\u{E000}"),
            None,
        ),
    ),
    (
        "http://example.org/#def",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", ""],
            None,
            Some("def"),
        ),
    ),
    (
        "http://example.org/?abc#def",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", ""],
            Some("abc"),
            Some("def"),
        ),
    ),
    (
        "tag:abc/def",
        (true, Some("tag"), None, &["abc", "def"], None, None),
    ),
    ("tag:", (true, Some("tag"), None, &[], None, None)),
    ("foo", (false, None, None, &["foo"], None, None)),
    ("..", (false, None, None, &[".."], None, None)),
    (
        "//example.org",
        (false, None, Some("example.org"), &[], None, None),
    ),
    ("?", (false, None, None, &[], Some(""), None)),
    ("#", (false, None, None, &[], None, Some(""))),
    ("?#", (false, None, None, &[], Some(""), Some(""))),
    (
        "http://example.org/#Andr%C3%A9",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", ""],
            None,
            Some("Andr%C3%A9"),
        ),
    ),
    (
        "http://example.org/?Andr%C3%A9",
        (
            true,
            Some("http"),
            Some("example.org"),
            &["", ""],
            Some("Andr%C3%A9"),
            None,
        ),
    ),
    (
        "?Andr%C3%A9#Andr%C3%A9",
        (
            false,
            None,
            None,
            &[],
            Some("Andr%C3%A9"),
            Some("Andr%C3%A9"),
        ),
    ),
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
