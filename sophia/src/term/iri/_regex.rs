use lazy_static::lazy_static;
use regex::Regex;

/// Check whether `txt` is a valid (absolute or relative) IRI reference.
#[inline]
pub fn is_valid_iri_ref(txt: &str) -> bool {
    IRI_REGEX.is_match(txt) || IRELATIVE_REF_REGEX.is_match(txt)
}

/// Check whether `txt` is an absolute IRI reference.
#[inline]
pub fn is_absolute_iri_ref(txt: &str) -> bool {
    IRI_REGEX.is_match(txt)
}

/// Check whether `txt` is a relative IRI reference.
#[inline]
pub fn is_relative_iri_ref(txt: &str) -> bool {
    IRELATIVE_REF_REGEX.is_match(txt)
}

lazy_static! {
    /// Match an absolute IRI reference.
    pub static ref IRI_REGEX: Regex = Regex::new(r"(?x)^
        #scheme
       ( # CAPTURE scheme
        [A-Za-z] [-A-Za-z0-9+.]*
       )
        :
        #ihier_part
        (?: #iauthority + ipath_abempty
          //
         ( # CAPTURE iauthority
          (?: # iuserinfo
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:]
          |
            %[0-9a-fA-F]{2}
          )*
          @
          )?
          # ihost
          (?: # ip_literal
             \[
            (?: # ipv6address
              (?:
                (?:[0-9a-fA-F]{1,4}:){6}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                ::
                (?:[0-9a-fA-F]{1,4}:){5}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){4}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,1}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){3}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,2}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){2}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,3}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}:
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,4}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,5}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,6}:[0-9a-fA-F]{1,4})?
                ::
              )
            | # ipvfuture
              v[0-9a-fA-F]+ \. [-A-Za-z0-9._~!$&'()*+,;=:]+
            )
             \]
          | # ipv4address
            (?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5])) (?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3}
          | # ireg_name
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=]
              | %[0-9a-fA-F]{2}
              )*
          )
          (?:
            :
            [0-9]* # port
          )?
         )
          #ipath_abempty
         ( # CAPTURE ipath_abempty
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        | #ipath_absolute
         ( # CAPTURE ipath_absolute
          /
          (?:
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
            (?:
              /
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
              | %[0-9a-fA-F]{2}
              )*
            )*
          )?
         )
        | #ipath_rootless
         ( # CAPTURE ipath_rootless
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
          | %[0-9a-fA-F]{2}
          )+
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        )? # optional because of ipath_empty
        (?: # ?iquery
          \?
         ( # CAPTURE iquery
          (?:
            [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@'\u{E000}-\u{F8FF}\u{F0000}-\u{FFFFD}\u{100000}-\u{10FFFD}/?]
            | %[0-9a-fA-F]{2}
          )*
         )
        )?
        (?: # #ifragment
          \#
         ( # CAPTURE ifragment
          (?:
            [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@/?]
            | %[0-9a-fA-F]{2}
          )*
         )
        )?
    $").unwrap();

    /// Match a relative IRI.
    pub static ref IRELATIVE_REF_REGEX: Regex = Regex::new(r"(?x)^
        #irelative_part
        (?: #iauthority + ipath_abempty
          //
         ( # CAPTURE iauthority
          (?: # iuserinfo
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:]
          |
            %[0-9a-fA-F]{2}
          )*
          @
          )?
          # ihost
          (?: # ip_literal
             \[
            (?: # ipv6address
              (?:
                (?:[0-9a-fA-F]{1,4}:){6}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                ::
                (?:[0-9a-fA-F]{1,4}:){5}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){4}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,1}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){3}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,2}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:){2}
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,3}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}:
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,4}:[0-9a-fA-F]{1,4})?
                ::
                (?:[0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}|(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))(?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3})
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,5}:[0-9a-fA-F]{1,4})?
                ::
                [0-9a-fA-F]{1,4}
              |
                (?:(?:[0-9a-fA-F]{1,4}:){0,6}:[0-9a-fA-F]{1,4})?
                ::
              )
            | # ipvfuture
              v[0-9a-fA-F]+ \. [-A-Za-z0-9._~!$&'()*+,;=:]+
            )
             \]
          | # ipv4address
            (?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5])) (?:\.(?:[0-9]|(?:[1-9][0-9])|(?:1[0-9]{2})|(?:2[0-4][0-9])|(?:25[0-5]))){3}
          | # ireg_name
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=]
              | %[0-9a-fA-F]{2}
              )*
          )
          (?:
            :
            [0-9]* # port
          )?
         )
          #ipath_abempty
         ( # CAPTURE ipath_abempty
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        | #ipath_absolute
         ( # CAPTURE ipath_absolute
          /
          (?:
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
            (?:
              /
              (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
              | %[0-9a-fA-F]{2}
              )*
            )*
          )?
         )
        | #ipath_noscheme
         ( # CAPTURE ipath_noscheme
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=@]
          | %[0-9a-fA-F]{2}
          )+
          (?:
            /
            (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@]
            | %[0-9a-fA-F]{2}
            )*
          )*
         )
        )? # optional because of ipath_empty
        (?: # ?iquery
          \?
         ( # CAPTURE iquery
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@'\u{E000}-\u{F8FF}\u{F0000}-\u{FFFFD}\u{100000}-\u{10FFFD}/?]
          | %[0-9a-fA-F]{2}
          )*
         )
        )?
        (?: # #ifragment
            \#
         ( # CAPTURE ifragment
          (?: [-A-Za-z0-9._~\u{A0}-\u{D7FF}\u{F900}-\u{FDCF}\u{FDF0}-\u{FFEF}\u{10000}-\u{1FFFD}\u{20000}-\u{2FFFD}\u{30000}-\u{3FFFD}\u{40000}-\u{4FFFD}\u{50000}-\u{5FFFD}\u{60000}-\u{6FFFD}\u{70000}-\u{7FFFD}\u{80000}-\u{8FFFD}\u{90000}-\u{9FFFD}\u{A0000}-\u{AFFFD}\u{B0000}-\u{BFFFD}\u{C0000}-\u{CFFFD}\u{D0000}-\u{DFFFD}\u{E1000}-\u{EFFFD}!$&'()*+,;=:@/?]
          | %[0-9a-fA-F]{2}
          )*
         )
        )?
    $").unwrap();
}

#[cfg(test)]
mod test {
    use super::super::test::{NEGATIVE_IRIS, POSITIVE_IRIS};
    use super::*;

    #[test]
    fn regex_abs() {
        for (txt, parsed) in POSITIVE_IRIS {
            assert_eq!(IRI_REGEX.is_match(txt), parsed.0);
        }
        for txt in NEGATIVE_IRIS {
            assert!(!IRI_REGEX.is_match(txt));
        }
    }

    #[test]
    fn regex_rel() {
        for (txt, parsed) in POSITIVE_IRIS {
            assert_eq!(IRELATIVE_REF_REGEX.is_match(txt), !parsed.0);
        }
        for txt in NEGATIVE_IRIS {
            assert!(!IRELATIVE_REF_REGEX.is_match(txt));
        }
    }
}
