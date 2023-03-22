/// Create a "namespace module"
/// defining a set of terms within a given IRI space.
///
/// # Tests
/// This macro also create a test module to check that all created IRIs are valid.
///
/// This allows to skip those checks at runtime, keeping the initialization of the namespace fast.
#[macro_export]
macro_rules! namespace {
    ($iri_prefix:expr, $($suffix:ident),*; $($r_id:ident, $r_sf:expr),*) => {
        /// Prefix used in this namespace.
        pub static PREFIX: $crate::ns::IriRef<&'static str> = $crate::ns::IriRef::new_unchecked_const($iri_prefix);
        $(
            $crate::ns_iri!(PREFIX, $suffix);
        )*
        $(
            $crate::ns_iri!(PREFIX, $r_id, $r_sf);
        )*

        /// Test module for checking tha IRIs are valid
        #[cfg(test)]
        mod test_valid_iri {
            $(
                #[allow(non_snake_case)]
                #[test]
                fn $suffix() {
                    let iri = $crate::ns::NsTerm {
                        ns: super::PREFIX,
                        suffix: stringify!($suffix),
                    };
                    assert!($crate::ns::IriRef::new(iri.to_string()).is_ok());
                }
            )*
            $(
                #[allow(non_snake_case)]
                #[test]
                fn $r_id() {
                    let iri = $crate::ns::NsTerm {
                        ns: super::PREFIX,
                        suffix: $r_sf,
                    };
                    assert!($crate::ns::IriRef::new(iri.to_string()).is_ok());
                }
            )*
        }
    };
    ($iri_prefix:expr, $($suffix:ident),*) => {
        namespace!($iri_prefix, $($suffix),*;);
    };
}

/// Create a term in a "namespace module".
/// In general, you should use the [`namespace!`](macro.namespace.html) macro instead.
///
/// # Safety
/// This macro is conceptually unsafe,
/// as it is never checked that the prefix IRI is a valid IRI reference.
#[macro_export]
macro_rules! ns_iri {
    ($prefix:expr, $ident:ident) => {
        $crate::ns_iri!($prefix, $ident, stringify!($ident));
    };
    ($prefix:expr, $ident:ident, $suffix:expr) => {
        /// Generated term.
        #[allow(non_upper_case_globals)]
        pub static $ident: $crate::ns::NsTerm = $crate::ns::NsTerm::new_unchecked($prefix, $suffix);
    };
}
