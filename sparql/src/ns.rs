use std::sync::{Arc, LazyLock};

use sophia_api::{
    ns::{NsTerm, rdf, xsd},
    term::IriRef,
};

pub static RDF_DIR_LANG_STRING: LazyLock<IriRef<Arc<str>>> =
    LazyLock::new(|| memoize(&rdf::dirLangString));
pub static RDF_LANG_STRING: LazyLock<IriRef<Arc<str>>> =
    LazyLock::new(|| memoize(&rdf::langString));
pub static XSD_BOOLEAN: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| memoize(&xsd::boolean));
pub static XSD_DATE_TIME: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| memoize(&xsd::dateTime));
pub static XSD_DECIMAL: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| memoize(&xsd::decimal));
pub static XSD_DOUBLE: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| memoize(&xsd::double));
pub static XSD_INTEGER: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| memoize(&xsd::integer));
pub static XSD_FLOAT: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| memoize(&xsd::float));
pub static XSD_STRING: LazyLock<IriRef<Arc<str>>> = LazyLock::new(|| memoize(&xsd::string));

fn memoize(nsterm: &NsTerm) -> IriRef<Arc<str>> {
    nsterm.iriref().map_unchecked(|txt| Arc::from(txt.as_ref()))
}
