/// JSON-LD serializer configuration.
#[derive(Clone, Debug, Default)]
pub struct JsonLdConfig {
    pub rdf_direction: Option<RdfDirectionMode>,
    pub spaces: u16,
    pub spec_version: JsonLdSpecVersion,
    pub use_native_types: bool,
    pub use_rdf_type: bool,
}

impl JsonLdConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn spaces(mut self, spaces: u16) -> Self {
        self.spaces = spaces;
        self
    }

    pub fn spec_version(mut self, version: JsonLdSpecVersion) -> Self {
        self.spec_version = version;
        self
    }

    pub fn use_native_types(mut self, flag: bool) -> Self {
        self.use_native_types = flag;
        self
    }

    pub fn use_rdf_type(mut self, flag: bool) -> Self {
        self.use_rdf_type = flag;
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum JsonLdSpecVersion {
    JsonLd10,
    JsonLd11,
}

impl Default for JsonLdSpecVersion {
    fn default() -> Self {
        JsonLdSpecVersion::JsonLd11
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum RdfDirectionMode {
    I18nDatatype,
    CompoundLiteral,
}
