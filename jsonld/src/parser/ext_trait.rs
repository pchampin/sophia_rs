use json::{object::Object, JsonValue};

pub trait JsonValueExt {
    fn as_array(&self) -> Option<&Vec<JsonValue>>;
    fn as_object(&self) -> Option<&Object>;
}

impl JsonValueExt for JsonValue {
    fn as_array(&self) -> Option<&Vec<JsonValue>> {
        match self {
            JsonValue::Array(v) => Some(v),
            _ => None,
        }
    }
    fn as_object(&self) -> Option<&Object> {
        match self {
            JsonValue::Object(obj) => Some(obj),
            _ => None,
        }
    }
}

pub trait ObjectExt {
    fn get_all(&self, key: &str) -> Box<dyn Iterator<Item = &JsonValue> + '_>;
}

impl ObjectExt for Object {
    fn get_all(&self, key: &str) -> Box<dyn Iterator<Item = &JsonValue> + '_> {
        all_items(self.get(key).unwrap_or(&JsonValue::Null))
    }
}

pub fn all_items(item: &JsonValue) -> Box<dyn Iterator<Item = &JsonValue> + '_> {
    match item {
        JsonValue::Array(a) => Box::new(a.iter()),
        JsonValue::Null => Box::new([].iter()),
        x => Box::new(std::iter::once(x)),
    }
}
