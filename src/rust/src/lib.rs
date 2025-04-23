mod deserialization;
mod serialization;

use extendr_api::prelude::*;
use hashbrown::HashMap;

pub(crate) const R_EPOCH: chrono::NaiveDate = match chrono::NaiveDate::from_ymd_opt(1970, 1, 1) {
    Some(date) => date,
    None => panic!("Failed to create R epoch date"),
};

// --- Structure Enum (Remains the same) ---
#[derive(Debug, Clone, PartialEq)] // Add PartialEq for easier comparison
#[extendr]
enum Structure {
    Map {
        fields: HashMap<&'static str, Structure>,
        ignore_extra_fields: bool,
        expected_fields_str: &'static [&'static str], // We will need to leak some memory to have
                                                      // comprehensible error messages
    },
    Vector(Box<Structure>),
    Date {
        format: &'static str,
    },
    Integer,
    Double,
    String,
    Logical,
    Optional(Box<Structure>), // Optional type for nullable fields
}

#[extendr]
impl Structure {
    fn print(&self) {
        rprintln!("{:#?}", self);
    }
    fn convert_from_robj(value: Robj) -> Result<Self> {
        let list = value
            .as_list()
            .ok_or_else(|| extendr_api::Error::Other("Input must be a list.".into()))?;

        let type_str = list
            .index("type")?
            .as_str()
            .ok_or_else(|| extendr_api::Error::Other("'type' element must be a string.".into()))?;

        match type_str {
            "optional" => {
                let val_robj = list.index("value")?;
                let element_structure = Structure::convert_from_robj(val_robj)?;
                Ok(Structure::Optional(Box::new(element_structure)))
            }
            "date" => {
                let format = list.index("value")?.as_str().ok_or_else(|| {
                    extendr_api::Error::Other("'value' element must be a string.".into())
                })?;
                Ok(Structure::Date { format })
            }
            "string" => Ok(Structure::String),
            "integer" => Ok(Structure::Integer),
            "double" => Ok(Structure::Double),
            "logical" => Ok(Structure::Logical),
            "vector" => {
                let val_robj = list.index("value")?;
                let element_structure = Structure::convert_from_robj(val_robj)?;
                Ok(Structure::Vector(Box::new(element_structure)))
            }
            "map" => {
                let val_robj = list.index("value")?;
                let map_list = val_robj.as_list().ok_or_else(|| {
                    extendr_api::Error::Other("'value' element for map must be a list.".into())
                })?;
                let mut fields = HashMap::new();
                for (key, val) in map_list.iter() {
                    if fields.contains_key(key) {
                        return Err(extendr_api::Error::Other(format!(
                            "Duplicate key found in map structure definition: '{}'",
                            key
                        )));
                    }
                    let field_structure = Structure::convert_from_robj(val)?;

                    let key: &'static str = Box::leak(key.into());

                    fields.insert(key, field_structure);
                }

                let ignore_extra_fields = list
                    .index("ignore_extra_fields")?
                    .as_bool()
                    .unwrap_or(false);

                let expected_fields_str =
                    Box::leak(fields.keys().cloned().collect::<Box<[&'static str]>>());

                Ok(Structure::Map {
                    fields,
                    expected_fields_str,
                    ignore_extra_fields,
                })
            }
            _ => Err(extendr_api::Error::Other(format!(
                "Unknown structure type: {}",
                type_str
            ))),
        }
    }
}

// Helper to format expected type for error messages
pub(crate) fn expected_type_str(s: &Structure) -> &'static str {
    match s {
        Structure::Integer => "an integer",
        Structure::Double => "a floating-point number",
        Structure::String => "a string",
        Structure::Logical => "a boolean (true/false)",
        Structure::Vector(inner) => match inner.as_ref() {
            Structure::Integer => "a vector of integers",
            Structure::Double => "a vector of floating-point numbers",
            Structure::String => "a vector of strings",
            Structure::Logical => "a vector of booleans (true/false)",
            Structure::Date { .. } => "a vector of dates",
            _ => "a list/vector",
        },
        Structure::Map { .. } => "an object/map",
        Structure::Optional(_) => "an optional value (nullable)",
        Structure::Date { .. } => "a date",
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod structr;
    impl Structure; // Export the Structure enum
    use deserialization;
    use serialization;
}
