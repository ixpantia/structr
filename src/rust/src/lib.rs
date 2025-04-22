use extendr_api::prelude::*;
use serde::de::{self, DeserializeSeed, Deserializer, MapAccess, SeqAccess, Visitor};
use std::collections::HashMap;
// Keep serde_json for the Deserializer
use std::convert::TryFrom;
use std::fmt;

// --- Structure Enum (Remains the same) ---
#[derive(Debug, Clone, PartialEq)] // Add PartialEq for easier comparison
#[extendr]
enum Structure {
    Map {
        fields: HashMap<Box<str>, Structure>,
        ignore_extra_fields: bool,
        expected_fields_str: &'static [&'static str], // We will need to leak some memory to have
                                                      // comprehensible error messages
    },
    Vector(Box<Structure>),
    Integer,
    Double,
    String,
    Logical,
    Optional(Box<Structure>), // Optional type for nullable fields
}

#[extendr]
impl Structure {
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
                    fields.insert(key.into(), field_structure);
                }

                let ignore_extra_fields = list
                    .index("ignore_extra_fields")?
                    .as_bool()
                    .unwrap_or(false);

                fn expected_fields_str(
                    fields: &HashMap<Box<str>, Structure>,
                ) -> &'static [&'static str] {
                    let fields = fields
                        .keys()
                        .cloned()
                        .map(|k| Box::leak(k) as &'static str)
                        .collect::<Box<[&'static str]>>();

                    Box::leak(fields)
                }

                let expected_fields_str = expected_fields_str(&fields);

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

// --- NEW: StructureSeed ---
// This struct holds the schema (`Structure`) and implements DeserializeSeed.
#[derive(Copy, Clone)]
struct StructureSeed<'a> {
    structure: &'a Structure,
}

impl<'de, 'a> DeserializeSeed<'de> for StructureSeed<'a> {
    // The type that will be produced is an Robj
    type Value = Nullable<Robj>;

    fn deserialize<D>(self, deserializer: D) -> std::result::Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        // Create the visitor, passing the structure reference
        let visitor = StructureVisitor {
            structure: self.structure,
        };

        // Delegate deserialization based on the expected structure type
        match self.structure {
            Structure::Integer => deserializer.deserialize_i64(visitor), // Prefer i64 for JSON number flexibility
            Structure::Double => deserializer.deserialize_f64(visitor),
            Structure::String => deserializer.deserialize_string(visitor),
            Structure::Logical => deserializer.deserialize_bool(visitor),
            Structure::Vector(_) => deserializer.deserialize_seq(visitor),
            Structure::Map { .. } => deserializer.deserialize_map(visitor),
            Structure::Optional(_) => {
                // Handle optional types by allowing null values
                deserializer.deserialize_option(visitor)
            }
        }
    }
}

// --- NEW: StructureVisitor ---
// This struct implements the Visitor trait to handle the actual data conversion.
struct StructureVisitor<'a> {
    structure: &'a Structure,
}

// Helper to format expected type for error messages
fn expected_type_str(s: &Structure) -> &'static str {
    match s {
        Structure::Integer => "an integer",
        Structure::Double => "a floating-point number",
        Structure::String => "a string",
        Structure::Logical => "a boolean (true/false)",
        Structure::Vector(_) => "an array/vector",
        Structure::Map { .. } => "an object/map",
        Structure::Optional(_) => "an optional value (nullable)",
    }
}

impl<'de, 'a> Visitor<'de> for StructureVisitor<'a> {
    // The type we are creating
    type Value = Nullable<Robj>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", expected_type_str(self.structure))
    }

    fn visit_some<D>(self, deserializer: D) -> std::result::Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        if let Structure::Optional(structure) = self.structure {
            let visitor = StructureVisitor { structure };
            // Allow null for optional types
            deserializer.deserialize_any(visitor)
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Option, &self))
        }
    }

    fn visit_none<E>(self) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        if let Structure::Optional(_) = self.structure {
            // Allow null for optional types
            Ok(Nullable::Null)
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Option, &self))
        }
    }

    fn visit_bool<E>(self, v: bool) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        if *self.structure == Structure::Logical {
            Ok(NotNull(Robj::from(v)))
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Bool(v), &self))
        }
    }

    fn visit_i64<E>(self, v: i64) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        match self.structure {
            Structure::Integer => {
                // R integers are i32. Check for overflow.
                if v >= i32::MIN as i64 && v <= i32::MAX as i64 {
                    Ok(NotNull(Robj::from(v as i32)))
                } else {
                    Err(de::Error::invalid_value(
                        de::Unexpected::Signed(v),
                        &"an integer representable in R (32-bit signed)",
                    ))
                }
            }
            Structure::Double => {
                // Allow integer to be treated as double
                Ok(NotNull(Robj::from(v as f64)))
            }
            _ => Err(de::Error::invalid_type(de::Unexpected::Signed(v), &self)),
        }
    }

    // Also handle u64 just in case JSON parser produces it (though less common)
    fn visit_u64<E>(self, v: u64) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        match self.structure {
            Structure::Integer => {
                if v <= i32::MAX as u64 {
                    // Check against i32 max
                    Ok(NotNull(Robj::from(v as i32)))
                } else {
                    Err(de::Error::invalid_value(
                        de::Unexpected::Unsigned(v),
                        &"an integer representable in R (32-bit signed)",
                    ))
                }
            }
            Structure::Double => {
                // Allow integer to be treated as double
                Ok(NotNull(Robj::from(v as f64)))
            }
            _ => Err(de::Error::invalid_type(de::Unexpected::Unsigned(v), &self)),
        }
    }

    fn visit_f64<E>(self, v: f64) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        match self.structure {
            Structure::Integer => {
                // Allow floats that are whole numbers if strict int required
                if v.fract() == 0.0 && v >= i32::MIN as f64 && v <= i32::MAX as f64 {
                    Ok(NotNull(Robj::from(v as i32)))
                } else {
                    Err(de::Error::invalid_value(
                        de::Unexpected::Float(v),
                        &"an integer or a whole number float representable as i32",
                    ))
                }
            }
            Structure::Double => Ok(NotNull(Robj::from(v))),
            _ => Err(de::Error::invalid_type(de::Unexpected::Float(v), &self)),
        }
    }

    fn visit_str<E>(self, v: &str) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        if *self.structure == Structure::String {
            Ok(NotNull(Robj::from(v)))
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Str(v), &self))
        }
    }

    fn visit_string<E>(self, v: String) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        // Optimization: If it's a String, we can potentially avoid a copy
        if *self.structure == Structure::String {
            Ok(NotNull(Robj::from(v))) // Robj::from String might be slightly more efficient
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Str(&v), &self))
        }
    }

    // --- Visit Methods for Compound Types ---

    fn visit_seq<A>(self, mut seq: A) -> std::result::Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        if let Structure::Vector(element_structure) = self.structure {
            let mut values = Vec::with_capacity(seq.size_hint().unwrap_or(0));
            // Create the seed for elements *once*
            let element_seed = StructureSeed {
                structure: element_structure,
            };
            // Deserialize each element using the element_seed
            while let Some(value) = seq.next_element_seed(element_seed)? {
                values.push(value);
            }
            // Return as an R list. Coercing to atomic vector within Rust
            // is complex due to R's type system and NA handling. Let R handle coercion if needed.
            Ok(NotNull(List::from_values(values).into()))
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Seq, &self))
        }
    }

    fn visit_map<A>(self, mut map: A) -> std::result::Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        if let Structure::Map {
            fields,
            ignore_extra_fields,
            expected_fields_str,
        } = self.structure
        {
            let ignore_extra_fields = *ignore_extra_fields;
            let mut r_map = HashMap::<&str, Nullable<Robj>>::new();

            // Process entries from the JSON map
            while let Some(key_str) = map.next_key::<&str>()? {
                if let Some(expected_field_structure) = fields.get(key_str) {
                    // Found an expected key, deserialize the value using the specific seed
                    let value_seed = StructureSeed {
                        structure: expected_field_structure,
                    };
                    let value_robj = map.next_value_seed(value_seed)?;

                    if r_map.contains_key(key_str) {
                        // Duplicate key found in JSON
                        return Err(de::Error::custom(format!(
                            "Duplicate key '{}' in JSON object",
                            key_str
                        )));
                    }

                    r_map.insert(key_str, value_robj);
                } else {
                    if ignore_extra_fields {
                        continue; // Ignore extra fields if specified
                    }

                    return Err(de::Error::unknown_field(key_str, expected_fields_str));
                }
            }

            for defined_name in *expected_fields_str {
                if !r_map.contains_key(defined_name) {
                    // TODO: Handle missing required fields
                    return Err(de::Error::missing_field(defined_name));
                }
            }

            let r_list = List::from_hashmap(r_map).expect("Failed to create R list");

            Ok(NotNull(r_list.into()))
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Map, &self))
        }
    }
}

/// Parses a JSON string according to a provided structure definition using streaming.
///
/// @param json_string A character string containing the JSON data.
/// @param structure An R list object representing the structure, created by `build_structure()`.
/// @return An R object (list, vector, atomic) representing the parsed JSON if validation succeeds.
/// @ Rtype R_parse_json(json_string: String, structure: list) -> Robj
/// @export
#[extendr]
fn parse_json_impl(json_string: &str, structure: &Structure) -> Result<Nullable<Robj>> {
    // 2. Create a JSON deserializer from the input string slice
    // Use StrDeserializer for zero-copy reading from the string slice
    let mut deserializer = serde_json::Deserializer::from_str(json_string);

    // 3. Create the top-level seed
    let seed = StructureSeed { structure };

    // 4. Deserialize using the seed
    // Map the serde error to an extendr error for R
    seed.deserialize(&mut deserializer)
        .map_err(|e| extendr_api::Error::Other(e.to_string()))

    // Optional: Check if there is any trailing data after the main JSON value
    // .and_then(|result| {
    //     deserializer.end().map_err(|e| extendr_api::Error::Other(format!("Trailing data after JSON: {}", e)))?;
    //     Ok(result)
    // })
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod structr;
    fn parse_json_impl; // Name remains the same
    impl Structure; // Export the Structure enum
}
