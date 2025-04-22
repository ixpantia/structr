use extendr_api::{prelude::*, ToVectorValue};
use hashbrown::{HashMap, HashSet};
use serde::de::{self, DeserializeSeed, Deserializer, Expected, MapAccess, SeqAccess, Visitor};
use std::convert::TryFrom;
use std::fmt;

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
            Structure::Integer => deserializer.deserialize_i32(visitor),
            Structure::Double => deserializer.deserialize_f64(visitor),
            Structure::String => deserializer.deserialize_string(visitor),
            Structure::Logical => deserializer.deserialize_bool(visitor),
            Structure::Vector(_) => deserializer.deserialize_seq(visitor),
            Structure::Map { .. } => deserializer.deserialize_map(visitor),
            Structure::Date { .. } => {
                // Handle date types separately if needed
                deserializer.deserialize_string(visitor)
            }
            Structure::Optional(_) => {
                // Handle optional types by allowing null values
                deserializer.deserialize_option(visitor)
            }
        }
    }
}

fn parse_r_date(v: &str, format: &'static str) -> std::result::Result<f64, chrono::ParseError> {
    let date = chrono::NaiveDate::parse_from_str(v, format)?;

    // Convert chrono::NaiveDate to an integer counting days since
    // EPOCH
    const EPOCH: chrono::NaiveDate = chrono::NaiveDate::from_ymd(1970, 1, 1);
    Ok((date - EPOCH).num_days() as f64)
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
        Structure::Date { .. } => "a date",
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
            match structure.as_ref() {
                Structure::Integer => deserializer.deserialize_i32(visitor),
                Structure::Double => deserializer.deserialize_f64(visitor),
                Structure::String => deserializer.deserialize_string(visitor),
                Structure::Logical => deserializer.deserialize_bool(visitor),
                Structure::Vector(_) => deserializer.deserialize_seq(visitor),
                Structure::Date { .. } => deserializer.deserialize_string(visitor),
                Structure::Map { .. } => deserializer.deserialize_map(visitor),
                _ => Err(de::Error::invalid_type(de::Unexpected::Option, &self)),
            }
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Option, &self))
        }
    }

    fn visit_unit<E>(self) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        if let Structure::Optional(_) = self.structure {
            // Allow null for optional types
            Ok(Nullable::Null)
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Unit, &self))
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

    fn visit_i32<E>(self, v: i32) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        match self.structure {
            Structure::Integer => Ok(NotNull(Robj::from(v))),
            Structure::Double => Ok(NotNull(Robj::from(v as f64))),
            _ => Err(de::Error::invalid_type(
                de::Unexpected::Signed(v as i64),
                &self,
            )),
        }
    }

    fn visit_u32<E>(self, v: u32) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        match self.structure {
            Structure::Integer => Ok(NotNull(Robj::from(v as i32))),
            Structure::Double => Ok(NotNull(Robj::from(v as f64))),
            _ => Err(de::Error::invalid_type(
                de::Unexpected::Unsigned(v as u64),
                &self,
            )),
        }
    }

    fn visit_f64<E>(self, v: f64) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        match self.structure {
            Structure::Double => Ok(NotNull(Robj::from(v))),
            _ => Err(de::Error::invalid_type(de::Unexpected::Float(v), &self)),
        }
    }

    fn visit_str<E>(self, v: &str) -> std::result::Result<Self::Value, E>
    where
        E: de::Error,
    {
        match *self.structure {
            Structure::String => Ok(NotNull(Robj::from(v))),
            Structure::Date { format } => {
                let days_since_epoch = parse_r_date(v, format)
                    .map_err(|_| de::Error::invalid_value(de::Unexpected::Str(v), &self))?;
                let mut date = Robj::from(days_since_epoch);
                date.set_class(["Date"]).expect("Failed to set class");

                // Handle date parsing here if needed
                // For now, just return as a string
                Ok(NotNull(date))
            }
            _ => Err(de::Error::invalid_type(de::Unexpected::Str(v), &self)),
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
        fn seq_to_r_vec<'de, T, A>(
            mut seq: A,
            size_hint: usize,
        ) -> std::result::Result<Nullable<Robj>, A::Error>
        where
            T: serde::Deserialize<'de> + ToVectorValue,
            A: SeqAccess<'de>,
        {
            let mut ints = Vec::with_capacity(size_hint);
            while let Some(value) = seq.next_element::<T>()? {
                ints.push(value);
            }
            Ok(NotNull(Robj::from(ints)))
        }

        fn seq_to_r_vec_date<'de, A>(
            mut seq: A,
            format: &'static str,
            size_hint: usize,
        ) -> std::result::Result<Nullable<Robj>, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut ints = Vec::with_capacity(size_hint);
            while let Some(value) = seq.next_element::<&str>()? {
                let date = parse_r_date(value, format).map_err(|e| {
                    de::Error::invalid_value(de::Unexpected::Str(&e.to_string()), &format)
                })?;
                ints.push(date);
            }
            let mut obj = Robj::from(ints);
            obj.set_class(["Date"]).expect("Failed to set class");
            Ok(NotNull(obj))
        }

        fn seq_to_r_vec_date_opt<'de, A>(
            mut seq: A,
            format: &'static str,
            size_hint: usize,
        ) -> std::result::Result<Nullable<Robj>, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut ints = Vec::with_capacity(size_hint);
            while let Some(value) = seq.next_element::<Option<&str>>()? {
                match value {
                    None => ints.push(None),
                    Some(value) => {
                        let date = parse_r_date(value, format).map_err(|e| {
                            de::Error::invalid_value(de::Unexpected::Str(&e.to_string()), &format)
                        })?;
                        ints.push(Some(date));
                    }
                }
            }
            let mut obj = Robj::from(ints);
            obj.set_class(["Date"]).expect("Failed to set class");
            Ok(NotNull(obj))
        }

        if let Structure::Vector(element_structure) = self.structure {
            let size_h = seq.size_hint().unwrap_or(0);
            match element_structure.as_ref() {
                Structure::Integer => return seq_to_r_vec::<i32, A>(seq, size_h),
                Structure::Double => return seq_to_r_vec::<f64, A>(seq, size_h),
                Structure::Logical => return seq_to_r_vec::<bool, A>(seq, size_h),
                Structure::String => return seq_to_r_vec::<&str, A>(seq, size_h),
                Structure::Date { format } => return seq_to_r_vec_date::<A>(seq, format, size_h),
                Structure::Optional(element_structure) => match element_structure.as_ref() {
                    Structure::Integer => return seq_to_r_vec::<Option<i32>, A>(seq, size_h),
                    Structure::Double => return seq_to_r_vec::<Option<f64>, A>(seq, size_h),
                    Structure::Logical => return seq_to_r_vec::<Option<bool>, A>(seq, size_h),
                    Structure::String => return seq_to_r_vec::<Option<&str>, A>(seq, size_h),
                    Structure::Date { format } => {
                        return seq_to_r_vec_date_opt::<A>(seq, format, size_h)
                    }
                    _ => (),
                },
                _ => (),
            }

            // Anything not handled above is a custom structure
            // We need to create a new seed for the elements

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

            let mut r_names = Vec::with_capacity(fields.len());
            let mut r_values = Vec::with_capacity(fields.len());
            let mut visited_names = HashSet::new();

            // Process entries from the JSON map
            while let Some(key_str) = map.next_key::<&str>()? {
                if let Some(expected_field_structure) = fields.get(key_str) {
                    if visited_names.contains(&key_str) {
                        // Duplicate key found in JSON
                        return Err(de::Error::custom(format!(
                            "Duplicate key '{}' in JSON object",
                            key_str
                        )));
                    }

                    // Found an expected key, deserialize the value using the specific seed
                    let value_seed = StructureSeed {
                        structure: expected_field_structure,
                    };
                    let value_robj = map.next_value_seed(value_seed)?;

                    r_names.push(key_str);
                    r_values.push(value_robj);
                    visited_names.insert(key_str);
                } else {
                    if ignore_extra_fields {
                        continue; // Ignore extra fields if specified
                    }

                    return Err(de::Error::unknown_field(key_str, expected_fields_str));
                }
            }

            for defined_name in *expected_fields_str {
                if !r_names.contains(defined_name) {
                    if let Some(Structure::Optional(_)) = fields.get(*defined_name) {
                        continue;
                    }

                    return Err(de::Error::missing_field(defined_name));
                }
            }

            // Create the R list from the names and values
            let mut r_list = List::from_values(r_values);
            r_list.set_names(r_names).expect("Failed to set names");

            Ok(NotNull(r_list.into()))
        } else {
            Err(de::Error::invalid_type(de::Unexpected::Map, &self))
        }
    }
}

#[extendr]
fn parse_json_impl(json_string: String, structure: &Structure) -> Result<Nullable<Robj>> {
    let mut byte_data = json_string.into_bytes();

    let mut deserializer = simd_json::Deserializer::from_slice(&mut byte_data)
        .map_err(|e| extendr_api::Error::Other(e.to_string()))?;

    let seed = StructureSeed { structure };

    seed.deserialize(&mut deserializer)
        .map_err(|e| extendr_api::Error::Other(e.to_string()))
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod structr;
    fn parse_json_impl; // Name remains the same
    impl Structure; // Export the Structure enum
}
