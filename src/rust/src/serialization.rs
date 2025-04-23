use crate::{Structure, R_EPOCH};
use extendr_api::prelude::*;
use serde::ser::{Error, SerializeMap, SerializeSeq};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
enum SerError {
    // (Actual, Expected)
    TypeMismatch(&'static str),
    OptionalMismatch(&'static str),
}

fn is_atomic(structure: &Structure) -> bool {
    matches!(
        structure,
        Structure::Integer
            | Structure::Double
            | Structure::Logical
            | Structure::String
            | Structure::Date { .. }
    )
}

fn type_mismatch<E>(structure: &Structure) -> E
where
    E: Error,
{
    Error::custom(SerError::TypeMismatch(crate::expected_type_str(structure)))
}

fn non_optional<E>(structure: &Structure) -> E
where
    E: Error,
{
    Error::custom(SerError::OptionalMismatch(crate::expected_type_str(
        structure,
    )))
}

impl fmt::Display for SerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SerError::TypeMismatch(expected) => {
                write!(f, "Type mismatch: expected {}", expected)
            }
            SerError::OptionalMismatch(expected) => {
                write!(
                    f,
                    "Found NA/null values in non-optional field of type {}",
                    expected
                )
            }
        }
    }
}

struct StructrWithData<'a> {
    structure: &'a Structure,
    data: Robj,
}

fn check_for_date_class(data: &Robj) -> bool {
    if let Some(mut classes) = data.class() {
        classes.any(|class| class == "Date")
    } else {
        false
    }
}

fn int_to_date(data: f64, format: &str) -> String {
    (R_EPOCH + chrono::Duration::days(data as i64))
        .format(format)
        .to_string()
}

impl<'a> serde::Serialize for StructrWithData<'a> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if !matches!(self.structure, Structure::Optional(_)) && self.data.is_null() {
            return Err(non_optional(self.structure));
        }

        match &self.structure {
            Structure::Date { format } => {
                if self.data.is_na() {
                    return Err(non_optional(self.structure));
                }

                let data = self
                    .data
                    .as_real()
                    .ok_or_else(|| type_mismatch(self.structure))?;

                if !check_for_date_class(&self.data) {
                    return Err(type_mismatch(self.structure));
                }

                // Convert the integer to a NaiveDate
                let date = int_to_date(data, format);
                serializer.serialize_str(&date)
            }
            Structure::Integer => {
                if self.data.is_na() {
                    return Err(non_optional(self.structure));
                }

                let data = self
                    .data
                    .as_integer()
                    .ok_or_else(|| type_mismatch(self.structure))?;
                serializer.serialize_i32(data)
            }
            Structure::Double => {
                if self.data.is_na() {
                    return Err(non_optional(self.structure));
                }

                let data = self
                    .data
                    .as_real()
                    .ok_or_else(|| type_mismatch(self.structure))?;
                serializer.serialize_f64(data)
            }
            Structure::Logical => {
                if self.data.is_na() {
                    return Err(non_optional(self.structure));
                }

                let data = self
                    .data
                    .as_bool()
                    .ok_or_else(|| type_mismatch(self.structure))?;
                serializer.serialize_bool(data)
            }
            Structure::String => {
                if self.data.is_na() {
                    return Err(non_optional(self.structure));
                }

                let data = self
                    .data
                    .as_str()
                    .ok_or_else(|| type_mismatch(self.structure))?;

                serializer.serialize_str(data)
            }
            Structure::Map { fields, .. } => {
                let data = self
                    .data
                    .as_list()
                    .ok_or_else(|| type_mismatch(self.structure))?;

                let mut map = serializer.serialize_map(Some(data.len()))?;

                for (&key, inner_structure) in fields.iter() {
                    let item = data.index(key).unwrap_or_else(|_| r!(NULL));
                    let nested_structr = StructrWithData {
                        structure: inner_structure,
                        data: item,
                    };
                    map.serialize_entry(key, &nested_structr)?;
                }

                map.end()
            }
            Structure::Vector(inner_structure) => match inner_structure.as_ref() {
                Structure::Date { format } => {
                    let data = self
                        .data
                        .as_real_slice()
                        .ok_or_else(|| type_mismatch(inner_structure))?;

                    if !check_for_date_class(&self.data) {
                        return Err(type_mismatch(inner_structure));
                    }

                    let mut seq = serializer.serialize_seq(Some(data.len()))?;
                    for &item in data.iter() {
                        if item.is_na() {
                            return Err(non_optional(inner_structure.as_ref()));
                        }
                        let date = int_to_date(item, format);
                        seq.serialize_element(&date)?;
                    }

                    seq.end()
                }
                Structure::Integer => {
                    let data = self
                        .data
                        .as_integer_slice()
                        .ok_or_else(|| type_mismatch(inner_structure))?;

                    let mut seq = serializer.serialize_seq(Some(data.len()))?;
                    for item in data.iter() {
                        if item.is_na() {
                            return Err(non_optional(inner_structure.as_ref()));
                        }
                        seq.serialize_element(&item)?;
                    }
                    seq.end()
                }
                Structure::Double => {
                    let data = self
                        .data
                        .as_real_slice()
                        .ok_or_else(|| type_mismatch(inner_structure))?;

                    let mut seq = serializer.serialize_seq(Some(data.len()))?;
                    for item in data.iter() {
                        if item.is_na() {
                            return Err(non_optional(inner_structure.as_ref()));
                        }
                        seq.serialize_element(&item)?;
                    }
                    seq.end()
                }
                Structure::Logical => {
                    let data = self
                        .data
                        .as_logical_slice()
                        .ok_or_else(|| type_mismatch(inner_structure))?;

                    let mut seq = serializer.serialize_seq(Some(data.len()))?;
                    for item in data.iter() {
                        if item.is_na() {
                            return Err(non_optional(inner_structure.as_ref()));
                        }
                        seq.serialize_element(&item.to_bool())?;
                    }
                    seq.end()
                }
                Structure::String => {
                    let data = self
                        .data
                        .as_str_iter()
                        .ok_or_else(|| type_mismatch(inner_structure))?;

                    let mut seq = serializer.serialize_seq(Some(data.len()))?;
                    for item in data.into_iter() {
                        if item.is_na() {
                            return Err(non_optional(inner_structure.as_ref()));
                        }
                        seq.serialize_element(&item)?;
                    }
                    seq.end()
                }
                Structure::Optional(inner_structure_opt) => match inner_structure_opt.as_ref() {
                    Structure::Date { format } => {
                        let data = self
                            .data
                            .as_real_slice()
                            .ok_or_else(|| type_mismatch(inner_structure_opt))?;

                        let mut seq = serializer.serialize_seq(Some(data.len()))?;
                        for &item in data.iter() {
                            if item.is_na() {
                                seq.serialize_element(&None::<String>)?;
                            } else {
                                let date = int_to_date(item, format);
                                seq.serialize_element(&Some(date))?;
                            }
                        }
                        seq.end()
                    }
                    Structure::Integer => {
                        let data = self
                            .data
                            .as_integer_slice()
                            .ok_or_else(|| type_mismatch(inner_structure_opt))?;

                        let mut seq = serializer.serialize_seq(Some(data.len()))?;
                        for item in data.iter() {
                            if item.is_na() {
                                seq.serialize_element(&None::<i32>)?;
                            } else {
                                seq.serialize_element(&Some(item))?;
                            }
                        }
                        seq.end()
                    }
                    Structure::Double => {
                        let data = self
                            .data
                            .as_real_slice()
                            .ok_or_else(|| type_mismatch(inner_structure_opt))?;

                        let mut seq = serializer.serialize_seq(Some(data.len()))?;
                        for item in data.iter() {
                            if item.is_na() {
                                seq.serialize_element(&None::<f64>)?;
                            } else {
                                seq.serialize_element(&Some(item))?;
                            }
                        }
                        seq.end()
                    }
                    Structure::Logical => {
                        let data = self
                            .data
                            .as_logical_slice()
                            .ok_or_else(|| type_mismatch(inner_structure_opt))?;

                        let mut seq = serializer.serialize_seq(Some(data.len()))?;
                        for item in data.iter() {
                            if item.is_na() {
                                seq.serialize_element(&None::<bool>)?;
                            } else {
                                seq.serialize_element(&Some(item.to_bool()))?;
                            }
                        }
                        seq.end()
                    }
                    Structure::String => {
                        let data = self
                            .data
                            .as_str_iter()
                            .ok_or_else(|| type_mismatch(inner_structure_opt))?;

                        let mut seq = serializer.serialize_seq(Some(data.len()))?;
                        for item in data.into_iter() {
                            if item.is_na() {
                                seq.serialize_element(&None::<String>)?;
                            } else {
                                seq.serialize_element(&Some(item.to_string()))?;
                            }
                        }
                        seq.end()
                    }
                    _ => {
                        // If we are here that means this is a vector of optionals
                        // that are not atomic.
                        //
                        // Since we have already implemented logic below for
                        // serializing optionals, we should call that
                        // for every item. Note that I wont use
                        // `inner_structure_opt` but rather `inner_structure`
                        // since we this is a vector of the optional type,
                        // it would be redundant to check for the optional type
                        // again.
                        let data = self
                            .data
                            .as_list()
                            .ok_or_else(|| type_mismatch(self.structure))?;
                        let mut seq = serializer.serialize_seq(Some(data.len()))?;
                        for item in data.values() {
                            seq.serialize_element(&StructrWithData {
                                structure: inner_structure.as_ref(),
                                data: item.clone(),
                            })?;
                        }
                        seq.end()
                    }
                },
                _ => {
                    let data = self
                        .data
                        .as_list()
                        .ok_or_else(|| type_mismatch(inner_structure))?;
                    let mut seq = serializer.serialize_seq(Some(data.len()))?;

                    for item in data.values() {
                        let nested_structr = StructrWithData {
                            structure: inner_structure.as_ref(),
                            data: item.clone(),
                        };

                        seq.serialize_element(&nested_structr)?;
                    }
                    seq.end()
                }
            },
            Structure::Optional(inner_structure) => {
                // If is an atomic type, check for NA
                if is_atomic(inner_structure) && self.data.is_na() {
                    return serializer.serialize_none();
                }

                if self.data.is_null() {
                    return serializer.serialize_none();
                }

                let nested_structr = StructrWithData {
                    structure: inner_structure,
                    data: self.data.clone(),
                };

                serializer.serialize_some(&nested_structr)
            }
        }
    }
}

#[extendr]
fn serialize_structure(structure: &Structure, data: Robj) -> Result<String> {
    let structr_with_data = StructrWithData { structure, data };
    let serialized = simd_json::to_string(&structr_with_data)
        .map_err(|e| extendr_api::Error::Other(format!("Failed to serialize structure: {}", e)))?;
    Ok(serialized)
}

#[extendr]
fn serialize_structure_pretty(structure: &Structure, data: Robj) -> Result<String> {
    let structr_with_data = StructrWithData { structure, data };
    let serialized = simd_json::to_string_pretty(&structr_with_data)
        .map_err(|e| extendr_api::Error::Other(format!("Failed to serialize structure: {}", e)))?;
    Ok(serialized)
}

extendr_module! {
    mod serialization;
    fn serialize_structure;
    fn serialize_structure_pretty;
}
