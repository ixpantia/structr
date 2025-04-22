# structr: Fast, Validating JSON Parsing for R

<!-- badges: start -->
[![R-CMD-check](https://github.com/ixpantia/structr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ixpantia/structr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- Add build status/codecov badges here later -->
<!-- badges: end -->

## Overview

`structr` provides a robust and efficient way to parse JSON data in R while strictly validating it against a user-defined schema. It ensures that incoming JSON not only conforms to expected structures (objects, arrays, nesting) but also adheres to specific data types (integer, double, string, logical, optional/nullable).

The core logic is implemented in Rust using the highly performant `serde` and `simd_json` crates (accessed via `{extendr}`), offering significant speed advantages over traditional R methods, especially when validation is required.

## Features

*   **Schema Definition:** Define expected JSON structures using intuitive R functions (`s_map`, `s_vector`, `s_integer`, `s_double`, `s_string`, `s_logical`, `s_optional`).
*   **Strict Validation:** Enforces type correctness, presence of required fields, nesting, and handles `null` values via `s_optional`. By default, extra fields in objects cause errors.
*   **High Performance:** Faster than base R or `jsonlite` for *validated* parsing, as validation is integrated into the Rust parsing process.
*   **Informative Errors:** Provides detailed messages pinpointing the location and nature of validation failures.

## Missing / Planned Features

*   JSON Serialization (R object -> JSON string).
*   Custom validation rules/functions.
*   Built-in support for more complex types (e.g., dates, enums).

## Performance Advantage

Unlike `jsonlite::fromJSON` which focuses on flexible parsing, `structr::parse_json` *simultaneously* parses and validates against the predefined structure. This often results in significantly faster execution times when validation is a requirement, as the checks are integrated into the high-performance Rust parsing layer (`simd_json`). It avoids the overhead of parsing into a flexible R object and then validating separately.

## Installation

**Prerequisites:**

You need the Rust compiler (`rustc`) and package manager (`cargo`) installed. The easiest way is via `rustup`:
```bash
# Install rustup (interactive installer)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# Follow instructions. Ensure Rust >= 1.70 (check DESCRIPTION)
```

**Install `structr`:**

```R
# Requires 'remotes' package: install.packages("remotes")
remotes::install_github("ixpantia/structr")

# Or, once on CRAN:
# install.packages("structr")
```

## Quick Start

The workflow involves three steps:

1.  **Define Schema:** Use `s_*` functions.
2.  **Build Structure:** Call `build_structure()` on the schema.
3.  **Parse & Validate:** Call `parse_json()` with the JSON string and the built structure.

### Example 1: Simple Object

```R
library(structr)

# 1. Define Schema
user_schema <- s_map(
  id = s_integer(),
  username = s_string(),
  is_active = s_logical()
)

# 2. Build Structure
user_structure <- build_structure(user_schema)

# 3. Parse Valid JSON
valid_json <- '{"id": 123, "username": "testuser", "is_active": true}'
parsed_data <- parse_json(valid_json, user_structure)

print(parsed_data)
# Output: list(id = 123L, username = "testuser", is_active = TRUE)
str(parsed_data)
# Output: Shows correct R types (integer, character, logical)
```

### Example 2: Nesting and Optional Fields

```R
# 1. Define Schema
product_schema <- s_map(
  product_id = s_string(),
  details = s_map(name = s_string(), price = s_double()),
  description = s_optional(s_string()) # Can be string or null
)

# 2. Build Structure
product_structure <- build_structure(product_schema)

# 3. Parse JSON (description present)
json_with_desc <- '{
  "product_id": "XYZ-123",
  "details": {"name": "Gadget", "price": 19.99},
  "description": "A useful gadget"
}'
parsed1 <- parse_json(json_with_desc, product_structure)
print(parsed1$description)
# Output: [1] "A useful gadget"

# 4. Parse JSON (description is null)
json_null_desc <- '{
  "product_id": "ABC-789",
  "details": {"name": "Widget", "price": 9.50},
  "description": null
}'
parsed2 <- parse_json(json_null_desc, product_structure)
print(parsed2$description)
# Output: NULL
```

### Example 3: Validation Error

```R
# Using user_structure from Example 1

# Type mismatch: id should be integer, not string
json_type_error <- '{ "id": "123", "username": "badtype", "is_active": true }'
try(parse_json(json_type_error, user_structure))
# Output: Error: invalid type: string "123", expected an integer at line 1 column 13

# Missing field: is_active is required
json_missing_field <- '{ "id": 456, "username": "incomplete" }'
try(parse_json(json_missing_field, user_structure))
# Output: Error: missing field `is_active`
```

## License

MIT License. See [LICENSE](LICENSE.md) file.

## Contributing

Bug reports and contributions are welcome! Please submit issues or pull requests on [GitHub](https://github.com/ixpantia/structr).

## Author

Andres Quintero <andres@ixpantia.com>
