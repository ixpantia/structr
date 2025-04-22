# structr: Parse and Validate JSON Data Against a Predefined Structure

## Overview

`structr` provides a robust and efficient way to parse JSON data in R while strictly validating it against a user-defined schema. It ensures that incoming JSON data not only conforms to expected structures (objects, arrays, nesting) but also adheres to specific data types (integer, double, string, logical, optional/nullable).

The core parsing and validation logic is implemented in Rust using the highly performant `serde` and `simd_json` crates, accessed seamlessly from R via the `{extendr}` framework. This approach offers significant speed advantages over traditional R-based JSON parsing and validation, especially for large or complex JSON inputs.

**Key Features:**

*   **Schema Definition:** Define expected JSON structures using intuitive R functions (`s_map`, `s_vector`, `s_integer`, `s_double`, `s_string`, `s_logical`, `s_optional`).
*   **Strict Validation:** Ensures type correctness, presence of required fields, absence of disallowed extra fields (by default), and correct nesting.
*   **Performance:** Leverages Rust and `simd_json` for fast streaming parsing and validation.
*   **Detailed Errors:** Provides informative error messages pinpointing the location and nature of validation failures (e.g., type mismatch, missing field, invalid syntax, integer overflow).
*   **Optional Types:** Supports JSON `null` values for fields defined using `s_optional()`.

## Features

- [x] Define JSON schemas using R functions
- [x] Validate JSON data against the defined schema
- [x] Parse JSON data into R objects
- [x] Support for nested structures and arrays
- [x] Detailed error messages for validation failures
- [x] Support for optional fields (nullable)
- [ ] Support for serializing R objects back to JSON (planned for future versions)
- [ ] Support for custom validation functions (planned for future versions)
- [ ] Support for more complex data types (e.g., dates, enums)

## Installation

### Prerequisites: Rust Toolchain

`structr` uses Rust code internally. Therefore, you need the Rust compiler (`rustc`) and package manager (`cargo`) installed on your system. The easiest way to get them is via `rustup`:

```bash
# Install rustup (interactive installer)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# Follow the on-screen instructions

# Alternatively, use system package managers (might be older versions):
# Debian/Ubuntu: sudo apt-get update && sudo apt-get install cargo rustc
# Fedora: sudo dnf install cargo rustc
# macOS (Homebrew): brew install rust
```

Ensure your Rust version meets the minimum requirement specified in the `DESCRIPTION` file (currently Rust >= 1.65).

### Installing `structr`

Once the Rust toolchain is set up, you can install `structr`.

```r
# Currently under development, install from GitHub (replace with actual path if needed):
# install.packages("remotes") # If you don't have remotes installed
remotes::install_github("ixpantia/structr") # Replace with the correct repo path

# Once on CRAN:
# install.packages("structr")
```

## Core Concepts

Using `structr` involves three main steps:

1.  **Define the Schema:** Use the `s_*` functions (`s_map`, `s_vector`, `s_integer`, etc.) to create a blueprint of the expected JSON structure and types. This is represented as a nested R list.
2.  **Build the Structure:** Pass your schema definition to `build_structure()`. This function validates your schema definition itself and prepares an internal representation optimized for the Rust backend.
3.  **Parse and Validate:** Call `parse_json()` with the JSON string and the *built structure* object from the previous step. The function will parse the JSON, validate it against the structure, and return the corresponding R object (usually nested lists) if successful, or throw a detailed error if validation fails.

## Usage

### Example 1: Simple Object Validation

```R
library(structr)

# 1. Define the schema for a user object
user_schema <- s_map(
  id = s_integer(),
  username = s_string(),
  is_active = s_logical(),
  scores = s_vector(s_double()) # A vector where each element must be a double
)

# 2. Build the structure representation
user_structure <- build_structure(user_schema)
# This step validates your schema definition

# 3. Parse valid JSON
valid_json <- '{
  "id": 123,
  "username": "testuser",
  "is_active": true,
  "scores": [9.5, 8.0, 10.0]
}'

parsed_data <- parse_json(valid_json, user_structure)

print(parsed_data)
# Output:
# list(id = 123L, username = "testuser", is_active = TRUE, scores = list(9.5, 8, 10))

str(parsed_data)
# Output:
# List of 4
#  $ id       : int 123
#  $ username : chr "testuser"
#  $ is_active: logi TRUE
#  $ scores   :List of 3
#   ..$ : num 9.5
#   ..$ : num 8
#   ..$ : num 10
```

### Example 2: Nested Structures and Optional Fields

```R
# 1. Define schema with nesting and an optional description
product_schema <- s_map(
  product_id = s_string(),
  details = s_map(
    name = s_string(),
    price = s_double()
  ),
  tags = s_vector(s_string()),
  description = s_optional(s_string()) # This field can be a string or null
)

# 2. Build the structure
product_structure <- build_structure(product_schema)

# 3. Parse JSON where optional field is present
json_with_desc <- '{
  "product_id": "XYZ-123",
  "details": {"name": "Gadget", "price": 19.99},
  "tags": ["tech", "new"],
  "description": "A useful gadget"
}'
parsed1 <- parse_json(json_with_desc, product_structure)
print(parsed1$description)
# Output: [1] "A useful gadget"

# 4. Parse JSON where optional field is null
json_null_desc <- '{
  "product_id": "ABC-789",
  "details": {"name": "Widget", "price": 9.50},
  "tags": [],
  "description": null
}'
parsed2 <- parse_json(json_null_desc, product_structure)
print(parsed2$description)
# Output: NULL

# Note: `s_optional` means the value can be null, *not* that the field can be absent.
# If a field defined in s_map (even if optional) is missing from the JSON,
# it will still cause a "missing field" error by default.
json_missing_desc <- '{
  "product_id": "DEF-456",
  "details": {"name": "Thingy", "price": 5.00},
  "tags": ["misc"]
}'
try(parse_json(json_missing_desc, product_structure))
# Output: Error: missing field `description`
```

### Example 3: Handling Validation Errors

`structr` provides informative errors when JSON doesn't match the structure.

```R
# Using user_structure from Example 1

# Type mismatch: id should be integer, not string
json_type_error <- '{ "id": "123", "username": "badtype", "is_active": true, "scores": [] }'
try(parse_json(json_type_error, user_structure))
# Expected Output: Error: invalid type: string "123", expected an integer for field `id`

# Missing field: username is required
json_missing_field <- '{ "id": 456, "is_active": false, "scores": [1.1] }'
try(parse_json(json_missing_field, user_structure))
# Expected Output: Error: missing field `username`

# Extra field: email is not defined in the schema (and ignore_extra_fields is FALSE by default)
json_extra_field <- '{
  "id": 789, "username": "extra", "is_active": true, "scores": [], "email": "a@b.com"
}'
try(parse_json(json_extra_field, user_structure))
# Expected Output: Error: unknown field `email`, expected one of `id`, `username`, `is_active`, `scores`

# Invalid JSON syntax
json_syntax_error <- '{ "id": 1 '
try(parse_json(json_syntax_error, user_structure))
# Expected Output: Error: EOF while parsing an object at line 1 column 8
```

## API Reference

### Structure Definition (`s_*` functions)

These functions create the building blocks for your schema definition.

*   `s_map(...)`: Defines a JSON object structure. Takes named arguments where names are expected JSON keys and values are other `s_*` definitions.
    *   `s_map(..., .ignore_extra_fields = TRUE)`: Allows extra fields in the JSON object (they will be ignored). Default is `FALSE`.
*   `s_vector(element_structure)`: Defines a JSON array where all elements must conform to the `element_structure` (another `s_*` definition).
*   `s_integer()`: Expects a JSON integer (within R's 32-bit signed range). Accepts whole number floats (e.g., `123.0`).
*   `s_double()`: Expects any JSON number (integer or float).
*   `s_string()`: Expects a JSON string.
*   `s_logical()`: Expects a JSON boolean (`true` or `false`).
*   `s_optional(structure_definition)`: Allows the JSON value to be either `null` or conform to the `structure_definition`.

### Structure Building

*   `build_structure(schema_definition)`: Validates the schema definition created by `s_*` functions and prepares it for use with `parse_json()`. Returns a structure object.

### Parsing and Validation

*   `parse_json(json_string, structure)`: Parses the `json_string` according to the rules defined in the `structure` object (returned by `build_structure()`). Returns an R list representing the parsed JSON on success, or throws an error on failure.

## System Requirements

*   R (>= 4.2 recommended)
*   Rust Toolchain: `cargo` (Rust's package manager) and `rustc` (Rust compiler). Version >= 1.65 required.

## License

This package is licensed under the MIT License. <!-- Adjust if using GPL3 or other -->

## Author

Andres Quintero <andres@ixpantia.com>
