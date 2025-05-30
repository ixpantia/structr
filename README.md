
<!-- README.md is generated from README.Rmd. Please edit that file -->

# structr: Fast, Validating JSON Parsing and Serialization for R

<!-- badges: start -->

[![R-CMD-check](https://github.com/ixpantia/structr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ixpantia/structr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- Add build status/codecov badges here later --> <!-- badges: end -->

## Overview

`structr` is a high-performance R package designed for strict JSON
parsing and serialization with schema validation. Whether you’re working
with APIs, databases, or complex nested JSON, `structr` ensures that
your data conforms exactly to the required structure—both when reading
and writing.

Powered by Rust (`serde`, `simd_json`) and exposed to R via `{extendr}`,
it provides significant speed advantages over traditional R JSON tools
while offering more precise control over data structure and types.

## Features

- **Schema Definition in R:** Use expressive R functions like `s_map`,
  `s_vector`, `s_integer`, `s_double`, `s_string`, `s_logical`,
  `s_optional`, and `s_date`.
- **Strict Validation:** Catch type mismatches, missing fields,
  unexpected fields, and NULLs with clear error messages.
- **High Performance:** Combines validation with parsing for efficient
  data ingestion.
- **Bidirectional Support:** Parse JSON *and* serialize R data back to
  JSON, validating against the same schema.

## Planned Features

- Support for enums and more complex/custom validations.
- Better date/time parsing and formatting options.
- Clear, location-aware error messages help identify problems early.

## Installation

### Prerequisites

You must have Rust (`rustc`, `cargo`) installed. You can install via
[rustup](https://rustup.rs):

``` bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Make sure Rust \>= 1.70.

### Install from GitHub

``` r
# Install 'remotes' if needed
install.packages("remotes")
remotes::install_github("ixpantia/structr")
```

## Quick Start

### Define a Schema

``` r
library(structr)

schema <- s_map(
  x = s_integer(),
  y = s_double(),
  z = s_optional(s_string())
)

compiled <- build_structure(schema)
```

### Parse JSON

``` r
json <- '{"x": 1, "y": 99.9, "z": "value"}'
parse_json(json, compiled)
#> $x
#> [1] 1
#> 
#> $y
#> [1] 99.9
#> 
#> $z
#> [1] "value"
```

### Serialize JSON

``` r
r_data <- list(x = 1L, y = 99.9, z = "value")
serialize_json(r_data, compiled, pretty = TRUE)
#> {
#>   "y": 99.9,
#>   "z": "value",
#>   "x": 1
#> }
```

## Error Handling

Invalid input raises clear, structured errors:

``` r
bad_data <- '{"x": "not-an-int", "y": 10.5, "z": "ok"}'
try(parse_json(bad_data, compiled))
#> Error in parse_json(bad_data, compiled) : ExpectedSigned at character 0
```

## Handling Optional and Missing Fields

All fields are required by default. Use `s_optional()` to allow null or
missing values.

``` r
parse_json('{"x": 1, "y": 99.9}', compiled) # OK
#> $x
#> [1] 1
#> 
#> $y
#> [1] 99.9
try(parse_json('{"x": null, "y": 99.9}', compiled)) # Error
#> Error in parse_json("{\"x\": null, \"y\": 99.9}", compiled) : 
#>   ExpectedSigned at character 0
```

## Advanced Example: Nested and Complex Structures

``` r
order_schema <- s_map(
  order_id = s_string(),
  order_date = s_date(format = "%Y-%m-%d"),
  customer = s_map(
    customer_id = s_integer(),
    name = s_string(),
    email = s_optional(s_string())
  ),
  items = s_vector(s_map(
    sku = s_string(),
    product_name = s_string(),
    quantity = s_integer(),
    unit_price = s_double()
  )),
  shipping_address = s_map(
    street = s_string(),
    city = s_string(),
    zip_code = s_string(),
    country = s_string()
  ),
  notes = s_optional(s_string())
)

compiled_order <- build_structure(order_schema)
```

``` r
# Parse or serialize safely
order_json <- '{"order_id":"ORD-1","order_date":"2024-01-01","customer":{"customer_id":1,"name":"Alice"},"items":[],"shipping_address":{"street":"1 Main","city":"City","zip_code":"00000","country":"USA"}}'
parsed <- parse_json(order_json, compiled_order)

serialize_json(parsed, compiled_order, pretty = TRUE)
#> {
#>   "customer": {
#>     "email": null,
#>     "customer_id": 1,
#>     "name": "Alice"
#>   },
#>   "items": [],
#>   "notes": null,
#>   "order_id": "ORD-1",
#>   "shipping_address": {
#>     "zip_code": "00000",
#>     "city": "City",
#>     "country": "USA",
#>     "street": "1 Main"
#>   },
#>   "order_date": "2024-01-01"
#> }
```

## Strict Serialization Example

``` r
log_schema <- s_map(
  eventId = s_string(),
  eventDate = s_date("%Y%m%d"),
  eventType = s_string(),
  userId = s_integer(),
  details = s_map(
    source = s_string(),
    metadata = s_optional(s_map(
      key = s_optional(s_string()),
      value = s_optional(s_double())
    ))
  )
)

compiled_log <- build_structure(log_schema)

r_log <- list(
  eventId = "evt-001",
  eventDate = as.Date("2024-08-01"),
  eventType = "login",
  userId = 123L,
  details = list(
    source = "web",
    metadata = list(key = "duration", value = 320.5)
  )
)

serialize_json(r_log, compiled_log, pretty = TRUE)
#> {
#>   "eventDate": "20240801",
#>   "eventType": "login",
#>   "userId": 123,
#>   "eventId": "evt-001",
#>   "details": {
#>     "metadata": {
#>       "value": 320.5,
#>       "key": "duration"
#>     },
#>     "source": "web"
#>   }
#> }
```

## License

MIT License. See [LICENSE](LICENSE.md).

## Contributing

Please file issues or submit PRs at
<https://github.com/ixpantia/structr>.

## Author

Andres Quintero <andres@ixpantia.com>
