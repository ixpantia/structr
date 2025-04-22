#' Parse and Validate JSON Against a Structure
#'
#' Parses a JSON string and validates it against a schema created with `build_structure()`.
#' Returns an R object reflecting the JSON content if valid, or throws a detailed error.
#'
#' @param json_string A character string containing the JSON input.
#' @param structure A structure definition created with `build_structure()`, based on `s_*` functions.
#'
#' @return The parsed and validated data as an R object:
#' \itemize{
#'   \item JSON objects → named `list`s
#'   \item JSON arrays → unnamed `list`s
#'   \item Strings, numbers, booleans → scalar R values (character, numeric, integer, logical)
#' }
#' Errors are raised for invalid syntax, type mismatches, missing or extra fields, duplicate keys,
#' and integer overflows.
#'
#' @details
#' Validation checks include:
#' \itemize{
#'   \item **Types**: JSON values must match the declared structure (e.g., string vs integer).
#'   \item **Required fields**: All fields in `s_map()` must be present.
#'   \item **No extra fields**: Additional fields not in the structure cause errors (unless `.ignore_extra_fields = TRUE`).
#'   \item **Duplicate keys**: Disallowed.
#'   \item **Integer range**: Must fit within R's 32-bit integer limits for `s_integer()`.
#'   \item **Homogeneous arrays**: Elements in `s_vector()` must match the defined structure.
#' }
#'
#' Parsing is powered by `serde_json` in Rust for performance.
#'
#' @export
#'
#' @examples
#' # Define and build a structure
#' schema <- build_structure(s_map(
#'   id = s_integer(),
#'   username = s_string(),
#'   is_active = s_logical(),
#'   scores = s_vector(s_double())
#' ))
#'
#' # Valid input
#' json <- '{"id":1,"username":"user","is_active":true,"scores":[9.5,8.0]}'
#' parse_json(json, schema)
#'
#' # Common errors:
#' # - Invalid JSON
#' # - Wrong type (e.g., "id": "abc")
#' # - Missing fields
#' # - Unexpected extra fields
#' # - Integer overflow
#' # - Duplicate keys
parse_json <- function(json_string, structure) {
  # Call the Rust implementation function (via the wrapper)
  result <- parse_json_impl(json_string, structure)

  if (inherits(result, "error")) {
    rlang::abort(result$value)
  }

  result
}
