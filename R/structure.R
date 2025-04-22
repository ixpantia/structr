# --- File: R/structure.R ---
#' Define Atomic Structure Types
#'
#' @description
#' These functions create intermediate definitions for basic JSON atomic types
#' (integer, number/double, string, boolean). These definitions are used within
#' `s_vector()` or `s_map()` and finalized by `build_structure()`.
#'
#' @details
#' \itemize{
#'   \item `s_integer()`: Expects a JSON integer that fits within R's 32-bit signed integer range. Allows JSON numbers without fractional parts (e.g., `123.0`).
#'   \item `s_double()`: Expects any JSON number (integer or float).
#'   \item `s_string()`: Expects a JSON string.
#'   \item `s_logical()`: Expects a JSON boolean (`true` or `false`).
#' }
#'
#' @return An intermediate list representing the atomic type definition (e.g., `list(type = "integer")`).
#' @name structure_atomics
#' @export
#' @examples
#' build_structure(s_integer()) # list(type = "integer")
#' build_structure(s_double())  # list(type = "double")
#' build_structure(s_string())  # list(type = "string")
#' build_structure(s_logical()) # list(type = "logical")
s_integer <- function() {
  list(type = "integer")
}

#' @rdname structure_atomics
#' @export
s_double <- function() {
  list(type = "double")
}

#' @rdname structure_atomics
#' @export
s_string <- function() {
  list(type = "string")
}

#' @rdname structure_atomics
#' @export
s_logical <- function() {
  list(type = "logical")
}

#' Define a Vector (Array) Structure
#'
#' Creates an intermediate definition for a JSON array where all elements must conform
#' to the same specified `element_structure`. This definition is used within
#' `s_map()` or finalized by `build_structure()`.
#'
#' @param element_structure The structure definition for the elements within the
#'   vector. This should be the result of another `s_*` function call (e.g., `s_integer()`,
#'   `s_map(id = s_integer())`).
#'
#' @return An intermediate list representing the vector structure definition.
#' @export
#' @examples
#' # Define a vector of strings
#' vec_str_def <- s_vector(s_string())
#' build_structure(vec_str_def)
#' # Expected: list(type = "vector", value = list(type = "string"))
#'
#' # Define a vector of objects, each having an 'id' (integer) and 'name' (string)
#' vec_obj_def <- s_vector(
#'   s_map(id = s_integer(), name = s_string())
#' )
#' build_structure(vec_obj_def)
#' # Expected: list(type = "vector", value = list(type = "map",
#' #                   value = list(id = list(type = "integer"),
#' #                                name = list(type = "string"))))
#'
#' # Parsing example (see ?parse_json)
#' json_data <- '[{"id": 1, "name": "A"}, {"id": 2, "name": "B"}]'
#' parsed <- parse_json(json_data, build_structure(vec_obj_def))
#' print(parsed) # Will be a list of lists
s_vector <- function(element_structure) {
  # Input validation for element_structure could be added here,
  # but build_structure handles the final validation.
  if (
    missing(element_structure) ||
      !is.list(element_structure) ||
      is.null(element_structure$type)
  ) {
    stop(
      "`element_structure` must be a valid structure definition from an `s_*` function.",
      call. = FALSE
    )
  }
  # Store the user's definition directly; build_structure will process it recursively.
  list(type = "vector", value = element_structure)
}

#' Define a Map (Object) Structure using Named Arguments
#'
#' Creates an intermediate definition for a JSON object (map) with named fields and
#' their corresponding structure definitions. This definition is used within
#' `s_vector()` or finalized by `build_structure()`.
#'
#' @param ... Named arguments defining the map's structure. The names correspond
#'   to the required JSON object keys (strings). The values must be structure
#'   definitions created by `s_*` function calls (e.g., `id = s_integer()`,
#'   `tags = s_vector(s_string())`, `metadata = s_map(...)`). An empty map
#'   (representing an empty JSON object `{}`) can be created by calling `s_map()`
#'   with no arguments.
#' @param .ignore_extra_fields Logical (default `FALSE`). If `TRUE`, allows the JSON
#'   object to contain fields not specified in `...`. These extra fields will be
#'   ignored during parsing. If `FALSE` (default), extra fields in the JSON will
#'   cause a validation error. *Note: This argument is currently processed by `build_structure`.*
#'
#' @return An intermediate list representing the map structure definition.
#' @export
#' @examples
#' # Define a simple map with a string 'name' and an integer 'age'
#' map_def1 <- s_map(name = s_string(), age = s_integer())
#' build_structure(map_def1)
#' # Expected: list(type = "map", value = list(name = list(type = "string"),
#' #                                          age = list(type = "integer")),
#' #                ignore_extra_fields = FALSE)
#'
#' # Define a nested map
#' map_def2 <- s_map(
#'   user = s_string(),
#'   details = s_map(
#'     email = s_string(),
#'     active = s_logical()
#'   ),
#'   permissions = s_vector(s_string())
#' )
#' str(build_structure(map_def2)) # Use str() for compact view
#'
#' # Define an empty map
#' empty_map_def <- s_map()
#' build_structure(empty_map_def)
#' # Expected: list(type = "map", value = list(), ignore_extra_fields = FALSE)
#'
#' # Define a map ignoring extra fields
#' map_def_ignore <- s_map(required_field = s_integer(), .ignore_extra_fields = TRUE)
#' build_structure(map_def_ignore)
#' # Expected: list(type = "map", value = list(required_field = list(type = "integer")),
#' #                ignore_extra_fields = TRUE)
#'
#' # Parsing example (see ?parse_json)
#' json_data <- '{"name": "Example", "age": 42}'
#' parsed <- parse_json(json_data, build_structure(map_def1))
#' print(parsed) # list(name = "Example", age = 42L)
#'
#' json_extra <- '{"name": "Example", "age": 42, "extra": true}'
#' # This will error by default:
#' try(parse_json(json_extra, build_structure(map_def1)))
#' # This will succeed if using ignore_extra_fields = TRUE definition:
#' parsed_ignore <- parse_json(json_extra, build_structure(map_def_ignore))
#' # print(parsed_ignore) # list(required_field = ...) - requires matching json
s_map <- function(..., .ignore_extra_fields = FALSE) {
  # Capture the field definitions passed via ...
  map_fields <- list(...)

  # Initial validation: Ensure all arguments in ... are named (or the list is empty)
  # and that field names are unique.
  nms <- names(map_fields)
  is_named_or_empty <- length(map_fields) == 0 ||
    (!is.null(nms) && all(nzchar(nms)) && !anyDuplicated(nms))

  if (!is_named_or_empty) {
    stop(
      "All arguments passed to `s_map` via `...` must be uniquely named.",
      call. = FALSE
    )
  }

  # Basic validation for field values (must be lists from s_*). Deeper validation
  # happens in build_structure.
  is_valid_structure <- function(x) is.list(x) && !is.null(x$type)
  if (length(map_fields) > 0 && !all(sapply(map_fields, is_valid_structure))) {
    stop(
      "All values passed to `s_map` via `...` must be valid structure definitions from `s_*` functions.",
      call. = FALSE
    )
  }

  if (
    !is.logical(.ignore_extra_fields) ||
      length(.ignore_extra_fields) != 1 ||
      is.na(.ignore_extra_fields)
  ) {
    stop("`.ignore_extra_fields` must be TRUE or FALSE.", call. = FALSE)
  }

  # Return the intermediate structure specification
  list(
    type = "map",
    value = map_fields,
    ignore_extra_fields = .ignore_extra_fields # Store the flag
  )
}

#' Define an Optional (Nullable) Structure
#'
#' Creates an intermediate definition indicating that a JSON value can either conform
#' to the specified `structure_definition` or be JSON `null`. This is typically
#' used within `s_map()` to define fields that are not required to have a non-null
#' value.
#'
#' @param structure_definition The structure definition that the JSON value should conform
#'   to if it is *not* `null`. This should be the result of another `s_*` function
#'   call (e.g., `s_integer()`, `s_string()`, `s_map(...)`).
#'
#' @return An intermediate list representing the optional structure definition.
#' @export
#' @examples
#' # Define a map where 'description' is optional (can be string or null)
#' map_with_optional <- s_map(
#'   id = s_integer(),
#'   description = s_optional(s_string())
#' )
#' built_optional <- build_structure(map_with_optional)
#' str(built_optional)
#' # Expected (simplified):
#' # list(type = "map",
#' #      value = list(id = list(type = "integer"),
#' #                   description = list(type = "optional",
#' #                                      value = list(type = "string"))),
#' #      ...)
#'
#' # Define a vector where elements can be integers or null
#' vec_optional_elements <- s_vector(s_optional(s_integer()))
#' build_structure(vec_optional_elements)
#' # Expected (simplified):
#' # list(type = "vector",
#' #      value = list(type = "optional", value = list(type = "integer")))
#'
#' # --- Parsing Examples (see ?parse_json) ---
#'
#' # Field present and valid
#' json_present <- '{"id": 1, "description": "A product"}'
#' parse_json(json_present, built_optional)
#' # Output: list(id = 1L, description = "A product")
#'
#' # Optional field is null
#' json_null <- '{"id": 2, "description": null}'
#' parse_json(json_null, built_optional)
#' # Output: list(id = 2L, description = NULL)
#'
#' # Optional field is missing (this causes an error by default with maps)
#' # Note: Optionality here means "can be null", not "can be absent".
#' # The 'missing field' error takes precedence unless the field is truly absent
#' # from the structure definition itself (which isn't the case here).
#' json_missing <- '{"id": 3}'
#' try(parse_json(json_missing, built_optional))
#' # Expected: Error about missing field "description"
#'
#' # Parsing a vector with optional elements
#' json_vec_opt <- '[10, null, 30, null]'
#' parse_json(json_vec_opt, build_structure(vec_optional_elements))
#' # Output: list(10L, NULL, 30L, NULL)
s_optional <- function(structure_definition) {
  # Basic validation
  if (
    missing(structure_definition) ||
      !is.list(structure_definition) ||
      is.null(structure_definition$type)
  ) {
    stop(
      "`structure_definition` must be a valid structure definition from an `s_*` function.",
      call. = FALSE
    )
  }
  list(type = "optional", value = structure_definition)
}
s_optional <- function(x) {
  list(type = "optional", value = x)
}

#' Finalize and Validate a Structure Definition
#'
#' Processes a structure definition created using the `s_*` helper functions
#' (like `s_integer()`, `s_map()`, `s_vector()`) into a final, validated
#' representation required by the `parse_json()` function.
#'
#' @param x The structure definition created using `s_*` functions. This defines
#'   the expected schema of the JSON data (e.g., `s_map(id = s_integer())`).
#'
#' @return An object representing the finalized structure definition. This object
#'   is specifically formatted for use as the `structure` argument in the
#'   `parse_json()` function. The internal details of this object are not typically
#'   needed by the user.
#'
#' @details
#' This function serves two main purposes:
#' \enumerate{
#'   \item \strong{Validation}: It checks the user-provided structure definition for
#'     correctness *before* attempting to parse any JSON. This includes verifying
#'     that `s_map()` arguments are correctly named and use valid `s_*` definitions,
#'     that `s_vector()` has a valid `element_structure`, and that nesting is
#'     consistent. Errors in the definition (like duplicate field names in `s_map`)
#'     will be caught at this stage.
#'   \item \strong{Preparation}: It converts the user-friendly definition created
#'     with `s_*` functions into the specific internal format required by the
#'     efficient JSON parsing engine used in `parse_json()`.
#' }
#' You must call `build_structure()` on your schema definition before passing it
#' to `parse_json()`.
#'
#' @export
#' @examples
#' # 1. Define the desired JSON structure using s_* functions
#' my_schema_definition <- s_map(
#'   product_id = s_string(),
#'   quantity = s_integer(),
#'   in_stock = s_logical(),
#'   attributes = s_vector(s_string())
#' )
#'
#' # 2. Finalize and validate the definition
#' finalized_structure <- build_structure(my_schema_definition)
#' # finalized_structure is now ready to be used with parse_json()
#'
#' # Example with nested structures
#' complex_definition <- s_map(
#'   order_id = s_integer(),
#'   customer = s_map(
#'     name = s_string(),
#'     email = s_string()
#'   ),
#'   items = s_vector(
#'     s_map(
#'       sku = s_string(),
#'       price = s_double()
#'     )
#'   )
#' )
#'
#' validated_complex_structure <- build_structure(complex_definition)
#'
#' # Example of an invalid definition caught by build_structure
#' invalid_definition <- s_map(
#'    name = s_string(),
#'    name = s_integer() # Duplicate field name
#' )
#' # This will throw an error when build_structure is called:
#' try(build_structure(invalid_definition))
#'
#' # Another invalid example: missing element structure in s_vector
#' try(build_structure(s_vector()))
#'
#' # Use the built structure with parse_json (see ?parse_json examples)
#' json_data <- '{
#'   "product_id": "XYZ-123",
#'   "quantity": 5,
#'   "in_stock": true,
#'   "attributes": ["red", "large"]
#' }'
#' parsed_data <- parse_json(json_data, structure = finalized_structure)
#' print(parsed_data)
#'
build_structure <- function(x) {
  # Call the internal function (which might be Rust or R based on implementation)
  # to perform the conversion and validation.
  result <- Structure$convert_from_robj(x)

  # Check if the internal function indicated an error
  if (inherits(result, "error")) {
    # Re-throw the error using R's standard error mechanism for clarity
    rlang::abort(result$value)
  }

  # Return the successfully built structure object
  result
}
