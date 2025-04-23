json <- function(x) {
  structure(
    x,
    class = c("json")
  )
}

#' Serialize data to JSON
#' @description
#' This function serializes R data to JSON format using a specified structure.
#' The main purpose is to provide a type-safe way to convert R objects into JSON strings
#' that conform to a predefined structure.
#'
#' Since R is an array language, many typical serialization tasks can make
#' interoperability with other languages difficult. It is not uncommon for
#' an API to expect a scalar value, but R will serialize it as a length-1 vector.
#'
#' This function is designed to handle such cases by providing a structure
#' that defines the expected types and formats of the data. The structure is
#' defined using the `build_structure` function, which allows you to specify
#' the types of each field in the data. The function will then serialize the
#' data according to this structure, ensuring that the output is in the correct
#' format.
#'
#' @param data The data to serialize
#' @param structure The structure to use for serialization
#' @param pretty Whether to pretty-print the JSON
#' @return A JSON string
#' @export
#' @examples
#' # Example usage
#' data <- list(name = "Test", value = 123L, active = TRUE)
#' structure <- build_structure(s_map(
#'   name = s_string(),
#'   value = s_integer(),
#'   active = s_logical()
#' ))
#' json_string <- serialize_json(data, structure)
#' print(json_string)
serialize_json <- function(data, structure, pretty = FALSE) {
  result <- if (pretty) serialize_structure_pretty(structure, data) else
    serialize_structure(structure, data)

  if (inherits(result, "error")) {
    rlang::abort(result$value)
  }
  json(result)
}


#' @export
print.json <- function(x, ...) {
  cat(x, "\n")
}
