# ./tests/testthat/test-parse_json.R

nameless_list <- function() {
  v <- list()
  names(v) <- character(0)
  v
}

# --- Test Setup ---
# Define some common structures to reuse
simple_map_struct <- build_structure(s_map(name = s_string(), value = s_integer()))
nested_struct <- build_structure(
  s_map(
    id = s_integer(),
    user = s_map(
      username = s_string(),
      active = s_logical()
    ),
    tags = s_vector(s_string()),
    scores = s_vector(s_double())
  )
)
vector_of_maps_struct <- build_structure(
  s_vector(
    s_map(item_id = s_integer(), label = s_string())
  )
)


test_that("Parses simple atomic types correctly", {
  expect_equal(parse_json("123", build_structure(s_integer())), 123L)
  expect_equal(parse_json("-45", build_structure(s_integer())), -45L)
  expect_equal(parse_json("123.45", build_structure(s_double())), 123.45)
  expect_equal(parse_json("-0.5e-3", build_structure(s_double())), -0.0005)
  # Integers can be parsed as doubles
  expect_equal(parse_json("123", build_structure(s_double())), 123.0)
  # Whole number floats can be parsed as integers
  expect_equal(parse_json("123.0", build_structure(s_integer())), 123L)
  expect_equal(parse_json("\"hello world\"", build_structure(s_string())), "hello world")
  expect_equal(parse_json("\"\"", build_structure(s_string())), "") # Empty string
  expect_equal(parse_json("true", build_structure(s_logical())), TRUE)
  expect_equal(parse_json("false", build_structure(s_logical())), FALSE)
})

test_that("Parses simple vectors correctly", {
  expect_equal(parse_json("[1, 2, 3]", build_structure(s_vector(s_integer()))), list(1L, 2L, 3L))
  expect_equal(parse_json("[]", build_structure(s_vector(s_string()))), list()) # Empty vector
  expect_equal(parse_json("[\"a\", \"b\"]", build_structure(s_vector(s_string()))), list("a", "b"))
  expect_equal(parse_json("[1.1, 2.2, 3.0]", build_structure(s_vector(s_double()))), list(1.1, 2.2, 3.0))
  # Mixed integer/float source data for double vector
  expect_equal(parse_json("[1, 2.5, 3]", build_structure(s_vector(s_double()))), list(1.0, 2.5, 3.0))
  # Whole number floats source data for integer vector
  expect_equal(parse_json("[1.0, 2.0, 3.0]", build_structure(s_vector(s_integer()))), list(1L, 2L, 3L))
})

test_that("Parses simple maps correctly", {
  json_str <- '{"name": "test", "value": 99}'
  expected <- list(name = "test", value = 99L)
  # Order might differ in list, sort names for comparison
  result <- parse_json(json_str, simple_map_struct)
  expect_equal(result[order(names(result))], expected[order(names(expected))])

  # Empty map
  expect_equal(parse_json("{}", build_structure(s_map())), nameless_list())
})

test_that("Parses nested structures correctly", {
  json_str <- '{
    "id": 101,
    "user": { "username": "alice", "active": true },
    "tags": ["R", "Rust", "JSON"],
    "scores": [9.5, 8.8]
  }'
  expected <- list(
    id = 101L,
    user = list(username = "alice", active = TRUE),
    tags = list("R", "Rust", "JSON"),
    scores = list(9.5, 8.8)
  )
  # Order might differ, sort names recursively? Simpler to check components
  result <- parse_json(json_str, nested_struct)

  expect_equal(result$id, expected$id)
  expect_equal(result$user[order(names(result$user))], expected$user[order(names(expected$user))])
  expect_equal(result$tags, expected$tags)
  expect_equal(result$scores, expected$scores)
})

test_that("Parses vector of maps correctly", {
  json_str <- '[
    {"item_id": 1, "label": "apple"},
    {"item_id": 2, "label": "banana"}
  ]'
  expected <- list(
    list(item_id = 1L, label = "apple"),
    list(item_id = 2L, label = "banana")
  )
  result <- parse_json(json_str, vector_of_maps_struct)
  # Need to compare element-wise, potentially sorting map keys
  expect_equal(length(result), length(expected))
  for(i in seq_along(result)) {
     expect_equal(result[[i]][order(names(result[[i]]))], expected[[i]][order(names(expected[[i]]))])
  }

  # Empty vector of maps
  expect_equal(parse_json("[]", vector_of_maps_struct), list())
})

test_that("Handles JSON number types flexibly within structure constraints", {
  # Integer in JSON parsed as R integer
  expect_equal(parse_json("100", build_structure(s_integer())), 100L)
  # Integer in JSON parsed as R double
  expect_equal(parse_json("100", build_structure(s_double())), 100.0)
  # Float in JSON parsed as R double
  expect_equal(parse_json("100.5", build_structure(s_double())), 100.5)
  # Float without fractional part parsed as R integer
  expect_equal(parse_json("100.0", build_structure(s_integer())), 100L)
  # Float without fractional part parsed as R double
  expect_equal(parse_json("100.0", build_structure(s_double())), 100.0)
})

test_that("Throws error on invalid JSON syntax", {
  expect_error(parse_json("{", simple_map_struct))
  expect_error(parse_json("[1, 2", simple_map_struct))
  expect_error(parse_json("[1, 2]", build_structure(s_integer()))) # Valid syntax, wrong top-level type
  expect_error(parse_json("{\"name\": \"test\", \"value\": }", simple_map_struct))
  expect_error(parse_json("{\"name\": \"test\" \"value\": 1}", simple_map_struct))
  expect_error(parse_json("abc", build_structure(s_string()))) # Not quoted string
  expect_error(parse_json("null", build_structure(s_string()))) # Null is valid JSON but wrong type
})

test_that("Throws error on type mismatches", {
  # Atomic mismatches
  expect_error(parse_json("\"abc\"", build_structure(s_integer())))
  expect_error(parse_json("123", build_structure(s_string())))
  expect_error(parse_json("true", build_structure(s_double())))
  expect_error(parse_json("123.45", build_structure(s_logical())))
  # Float with fractional part for integer field
  expect_error(parse_json("12.5", build_structure(s_integer())))

  # Compound mismatches
  expect_error(parse_json("{}", build_structure(s_vector(s_integer()))))
  expect_error(parse_json("[]", simple_map_struct))

  # Mismatch within vector
  expect_error(parse_json("[1, \"2\", 3]", build_structure(s_vector(s_integer()))))
  expect_error(parse_json("[1, 2, true]", build_structure(s_vector(s_integer()))))

  # Mismatch within map
  expect_error(parse_json('{"name": 123, "value": 99}', simple_map_struct))
  expect_error(parse_json('{"name": "test", "value": "99"}', simple_map_struct))

  # Mismatch within nested map
  expect_error(parse_json('{"id": 1, "user": {"username": "x", "active": "yes"}}', nested_struct))
})

test_that("Throws error on missing required fields", {
  expect_error(parse_json('{"name": "test"}', simple_map_struct))
  expect_error(parse_json('{"id": 1, "tags": [], "scores": []}', nested_struct))
  # Missing field within nested map
  expect_error(parse_json('{"id": 1, "user": {"username": "x"}, "tags": [], "scores": []}', nested_struct))
})

test_that("Throws error on unknown/extra fields (default behavior)", {
  # Currently assumes ignore_extra_fields = FALSE is default/only behavior
  expect_error(parse_json('{"name": "test", "value": 99, "extra": true}', simple_map_struct))
  expect_error(parse_json('{"id": 1, "user": {"username": "x", "active": true, "admin": false}, "tags": [], "scores": []}', nested_struct))
})

test_that("Throws error on integer overflow for R's 32-bit integer", {
  # R's max integer is 2147483647
  max_int_plus_1 <- "2147483648"
  min_int_minus_1 <- "-2147483649"
  expect_error(parse_json(max_int_plus_1, build_structure(s_integer())))
  expect_error(parse_json(min_int_minus_1, build_structure(s_integer())))

  # Should work as double
  expect_equal(parse_json(max_int_plus_1, build_structure(s_double())), 2147483648)
})

test_that("Handles duplicate keys in JSON object", {
  # serde_json default behavior is to use the *last* value seen.
  # The Rust visitor implementation adds a check for duplicates.
  json_str <- '{"name": "first", "value": 1, "name": "last"}'
  expect_error(parse_json(json_str, simple_map_struct))
})

# Optional: Add tests for ignore_extra_fields = TRUE if implemented
# context("JSON Parsing - Ignore Extra Fields")
# test_that("Ignores extra fields when requested", {
#   struct_ignore <- build_structure(s_map(name = s_string(), .ignore_extra = TRUE)) # Hypothetical syntax
#   json_extra <- '{"name": "test", "extra": 123}'
#   expect_equal(parse_json(json_extra, struct_ignore), list(name = "test"))
# })
