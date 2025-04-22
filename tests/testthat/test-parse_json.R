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
  expect_equal(parse_json("123", build_structure(s_double())), 123.0)
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
  expect_equal(parse_json("[1.0, 2.0, 3.0]", build_structure(s_vector(s_double()))), list(1, 2, 3))
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

test_that("Handles optional atomic types correctly", {
  # Optional string
  s_opt_str <- build_structure(s_optional(s_string()))
  expect_equal(parse_json("\"hello\"", s_opt_str), "hello")
  expect_equal(parse_json("null", s_opt_str), NULL)
  expect_error(parse_json("123", s_opt_str)) # Wrong type when present

  # Optional integer
  s_opt_int <- build_structure(s_optional(s_integer()))
  expect_equal(parse_json("123", s_opt_int), 123L)
  expect_equal(parse_json("null", s_opt_int), NULL)
  expect_error(parse_json("\"123\"", s_opt_int)) # Wrong type when present
  expect_error(parse_json("12.5", s_opt_int))   # Wrong type (float) when present

  # Optional logical
  s_opt_log <- build_structure(s_optional(s_logical()))
  expect_equal(parse_json("true", s_opt_log), TRUE)
  expect_equal(parse_json("null", s_opt_log), NULL)
  expect_error(parse_json("\"true\"", s_opt_log)) # Wrong type when present
})

test_that("Handles optional fields within maps correctly", {
  s_map_opt <- build_structure(s_map(
    required_field = s_string(),
    optional_field = s_optional(s_integer())
  ))

  # Both present
  json1 <- '{"required_field": "abc", "optional_field": 10}'
  expected1 <- list(required_field = "abc", optional_field = 10L)
  result1 <- parse_json(json1, s_map_opt)
  expect_equal(result1[order(names(result1))], expected1[order(names(expected1))])

  # Optional field is null
  json2 <- '{"required_field": "def", "optional_field": null}'
  expected2 <- list(required_field = "def", optional_field = NULL)
  result2 <- parse_json(json2, s_map_opt)
  expect_equal(result2[order(names(result2))], expected2[order(names(expected2))])

  # Error: Optional field present but wrong type
  json3 <- '{"required_field": "ghi", "optional_field": "10"}'
  expect_error(parse_json(json3, s_map_opt))

  # Error: Required field is missing
  json4 <- '{"optional_field": 10}'
  expect_error(parse_json(json4, s_map_opt))

  # Error: Required field is null (not allowed unless explicitly optional)
  json5 <- '{"required_field": null, "optional_field": 10}'
  s_map_req_not_opt <- build_structure(s_map(required_field = s_string()))
  expect_error(parse_json('{"required_field": null}', s_map_req_not_opt))
})

test_that("Handles optional vectors correctly", {
  # An optional vector (the whole vector can be null)
  s_opt_vec <- build_structure(s_optional(s_vector(s_string())))
  expect_equal(parse_json('["a", "b"]', s_opt_vec), list("a", "b"))
  expect_equal(parse_json("null", s_opt_vec), NULL)
  expect_error(parse_json('["a", 1]', s_opt_vec)) # Wrong element type when present
  expect_error(parse_json('{"a": 1}', s_opt_vec)) # Wrong container type when present

  # A vector containing optional elements (each element can be null)
  s_vec_opt_el <- build_structure(s_vector(s_optional(s_integer())))
  expect_equal(parse_json('[1, null, 3, null]', s_vec_opt_el), list(1L, NULL, 3L, NULL))
  expect_equal(parse_json('[1, 2, 3]', s_vec_opt_el), list(1L, 2L, 3L))
  expect_equal(parse_json('[null, null]', s_vec_opt_el), list(NULL, NULL))
  expect_equal(parse_json('[]', s_vec_opt_el), list())
  expect_error(parse_json('[1, "a", 3]', s_vec_opt_el)) # Wrong element type when present
})


test_that("Handles optional maps correctly", {
  # An optional map (the whole map can be null)
  s_opt_map <- build_structure(s_optional(s_map(key = s_string())))
  expect_equal(parse_json('{"key": "value"}', s_opt_map), list(key = "value"))
  expect_equal(parse_json("null", s_opt_map), NULL)
  expect_error(parse_json('{"key": 123}', s_opt_map))
  expect_error(parse_json('[]', s_opt_map))
})

test_that("Handles nested optional types", {
    # Optional map containing an optional field
    s_nested_opt <- build_structure(
        s_optional(
            s_map(
                opt_val = s_optional(s_double())
            )
        )
    )

    # Outer map is present, inner value is present
    expect_equal(parse_json('{"opt_val": 1.5}', s_nested_opt), list(opt_val = 1.5))
    # Outer map is present, inner value is null
    expect_equal(parse_json('{"opt_val": null}', s_nested_opt), list(opt_val = NULL))
    # Outer map is null
    expect_equal(parse_json('null', s_nested_opt), NULL)
    # Outer map present, inner optional field key is missing (error)
    expect_equal(parse_json('{}', s_nested_opt), nameless_list())

    # Error: Outer map present, inner value wrong type
    expect_error(parse_json('{"opt_val": "1.5"}', s_nested_opt))
    # Error: Outer map wrong type
    expect_error(parse_json('[]', s_nested_opt))


    # Vector containing optional maps, which have optional fields
    s_vec_opt_map_opt_field <- build_structure(
        s_vector(
            s_optional( # Each element in the vector can be null OR a map
                s_map(
                    id = s_integer(),                 # Required field in map
                    desc = s_optional(s_string())     # Optional field in map
                )
            )
        )
    )
    # Valid nested JSON adhering to strict optionality (keys present or element null)
    json_nested_strict <- '[
        {"id": 1, "desc": "first"},
        null,
        {"id": 2, "desc": null},
        {"id": 3, "desc": "third"}
    ]'
    expected_nested_strict <- list(
        list(id = 1L, desc = "first"),
        NULL,
        list(id = 2L, desc = NULL),
        list(id = 3L, desc = "third")
    )
    result_nested <- parse_json(json_nested_strict, s_vec_opt_map_opt_field)

    # Compare element-wise after sorting map keys within each element
    expect_equal(length(result_nested), length(expected_nested_strict))
    for(i in seq_along(result_nested)) {
      if(is.list(result_nested[[i]])) {
        expect_equal(result_nested[[i]][order(names(result_nested[[i]]))],
                     expected_nested_strict[[i]][order(names(expected_nested_strict[[i]]))])
      } else {
        expect_equal(result_nested[[i]], expected_nested_strict[[i]]) # Compare NULLs directly
      }
    }

    # Error: Element is map, but optional field KEY is missing
    json_nested_err_key_miss <- '[{"id": 1, "desc": "ok"}, {"id": 2}]' # second element misses 'desc' key
    res <- parse_json(json_nested_err_key_miss, s_vec_opt_map_opt_field)
    expect_mapequal(res[[1]], list(id = 1L, desc = "ok"))
    expect_mapequal(res[[2]], list(id = 2L))

    # Error: Element is map, but required field KEY is missing
    json_nested_err_req_key_miss <- '[{"id": 1, "desc": "ok"}, {"desc": "no id"}]'
    expect_error(parse_json(json_nested_err_req_key_miss, s_vec_opt_map_opt_field))

    # Error: Element is map, optional field present but wrong type
    json_nested_err_type <- '[{"id": 1, "desc": 123}]'
    expect_error(parse_json(json_nested_err_type, s_vec_opt_map_opt_field))

})
