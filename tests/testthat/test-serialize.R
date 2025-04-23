NA_Date_ <- as.Date(NA)

compare_objects <- function(parsed_object, parsed_expected) {
  # Use testthat's comparison for R objects
  if (is.list(parsed_object) && is.list(parsed_expected)) {
    # if is a named list
    if (length(names(parsed_object)) > 0) {
      names_parsed_object <- names(parsed_object)
      names_parsed_expected <- names(parsed_expected)

      # Check if the names are the same
      expect_setequal(names_parsed_object, names_parsed_expected)

      purrr::iwalk(
        parsed_object,
        function(parsed_value, key) {
          expected_value <- parsed_expected[[key]]
          compare_objects(parsed_value, expected_value)
        }
      )
    } else {
      purrr::walk2(
        parsed_object,
        parsed_expected,
        compare_objects
      )
    }
  } else {
    expect_equal(parsed_object, parsed_expected)
  }
}

# Helper for testing map serialization where key order might differ
expect_json_equivalent <- function(object, expected, structure) {
  # Check if both are valid JSON
  parsed_object <- parse_json(object, structure)
  parsed_expected <- parse_json(expected, structure)

  compare_objects(parsed_object, parsed_expected)
}


test_that("Can serialize simple maps", {
  map_struct <- build_structure(s_map(
    name = s_string(),
    value = s_integer(),
    active = s_logical()
  ))
  map_struct_opt <- build_structure(s_map(
    id = s_integer(),
    description = s_optional(s_string())
  ))

  r_list1 <- list(name = "Test", value = 123L, active = TRUE)
  expected_json1 <- '{"name":"Test","value":123,"active":true}'
  # Use helper because Rust hashmap order isn't guaranteed
  expect_json_equivalent(
    serialize_json(r_list1, map_struct),
    expected_json1,
    map_struct
  )

  r_list2 <- list(id = 1L, description = "Optional value")
  expected_json2 <- '{"id":1,"description":"Optional value"}'
  expect_json_equivalent(
    serialize_json(r_list2, map_struct_opt),
    expected_json2,
    map_struct_opt
  )

  r_list3 <- list(id = 2L, description = NULL) # Use NULL for optional null
  expected_json3 <- '{"id":2,"description":null}'
  expect_json_equivalent(
    serialize_json(r_list3, map_struct_opt),
    expected_json3,
    map_struct_opt
  )

  # Empty map
  empty_map_struct <- build_structure(s_map())
  expect_equal(serialize_json(list(), empty_map_struct), json("{}"))
})

test_that("Can serialize nested structures", {
  nested_struct <- build_structure(s_map(
    id = s_integer(),
    details = s_map(
      type = s_string(),
      score = s_double()
    ),
    tags = s_vector(s_string())
  ))

  r_list_nested <- list(
    id = 101L,
    details = list(type = "complex", score = 99.9),
    tags = c("R", "Rust")
  )
  expected_json_nested <- '{
    "id":101,
    "details":{"type":"complex","score":99.9},
    "tags":["R","Rust"]
  }'
  # Remove whitespace for comparison, use helper for map order
  expect_json_equivalent(
    serialize_json(r_list_nested, nested_struct),
    expected_json_nested, # Minify to remove formatting
    nested_struct
  )

  # Nested vector of maps
  vec_map_struct <- build_structure(s_vector(
    s_map(item = s_string(), count = s_integer())
  ))
  r_list_vec_map <- list(
    list(item = "A", count = 10L),
    list(item = "B", count = 20L)
  )
  expected_json_vec_map <- '[{"item":"A","count":10},{"item":"B","count":20}]'
  expect_json_equivalent(
    serialize_json(r_list_vec_map, vec_map_struct),
    expected_json_vec_map,
    vec_map_struct
  )
})

test_that("Handles optional map fields and nested optionals correctly", {
  struct_opt_nested <- build_structure(s_map(
    req_field = s_integer(),
    opt_field = s_optional(s_string()),
    opt_map = s_optional(s_map(
      inner_opt = s_optional(s_logical())
    )),
    opt_vec_opt_elem = s_optional(s_vector(s_optional(s_integer())))
  ))

  # All present
  r_list1 <- list(
    req_field = 1L,
    opt_field = "hello",
    opt_map = list(inner_opt = TRUE),
    opt_vec_opt_elem = c(1L, NA, 3L)
  )
  expected_json1 <- '{
    "req_field":1,
    "opt_field":"hello",
    "opt_map":{"inner_opt":true},
    "opt_vec_opt_elem":[1,null,3]
  }'
  expect_json_equivalent(
    serialize_json(r_list1, struct_opt_nested),
    expected_json1,
    struct_opt_nested
  )

  # Various fields set to NULL (or NA for vector elements)
  r_list2 <- list(
    req_field = 2L,
    opt_field = NULL,
    opt_map = list(inner_opt = NULL),
    opt_vec_opt_elem = c(NA_integer_)
  )
  expected_json2 <- '{
    "req_field":2,
    "opt_field":null,
    "opt_map":{"inner_opt":null},
    "opt_vec_opt_elem":[null]
  }'

  expect_json_equivalent(
    serialize_json(r_list2, struct_opt_nested),
    expected_json2,
    struct_opt_nested
  )

  # Entire optional map/vector set to NULL
  r_list3 <- list(
    req_field = 3L,
    opt_field = "world",
    opt_map = NULL,
    opt_vec_opt_elem = NULL
  )
  expected_json3 <- '{
    "req_field":3,
    "opt_field":"world",
    "opt_map":null,
    "opt_vec_opt_elem":null
  }'
  expect_json_equivalent(
    serialize_json(r_list3, struct_opt_nested),
    expected_json3,
    struct_opt_nested
  )
})


test_that("Can serialize dates correctly", {
  date_struct <- build_structure(s_date(format = "%Y-%m-%d"))
  date_struct_custom <- build_structure(s_date(format = "%m/%d/%Y"))
  date_opt_struct <- build_structure(s_optional(s_date()))
  date_vec_struct <- build_structure(s_vector(s_date()))
  date_vec_opt_struct <- build_structure(s_vector(s_optional(s_date())))
  map_date_struct <- build_structure(s_map(
    event = s_string(),
    when = s_date("%Y%m%d"),
    expires = s_optional(s_date("%Y-%m-%d"))
  ))

  r_date <- as.Date("2024-07-16")

  # Simple date, default format
  expect_equal(serialize_json(r_date, date_struct), json('"2024-07-16"'))
  # Simple date, custom format
  expect_equal(serialize_json(r_date, date_struct_custom), json('"07/16/2024"'))
  # Optional date, present
  expect_equal(serialize_json(r_date, date_opt_struct), json('"2024-07-16"'))
  # Optional date, NULL
  # expect_equal(serialize_json(NULL, date_opt_struct), 'null') # Need to check top-level NULL handling
  # Optional date, NA
  expect_equal(serialize_json(as.Date(NA), date_opt_struct), json('null'))

  # Vector of dates
  r_dates <- c(as.Date("2024-01-01"), as.Date("2024-01-02"))
  expect_equal(
    serialize_json(r_dates, date_vec_struct),
    json('["2024-01-01","2024-01-02"]')
  )

  # Vector of optional dates
  r_dates_opt <- c(as.Date("2024-01-01"), NA, as.Date("2024-01-03"))
  expect_equal(
    serialize_json(r_dates_opt, date_vec_opt_struct),
    json('["2024-01-01",null,"2024-01-03"]')
  )
  expect_equal(
    serialize_json(c(NA_Date_, NA_Date_), date_vec_opt_struct),
    json('[null,null]')
  )

  # Map with dates
  r_map_dates <- list(
    event = "Launch",
    when = as.Date("20241031", "%Y%m%d"),
    expires = as.Date("2025-12-31")
  )
  expected_map_dates <- '{"event":"Launch","when":"20241031","expires":"2025-12-31"}'
  expect_json_equivalent(
    serialize_json(r_map_dates, map_date_struct),
    expected_map_dates,
    map_date_struct
  )

  # Map with optional date set to NA/NULL
  r_map_dates_null <- list(
    event = "Setup",
    when = as.Date("20240101", "%Y%m%d"),
    expires = NA_Date_
  )
  expected_map_dates_null <- '{"event":"Setup","when":"20240101","expires":null}'
  expect_json_equivalent(
    serialize_json(r_map_dates_null, map_date_struct),
    expected_map_dates_null,
    map_date_struct
  )
})

test_that("Serialization fails for type mismatches", {
  int_struct <- build_structure(s_integer())
  vec_int_struct <- build_structure(s_vector(s_integer()))
  map_int_struct <- build_structure(s_map(a = s_integer()))
  date_struct <- build_structure(s_date())

  # Atomic
  expect_error(serialize_json("hello", int_struct), "Type mismatch")
  expect_error(serialize_json(TRUE, int_struct), "Type mismatch")
  expect_error(serialize_json(1.5, int_struct), "Type mismatch") # Maybe? Depends on Rust handling

  # Vector Elements
  expect_error(serialize_json(c(1L, "2", 3L), vec_int_struct)) # R coerces, error likely at Rust level
  expect_error(serialize_json(c(1L, 2.5, 3L), vec_int_struct))
  expect_error(serialize_json(list(1L, 2L), vec_int_struct), "Type mismatch") # List vs vector

  # Map Fields
  expect_error(serialize_json(list(a = "1"), map_int_struct))
  expect_error(serialize_json(list(a = TRUE), map_int_struct))
  expect_error(serialize_json(list(a = 1.0), map_int_struct)) # Requires exact integer

  # Dates
  expect_error(serialize_json("2024-01-01", date_struct)) # Should be Date object
  expect_error(serialize_json(123, date_struct))
  expect_error(serialize_json(list(a = "2024-01-01"), map_date_struct)) # Requires Date type
})

test_that("Serialization fails for disallowed NA values", {
  vec_int_struct <- build_structure(s_vector(s_integer()))
  map_str_struct <- build_structure(s_map(a = s_string()))
  date_struct <- build_structure(s_date())

  expect_error(serialize_json(c(1L, NA, 3L), vec_int_struct))
  expect_error(serialize_json(list(a = NA_character_), map_str_struct))
  expect_error(serialize_json(as.Date(NA), date_struct))
})

test_that("Serialization fails for map structure mismatches", {
  map_struct <- build_structure(s_map(a = s_integer(), b = s_string()))

  # Missing field 'b' in R list
  expect_error(serialize_json(list(a = 1L), map_struct)) # Or similar error

  # R vector instead of list for map
  expect_error(serialize_json(c(a = 1L, b = "x"), map_struct))
})

test_that("Serialization handles empty inputs correctly", {
  vec_int_struct <- build_structure(s_vector(s_integer()))
  map_struct <- build_structure(s_map())
  vec_map_struct <- build_structure(s_vector(s_map(a = s_integer())))

  # Empty vector
  expect_equal(serialize_json(integer(0), vec_int_struct), json("[]"))

  # Empty map
  expect_equal(serialize_json(list(), map_struct), json("{}"))

  # Empty vector of maps
  expect_equal(serialize_json(list(), vec_map_struct), json("[]"))
})

# (End of added code)
