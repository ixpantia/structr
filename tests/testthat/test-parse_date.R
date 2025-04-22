test_that("Parses dates with default format (%Y-%m-%d)", {
  s_date_default <- build_structure(s_date()) # format = "%Y-%m-%d"

  expect_equal(
    parse_json('"2024-07-15"', s_date_default),
    as.Date("2024-07-15")
  )
  expect_equal(
    parse_json('"2000-01-01"', s_date_default),
    as.Date("2000-01-01")
  )
  # Leap year
  expect_equal(
    parse_json('"2024-02-29"', s_date_default),
    as.Date("2024-02-29")
  )
})

test_that("Parses dates with custom formats", {
  # Format: mm/dd/yyyy
  s_date_mdy <- build_structure(s_date(format = "%m/%d/%Y"))
  expect_equal(
    parse_json('"07/15/2024"', s_date_mdy),
    as.Date("2024-07-15")
  )

  # Format: dd-Mon-yyyy (e.g., 15-Jul-2024 - locale dependent month abbr!)
  # Note: This might be locale-dependent, using a numeric month is safer
  s_date_dmy_num <- build_structure(s_date(format = "%d-%m-%Y"))
  expect_equal(
    parse_json('"15-07-2024"', s_date_dmy_num),
    as.Date("2024-07-15")
  )

  # Format: YYYYMMDD
  s_date_compact <- build_structure(s_date(format = "%Y%m%d"))
  expect_equal(
    parse_json('"20240715"', s_date_compact),
    as.Date("2024-07-15")
  )
})

test_that("Throws error for invalid date formats", {
  s_date_default <- build_structure(s_date()) # format = "%Y-%m-%d"

  # Wrong separator
  expect_error(
    parse_json('"2024/07/15"', s_date_default)
  )
  # Wrong order
  expect_error(
    parse_json('"15-07-2024"', s_date_default)
  )

  # Extra characters
  expect_error(
    parse_json('"2024-07-15 extra"', s_date_default)
  )
})

test_that("Throws error for invalid date values (correct format, impossible date)", {
  s_date_default <- build_structure(s_date()) # format = "%Y-%m-%d"

  # Day 32
  expect_error(parse_json('"2024-07-32"', s_date_default))
  # Month 13
  expect_error(parse_json('"2024-13-01"', s_date_default))
  # Feb 30 (non-leap year)
  expect_error(parse_json('"2023-02-30"', s_date_default))
  # Feb 29 (non-leap year)
  expect_error(parse_json('"2023-02-29"', s_date_default))
})


test_that("Throws error when wrong JSON type is provided for date", {
  s_date_default <- build_structure(s_date())

  expect_error(parse_json('123', s_date_default))
  expect_error(parse_json('true', s_date_default))
  expect_error(parse_json('{}', s_date_default))
  expect_error(parse_json('[]', s_date_default))
  expect_error(parse_json('null', s_date_default)) # Error because not optional
})

test_that("Handles dates within maps", {
  s_map_date <- build_structure(s_map(
    event = s_string(),
    event_date = s_date(format = "%Y/%m/%d")
  ))

  json_valid <- '{"event": "Launch", "event_date": "2023/10/26"}'
  expected <- list(event = "Launch", event_date = as.Date("2023-10-26"))
  result <- parse_json(json_valid, s_map_date)
  expect_equal(result[order(names(result))], expected[order(names(expected))])

  # Error: incorrect date format in map
  json_invalid_fmt <- '{"event": "Launch", "event_date": "2023-10-26"}'
  expect_error(parse_json(json_invalid_fmt, s_map_date))

  # Error: incorrect date type in map
  json_invalid_type <- '{"event": "Launch", "event_date": 20231026}'
  expect_error(parse_json(json_invalid_type, s_map_date))
})

test_that("Handles dates within vectors", {
  s_vec_date <- build_structure(s_vector(s_date(format = "%d.%m.%Y")))

  json_valid <- '["15.07.2024", "16.07.2024"]'
  expected <- c(as.Date("2024-07-15"), as.Date("2024-07-16"))
  result <- parse_json(json_valid, s_vec_date)
  # Vectors of dates come back as Date objects directly
  expect_equal(result, expected)
  expect_s3_class(result, "Date")

  # Empty vector
  expect_equal(
    parse_json('[]', s_vec_date),
    structure(numeric(0), class = "Date")
  )

  # Error: incorrect date format in vector
  json_invalid_fmt <- '["15.07.2024", "2024-07-16"]'
  expect_error(parse_json(json_invalid_fmt, s_vec_date))

  # Error: incorrect type in vector
  json_invalid_type <- '["15.07.2024", true]'
  expect_error(parse_json(json_invalid_type, s_vec_date))
})

test_that("Handles optional dates correctly", {
  # Optional date at top level
  s_opt_date <- build_structure(s_optional(s_date()))
  expect_equal(parse_json('null', s_opt_date), NULL)
  expect_equal(parse_json('"2024-01-01"', s_opt_date), as.Date("2024-01-01"))
  expect_error(parse_json('"01/01/2024"', s_opt_date))
  expect_error(parse_json('123', s_opt_date))

  # Optional date field in map
  s_map_opt_date <- build_structure(s_map(
    id = s_integer(),
    expiry = s_optional(s_date(format = "%Y-%m-%d"))
  ))
  json_present <- '{"id": 1, "expiry": "2025-12-31"}'
  expect_equal(
    parse_json(json_present, s_map_opt_date),
    list(id = 1L, expiry = as.Date("2025-12-31"))
  )
  json_null <- '{"id": 2, "expiry": null}'
  expect_equal(
    parse_json(json_null, s_map_opt_date),
    list(id = 2L, expiry = NULL)
  )

  # Vector of optional dates
  s_vec_opt_date <- build_structure(s_vector(s_optional(s_date())))
  json_vec_opt <- '["2024-01-01", null, "2024-01-03"]'
  expected_vec_opt <- as.Date(c("2024-01-01", NA, "2024-01-03"))
  result_vec_opt <- parse_json(json_vec_opt, s_vec_opt_date)
  # The result should be a Date vector with NA for nulls
  expect_equal(result_vec_opt, expected_vec_opt)
  expect_s3_class(result_vec_opt, "Date")

  # Error: wrong type in optional vector element
  json_vec_opt_err <- '["2024-01-01", null, 123]'
  expect_error(parse_json(json_vec_opt_err, s_vec_opt_date))
})
