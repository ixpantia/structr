# --- Benchmark Setup ---
# install.packages(c("structr", "jsonlite", "microbenchmark"))

library(structr)
library(jsonlite)
library(microbenchmark)

# --- Test Data and Structures ---

# Scenario 1: Simple Flat Object
simple_json <- '{"id": 123, "name": "Example Item", "value": 99.95, "active": true}'
simple_schema <- s_map(
  id = s_integer(),
  name = s_string(),
  value = s_double(),
  active = s_logical()
)
# Pre-build the structr structure (outside the benchmark)
simple_structure <- build_structure(simple_schema)


# Scenario 2: Nested Object with Arrays
nested_json <- '{
  "orderId": "ORD-001",
  "customer": {
    "name": "Alice",
    "email": "alice@example.com",
    "addresses": [
      {"street": "123 Main St", "city": "Anytown", "zip": "12345"},
      {"street": "456 Oak Ave", "city": "Otherville", "zip": "67890"}
    ]
  },
  "items": [
    {"sku": "A1", "qty": 2, "price": 10.50},
    {"sku": "B2", "qty": 1, "price": 25.00}
  ],
  "total": 46.00
}'
nested_schema <- s_map(
  orderId = s_string(),
  customer = s_map(
    name = s_string(),
    email = s_string(),
    addresses = s_vector(
      s_map(street = s_string(), city = s_string(), zip = s_string())
    )
  ),
  items = s_vector(
    s_map(sku = s_string(), qty = s_integer(), price = s_double())
  ),
  total = s_double()
)
# Pre-build the structr structure
nested_structure <- build_structure(nested_schema)


# Scenario 3: Array of Simple Objects
array_json <- '[
  {"ticker": "AAPL", "price": 170.34, "change": -1.2},
  {"ticker": "GOOGL", "price": 2800.50, "change": 5.1},
  {"ticker": "MSFT", "price": 300.80, "change": 0.5},
  {"ticker": "AMZN", "price": 3100.00, "change": -10.0}
]'
array_schema <- s_vector(
  s_map(
    ticker = s_string(),
    price = s_double(),
    change = s_double()
  )
)
# Pre-build the structr structure
array_structure <- build_structure(array_schema)


# --- Run Benchmarks ---

# Set number of repetitions (adjust as needed)
n_times <- 100

cat("--- Benchmarking: Simple Object ---\n")
bm_simple <- microbenchmark(
  structr = structr::parse_json(simple_json, simple_structure),
  jsonlite = jsonlite::fromJSON(simple_json),
  times = n_times,
  unit = "ms" # Milliseconds might be appropriate, adjust if needed
)
print(bm_simple)

cat("\n--- Benchmarking: Nested Object ---\n")
bm_nested <- microbenchmark(
  structr = structr::parse_json(nested_json, nested_structure),
  jsonlite = jsonlite::fromJSON(nested_json),
  times = n_times,
  unit = "ms"
)
print(bm_nested)

cat("\n--- Benchmarking: Array of Objects ---\n")
bm_array <- microbenchmark(
  structr = structr::parse_json(array_json, array_structure),
  jsonlite = jsonlite::fromJSON(array_json),
  times = n_times,
  unit = "ms"
)
print(bm_array)

# --- Benchmarking: Simple Object ---
# Unit: milliseconds
#      expr      min       lq       mean    median        uq      max neval
#   structr 0.018943 0.022263 0.02546514 0.0237330 0.0252595 0.174436   100
#  jsonlite 0.029641 0.030514 0.03637719 0.0314455 0.0329575 0.411497   100
#
# --- Benchmarking: Nested Object ---
# Unit: milliseconds
#      expr      min        lq       mean    median       uq      max neval
#   structr 0.028181 0.0326355 0.03974463 0.0356530 0.039387 0.247207   100
#  jsonlite 0.137339 0.1443770 0.15662240 0.1537135 0.158525 0.554185   100
#
# --- Benchmarking: Array of Objects ---
# Unit: milliseconds
#      expr      min        lq       mean   median       uq      max neval
#   structr 0.023753 0.0271000 0.03136015 0.028422 0.031495 0.087143   100
#  jsonlite 0.062603 0.0685195 0.07453725 0.071602 0.074944 0.172419   100
