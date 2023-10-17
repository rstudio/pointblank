test_that("Full range of tidyselect features available in column selection", {
  
  tbl <- data.frame(x = 1:2, y = 1:2, yy = 1:2, nonunique = 1)
  
  # Single symbol
  expect_success(expect_rows_distinct(tbl, x))
  expect_failure(expect_rows_distinct(tbl, nonunique))
  
  # Backward-compatibility with `vars()` syntax
  expect_success(expect_rows_distinct(tbl, vars(x)))
  expect_success(expect_rows_distinct(tbl, vars(x, nonunique)))
  expect_failure(expect_rows_distinct(tbl, vars(nonunique)))
  
  # Preferred {tidyselect}-style `c()` syntax
  expect_success(expect_rows_distinct(tbl, c(x)))
  expect_success(expect_rows_distinct(tbl, c(x, nonunique)))
  expect_failure(expect_rows_distinct(tbl, c(nonunique)))
  
  # {tidyselect} functions
  expect_success(expect_rows_distinct(tbl, tidyselect::all_of("x")))
  expect_success(expect_rows_distinct(tbl, tidyselect::all_of(c("x", "nonunique"))))
  expect_failure(expect_rows_distinct(tbl, tidyselect::all_of("nonunique")))
  
  # NEW: {tidyselect} integer indexing
  expect_success(expect_rows_distinct(tbl, 1))
  expect_success(expect_rows_distinct(tbl, c(1, 4)))
  expect_failure(expect_rows_distinct(tbl, 4))
  
  # NEW: {tidyselect} negative indexing
  expect_success(expect_rows_distinct(tbl, -(2:4)))
  expect_success(expect_rows_distinct(tbl, -(2:3)))
  expect_failure(expect_rows_distinct(tbl, -(1:3)))
  
  # NEW: {tidyselect} functions in complex expressions
  exist_col <- "y"
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::all_of(exist_col))))
  nonexist_col <- "z"
  expect_error(expect_rows_distinct(tbl, c(x, tidyselect::all_of(nonexist_col))))
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::any_of(nonexist_col))))
  
  # DEPRECATION: Supplying a character vector variable still works, but signals deprecation:
  options(lifecycle_verbosity = "warning")
  expect_success(expect_warning(
    expect_rows_distinct(tbl, exist_col),
    "Using an external vector in selections was deprecated in tidyselect 1.1.0."
  ))
  
})
