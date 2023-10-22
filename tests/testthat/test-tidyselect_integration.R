test_that("Full range of tidyselect features available in column selection", {
  
  tbl <- data.frame(x = 1:2, y = 1:2, nonunique = "A")
  
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
  expect_success(expect_rows_distinct(tbl, c(1, 3)))
  expect_failure(expect_rows_distinct(tbl, 3))
  
  # NEW: {tidyselect} negative indexing
  expect_success(expect_rows_distinct(tbl, -(2:3)))
  expect_success(expect_rows_distinct(tbl, -2))
  expect_failure(expect_rows_distinct(tbl, -(1:2)))
  
  # NEW: {tidyselect} `where()` predicate:
  expect_success(expect_rows_distinct(tbl, !tidyselect::where(is.character)))
  expect_success(expect_rows_distinct(tbl, tidyselect::where(is.numeric)))
  expect_failure(expect_rows_distinct(tbl, tidyselect::where(is.character)))
  
  # NEW: {tidyselect} functions in complex expressions
  exist_col <- "y"
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::all_of(exist_col))))
  nonexist_col <- "z"
  expect_error(expect_rows_distinct(tbl, c(x, tidyselect::all_of(nonexist_col))))
  expect_success(expect_rows_distinct(tbl, c(x, tidyselect::any_of(nonexist_col))))
  
  # Supplying a character vector variable still works, but signals deprecation:
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_success(expect_warning(
    expect_rows_distinct(tbl, exist_col),
    "Using an external vector in selections was deprecated in tidyselect 1.1.0."
  ))
  
})


test_that("'NULL = select everything' behavior in rows_*() validation functions", {

  # For `rows_*()` functions specifically, empty/NULL = "select everything" behavior:
  expect_success(expect_rows_distinct(data.frame(x = 1, y = 2)))
  expect_success(expect_rows_complete(data.frame(x = 1, y = 2)))
  expect_failure(expect_rows_distinct(data.frame(x = c(1, 1))))
  expect_failure(expect_rows_complete(data.frame(x = c(1, NA))))
  expect_success(expect_rows_distinct(data.frame(x = 1, y = 2), columns = NULL))
  expect_success(expect_rows_complete(data.frame(x = 1, y = 2), columns = NULL))
  expect_failure(expect_rows_distinct(data.frame(x = c(1, 1)), columns = NULL))
  expect_failure(expect_rows_complete(data.frame(x = c(1, NA)), columns = NULL))
  
  # Report shows all column names with empty `columns` argument
  expect_equal({
    small_table %>%
      create_agent() %>% 
      rows_distinct() %>% 
      rows_complete() %>% 
      interrogate() %>% 
      {.$validation_set$column} %>% 
      unlist() %>% 
      unique()
  }, toString(colnames(small_table)))
  
  # Report shows all column names with explicit NULL `columns` argument
  expect_equal({
    small_table %>%
      create_agent() %>% 
      rows_distinct(columns = NULL) %>% 
      rows_complete(columns = NULL) %>% 
      interrogate() %>% 
      {.$validation_set$column} %>% 
      unlist() %>% 
      unique()
  }, toString(colnames(small_table)))
    
})
