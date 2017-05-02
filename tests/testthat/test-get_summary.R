context("Creating a validation summary")

test_that("Getting a validation summary is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_character()` function to create
  # a validation step
  validation <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_character(column = "b") %>%
    interrogate()
  
  # Get a summary of the validation
  summary <- get_summary(validation)
  
  # Expect that the summary has specific column names
  expect_equivalent(
    colnames(summary),
    c("tbl_name", "db_type", "assertion_type", "column", "value",
      "set", "regex", "preconditions", "all_passed", "n", "n_passed",
      "f_passed", "action"))
  
  # Expect a single row in this summary
  expect_equivalent(nrow(summary), 1)
  
  # Expect certain column types for this summary
  expect_is(summary$tbl_name, "character")
  expect_is(summary$db_type, "character")
  expect_is(summary$assertion_type, "character")
  expect_is(summary$column, "character")
  expect_is(summary$value, "numeric")
  expect_is(summary$set, "list")
  expect_is(summary$regex, "character")
  expect_is(summary$preconditions, "list")
  expect_is(summary$all_passed, "logical")
  expect_is(summary$n, "numeric")
  expect_is(summary$n_passed, "numeric")
  expect_is(summary$f_passed, "numeric")
  expect_is(summary$action, "character")
  
  # Expect an error if getting a summary without an interrogation
  expect_error(
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_character(column = "b") %>%
    get_summary())
  
})
