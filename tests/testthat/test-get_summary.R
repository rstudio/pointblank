context("Creating a validation summary")

test_that("Getting a validation summary is possible", {
  
  library(tibble)
  library(dplyr)
  
  # Use `col_is_character()` function to create
  # a validation step and then `interrogate()`
  agent <-
    create_agent() %>%
    focus_on(
      file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
    col_is_character(column = "b") %>%
    interrogate()
  
  # Get a summary of the interrogation
  summary <- get_interrogation_summary(agent)
  
  # Expect that the summary has specific column names
  expect_equivalent(
    colnames(summary),
    c("tbl_name", "db_type", "assertion_type", "column", "value",
      "regex", "all_passed", "n", "n_passed",
      "f_passed", "action", "brief"))
  
  # Expect a single row in this summary
  expect_equivalent(nrow(summary), 1)
  
  # Expect certain column types for this summary
  expect_is(summary$tbl_name, "character")
  expect_is(summary$db_type, "character")
  expect_is(summary$assertion_type, "character")
  expect_is(summary$column, "character")
  expect_is(summary$value, "numeric")
  expect_is(summary$regex, "character")
  expect_is(summary$all_passed, "logical")
  expect_is(summary$n, "numeric")
  expect_is(summary$n_passed, "numeric")
  expect_is(summary$f_passed, "numeric")
  expect_is(summary$action, "character")
  expect_is(summary$brief, "character")
  
  # Expect an error if getting a summary without an interrogation
  expect_error(
    create_agent() %>%
      focus_on(
        file_name = system.file("extdata", "small_table.csv", package = "pointblank")) %>%
      col_is_character(column = "b") %>%
      get_interrogation_summary())
  
})
