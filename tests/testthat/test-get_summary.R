context("Creating a validation summary")

test_that("Getting a validation summary is possible", {
  
  small_table <-
    readr::read_csv(
      system.file("extdata", "small_table.csv", package = "pointblank"),
      col_types = "TDicddlc")
  
  # Use `col_is_character()` function to create
  # a validation step and then `interrogate()`
  agent <-
    create_agent(tbl = small_table) %>%
    col_is_character(column = "b") %>%
    interrogate()
  
  # Get a summary of the interrogation
  summary <- get_interrogation_summary(agent)
  
  # Expect that the summary has specific column names
  expect_equivalent(
    colnames(summary),
    c("assertion_type", "column", "value", "set", "regex",
      "preconditions", "all_passed", "n", "f_passed", "n_passed",
      "action", "brief"))
  
  # Expect a single row in this summary
  expect_equivalent(nrow(summary), 1)
  
  # Expect certain column types for this summary
  expect_is(summary$assertion_type, "character")
  expect_is(summary$column, "list")
  expect_is(summary$value, "numeric")
  expect_is(summary$set, "list")
  expect_is(summary$regex, "character")
  expect_is(summary$preconditions, "list")
  expect_is(summary$all_passed, "logical")
  expect_is(summary$n, "integer")
  expect_is(summary$f_passed, "numeric")
  expect_is(summary$n_passed, "integer")
  expect_is(summary$action, "character")
  expect_is(summary$brief, "character")
  
  # Expect an error if getting a summary without an interrogation
  expect_error(
    create_agent(tbl = small_table) %>%
      col_is_character(column = "b") %>%
      get_interrogation_summary()
  )
})
