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
    col_is_character(columns = "b") %>%
    interrogate()
  
  # Get a summary of the interrogation
  summary <- get_agent_report(agent)
  
  # Expect that the summary has specific column names
  expect_equivalent(
    colnames(summary),
    c("i", "type", "columns", "value", "set", "regex", "preconds",
      "units", "n_pass", "f_pass", "state", "extract")
  )
  
  # Expect a single row in this summary
  expect_equivalent(nrow(summary), 1)
  
  # Expect certain column types for this summary
  expect_is(summary$type, "character")
  expect_is(summary$columns, "character")
  expect_is(summary$value, "numeric")
  expect_is(summary$set, "character")
  expect_is(summary$regex, "character")
  expect_is(summary$preconds, "logical")
  expect_is(summary$units, "numeric")
  expect_is(summary$n_pass, "numeric")
  expect_is(summary$f_pass, "numeric")
  expect_is(summary$state, "character")
  expect_is(summary$extract, "logical")
})
