context("Creating an agent report")

test_that("Getting an agent report is possible", {
  
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
  
  # Get a report of the interrogation
  report <- get_agent_report(agent)
  
  # Expect that the report has specific column names
  expect_equivalent(
    colnames(report),
    c("i", "type", "columns", "value", "set", "regex", "preconds",
      "units", "n_pass", "f_pass", "state", "extract")
  )
  
  # Expect a single row in this report
  expect_equivalent(nrow(report), 1)
  
  # Expect certain column types for this report
  expect_is(report$type, "character")
  expect_is(report$columns, "character")
  expect_is(report$value, "numeric")
  expect_is(report$set, "character")
  expect_is(report$regex, "character")
  expect_is(report$preconds, "logical")
  expect_is(report$units, "numeric")
  expect_is(report$n_pass, "numeric")
  expect_is(report$f_pass, "numeric")
  expect_is(report$state, "character")
  expect_is(report$extract, "logical")
  
  
  agent_report_empty <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = "b") %>%
    get_agent_report()
  
  # Expect certain values in the single row report
  expect_equal(agent_report_empty$type, "col_is_character")
  expect_equal(agent_report_empty$columns, "b")
  expect_equal(agent_report_empty$value, NA_real_)
  expect_equal(agent_report_empty$set, NA_character_)
  expect_equal(agent_report_empty$regex, NA_character_)
  expect_equal(agent_report_empty$preconds, FALSE)
  expect_equal(agent_report_empty$units, NA_integer_)
  expect_equal(agent_report_empty$n_pass, NA_integer_)
  expect_equal(agent_report_empty$f_pass, NA_real_)
  expect_equal(agent_report_empty$state, NA_character_)
  expect_equal(agent_report_empty$extract, NA)
})
