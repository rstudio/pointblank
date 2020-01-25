context("Creating an agent report")

test_that("Getting an agent report is possible", {
  
  # Use `col_is_character()` function to create
  # a validation step and then `interrogate()`
  agent <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = "b") %>%
    interrogate()
  
  # Get a report of the interrogation
  report <- get_agent_report(agent, display_table = FALSE)
  
  # Expect that the report has specific column names
  expect_equivalent(
    colnames(report),
    c("i", "type", "columns", "values", "precon", "eval",
      "units", "n_pass", "f_pass", "W", "S", "N", "extract")
  )
  
  # Expect a single row in this report
  expect_equivalent(nrow(report), 1)
  
  # Expect certain column types for this report
  expect_is(report$type, "character")
  expect_is(report$columns, "character")
  expect_is(report$values, "character")
  expect_is(report$precon, "character")
  expect_is(report$units, "numeric")
  expect_is(report$n_pass, "numeric")
  expect_is(report$f_pass, "numeric")
  expect_is(report$W, "logical")
  expect_is(report$S, "logical")
  expect_is(report$N, "logical")
  expect_is(report$extract, "integer")
  
  
  agent_report_empty <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = "b") %>%
    get_agent_report(display_table = FALSE)
  
  # Expect certain values in the single row report
  expect_equal(agent_report_empty$type, "col_is_character")
  expect_equal(agent_report_empty$columns, "b")
  expect_equal(agent_report_empty$values, NA_character_)
  expect_equal(agent_report_empty$precon, NA_character_)
  expect_equal(agent_report_empty$units, NA_integer_)
  expect_equal(agent_report_empty$n_pass, NA_integer_)
  expect_equal(agent_report_empty$f_pass, NA_real_)
  expect_equal(agent_report_empty$W, NA)
  expect_equal(agent_report_empty$S, NA)
  expect_equal(agent_report_empty$N, NA)
  expect_equal(agent_report_empty$extract, NA)
})

test_that("The `all_passed()` function works", {
  
  agent <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = "b") %>%
    col_vals_gt(vars(c), 1, na_pass = TRUE) %>%
    interrogate()
  
  agent %>% all_passed() %>% expect_true()
  
  agent <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = "b") %>%
    col_vals_gt(vars(c), 1, na_pass = FALSE) %>%
    interrogate()
  
  agent %>% all_passed() %>% expect_false()
  
  expect_error(
    create_agent(tbl = small_table) %>%
      col_is_character(columns = "b") %>%
      col_vals_gt(vars(c), 1, na_pass = FALSE) %>%
      all_passed()
  )
})
