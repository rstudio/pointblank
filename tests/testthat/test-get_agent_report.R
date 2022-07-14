skip_on_cran()

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
    c("i", "type", "columns", "values", "precon", "active", "eval",
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
  
  # Use `col_is_character()` function to create
  # a validation step but do not `interrogate()`
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
  
  # The following agent will perform an interrogation that results
  # in all test units in an validation steps to have zero failures
  agent <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = "b") %>%
    col_vals_gt(vars(c), 1, na_pass = TRUE) %>%
    interrogate()
  
  # Expect that the interrogation has a completely
  # 'all passed' state (returning TRUE)
  agent %>% all_passed() %>% expect_true()
  
  # The following agent will perform an interrogation that results
  # in two test units failing in the 2nd validation step (the first
  # *did* pass)
  agent <-
    create_agent(tbl = small_table) %>%
    col_is_character(columns = "b") %>%
    col_vals_gt(vars(c), 1, na_pass = FALSE) %>%
    interrogate()
  
  # Expect that the interrogation *does not* have
  # a completely 'all passed' state (returning FALSE)
  agent %>% all_passed() %>% expect_false()
  
  # The following agent will perform an interrogation that results
  # in all test units passing in the second validation step, but
  # the first experiences an evaluation error (since column
  # `z` doesn't exist in `small_table`)
  agent <-
    create_agent(tbl = small_table) %>%
    col_vals_not_null(vars("z")) %>%
    col_vals_gt(vars(c), 1, na_pass = TRUE) %>%
    interrogate()
  
  # Expect that the interrogation *does not* have
  # a completely 'all passed' state (returning FALSE)
  agent %>% all_passed() %>% expect_false()
  
  # If narrowing the `all_passed()` evaluation to only
  # the second validation step, then we should expect TRUE
  agent %>% all_passed(i = 2) %>% expect_true()
  
  # If narrowing the `all_passed()` evaluation to only
  # the first validation step, then we should expect FALSE
  agent %>% all_passed(i = 1) %>% expect_false()
  
  # Expect an NA value if an interrogation was carried
  # out by an agent with no validation steps
  expect_equal(
    create_agent(tbl = small_table) %>%
      interrogate() %>%
      all_passed(),
    NA
  )
  
  # Expect an error if `all_passed()` is called on an
  # agent that did not yet perform an interrogation
  expect_error(
    create_agent(tbl = small_table) %>%
      col_is_character(columns = "b") %>%
      col_vals_gt(vars(c), 1, na_pass = FALSE) %>%
      all_passed()
  )
  
  # Expect an error if `all_passed()` has an `i` vector
  # with element values not in the range of validation
  # step indices
  expect_error(
    create_agent(tbl = small_table) %>%
      col_is_character(columns = "b") %>%
      col_vals_gt(vars(c), 1, na_pass = FALSE) %>%
      interrogate() %>%
      all_passed(i = 2:3)
  )
  
  # Expect an error if `all_passed()` has an `i` vector
  # and there are no validation steps for the agent
  # with intel
  expect_error(
    create_agent(tbl = small_table) %>%
      interrogate() %>%
      all_passed(i = 1)
  )
})

test_that("The correct title is rendered in the agent report", {
  
  the_table_name <- "small_table"
  
  agent <-
    create_agent(
      tbl = small_table,
      tbl_name = "small_table",
      label = "Agent Report"
    ) %>%
    col_vals_gt(vars(a), 0) %>%
    interrogate()
  
  # expect_match(
  #   get_agent_report(agent) %>%
  #     gt::as_raw_html(inline_css = FALSE),
  #   ">Pointblank Validation</th>",
  #   fixed = TRUE
  # )
  
  # expect_match(
  #   get_agent_report(agent, title = ":default:") %>%
  #     gt::as_raw_html(inline_css = FALSE),
  #   ">Pointblank Validation</th>",
  #   fixed = TRUE
  # )
  
  # expect_match(
  #   get_agent_report(agent, title = ":tbl_name:") %>%
  #     gt::as_raw_html(inline_css = FALSE),
  #   "><code>small_table</code></th>",
  #   fixed = TRUE
  # )
  
  # expect_match(
  #   get_agent_report(agent, title = "The validation of `small_tbl`") %>%
  #     gt::as_raw_html(inline_css = FALSE),
  #   ">The validation of <code>small_tbl</code></th>",
  #   fixed = TRUE
  # )
  
  # expect_match(
  #   get_agent_report(agent, title = I("The validation of `small_tbl`")) %>%
  #     gt::as_raw_html(inline_css = FALSE),
  #   ">The validation of `small_tbl`</th>",
  #   fixed = TRUE
  # )

  # expect_match(
  #   get_agent_report(agent, title = glue::glue("The validation of `{the_table_name}`")) %>%
  #     gt::as_raw_html(inline_css = FALSE),
  #   ">The validation of <code>small_table</code></th>",
  #   fixed = TRUE
  # )
  
  # expect_match(
  #   get_agent_report(agent, title = glue::glue("The validation of `{agent$tbl_name}`")) %>%
  #     gt::as_raw_html(inline_css = FALSE),
  #   ">The validation of <code>small_table</code></th>",
  #   fixed = TRUE
  # )
  
  
  # expect_false(
  #   grepl(
  #     ">Pointblank Validation</th>",
  #     get_agent_report(agent, title = I(NA)) %>%
  #       gt::as_raw_html(inline_css = FALSE),
  #     fixed = TRUE
  #   )
  # )
  # expect_false(
  #   grepl(
  #     ">Pointblank Validation</th>",
  #     get_agent_report(agent, title = ":none:") %>%
  #       gt::as_raw_html(inline_css = FALSE),
  #     fixed = TRUE
  #   )
  # )
  # expect_false(
  #   grepl(
  #     ">Pointblank Validation</th>",
  #     get_agent_report(agent, title = "") %>%
  #       gt::as_raw_html(inline_css = FALSE),
  #     fixed = TRUE
  #   )
  # )
  # expect_false(
  #   grepl(
  #     ">Pointblank Validation</th>",
  #     get_agent_report(agent, title = NULL) %>%
  #       gt::as_raw_html(inline_css = FALSE),
  #     fixed = TRUE
  #   )
  # )
  # expect_false(
  #   grepl(
  #     ">Pointblank Validation</th>",
  #     get_agent_report(agent, title = NA) %>%
  #       gt::as_raw_html(inline_css = FALSE),
  #     fixed = TRUE
  #   )
  # )
  # expect_false(
  #   grepl(
  #     ">Pointblank Validation</th>",
  #     get_agent_report(agent, title = I(NA)) %>%
  #       gt::as_raw_html(inline_css = FALSE),
  #     fixed = TRUE
  #   )
  # )
  # expect_false(
  #   grepl(
  #     ">Pointblank Validation</th>",
  #     get_agent_report(agent, title = I("")) %>%
  #       gt::as_raw_html(inline_css = FALSE),
  #     fixed = TRUE
  #   )
  # )
})
