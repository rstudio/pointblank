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
  expect_type(report$type, "character")
  expect_type(report$columns, "character")
  expect_type(report$values, "character")
  expect_type(report$precon, "character")
  expect_type(report$units, "double")
  expect_type(report$n_pass, "double")
  expect_type(report$f_pass, "double")
  expect_type(report$W, "logical")
  expect_type(report$S, "logical")
  expect_type(report$N, "logical")
  expect_type(report$extract, "integer")
  
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

test_that("col_vals_expr() shows used columns", {
  
  tbl <-
    dplyr::tibble(
      a = c(1, 2, 1, 7, 8, 6),
      b = c(0, 0, 0, 1, 1, 1),
      c = c(0.5, 0.3, 0.8, 1.4, 1.9, 1.2),
    )
  c <- 1
  
  agent <- tbl %>%
    create_agent() %>% 
    # Uses column `a`
    col_vals_expr(expr = ~ a %% 1 == 0) %>% 
    # Uses columns `a`, `b`, `c`
    col_vals_expr(expr = ~ case_when(
      b == 0 ~ a %>% between(0, 5) & c < 1,
      b == 1 ~ a > 5 & c >= 1
    )) %>% 
    # Uses columns `a` and `b`, and injects global variable `c`
    col_vals_expr(expr(.data$a + b == !!c)) %>% 
    # Uses column `a` but variable `d` is not defined (= not a column)
    col_vals_expr(expr = ~ a + d > 0) %>% 
    interrogate()
  
  columns <- agent$validation_set$column
  expect_equal(columns[[1]], "a")
  expect_setequal(columns[[2]], c("a", "b", "c"))
  expect_setequal(columns[[3]], c("a", "b"))
  expect_equal(columns[[4]], "a")
  
  report_columns <- get_agent_report(agent, display_table = FALSE)$columns
  
  expect_equal(report_columns[1], "a")
  expect_setequal(strsplit(report_columns[2], ", ")[[1]], c("a", "b", "c"))
  expect_setequal(strsplit(report_columns[3], ", ")[[1]], c("a", "b"))
  expect_equal(report_columns[4], "a")
  
})

test_that("report shows informative error tooltips", {
  
  df <- data.frame(date = "invalid date")
  agent <- create_agent(df) |> 
    col_vals_equal(date, Sys.Date()) |> 
    interrogate(progress = TRUE)
  report <- get_agent_report(agent)
  
  error_source <- agent$validation_set$capture_stack[[1]]$pb_call
  error_tooltip <- report$`_data`$eval_sym
  
  expect_equal(error_source, "tbl_val_comparison")
  expect_true(grepl(error_source, error_tooltip))
  
})

test_that("rows of report can be shuffled or dropped", {
  
  agent <- iris %>%  
    create_agent(actions = action_levels(warn_at = 1)) %>% 
    col_exists("Petal.Length",
               active = has_columns(iris, Petal.Length)) %>% 
    col_exists("skip",
               active = has_columns(iris, Spec)) %>% 
    col_exists("fail") %>% 
    interrogate()
  
  # Steps:
  # - 1) Pass (low severity)
  # - 2) Skip (no severity)
  # - 3) Fail (high severity)
  base <- get_agent_report(agent, display_table = FALSE)
  
  # Basic shuffling of row order via `arrange_by`
  shuffled <- get_agent_report(agent, arrange_by = "severity", display_table = FALSE)
  expect_identical(
    base[c(3, 1, 2),],
    shuffled
  )
  
  # With manual filtering of `$validation_set` (#563)
  agent2 <- agent
  agent2$validation_set <- agent2$validation_set[agent2$validation_set$eval_active, ]
  filtered <- get_agent_report(agent2, display_table = FALSE)
  expect_identical(
    base[c(1, 3), ],
    filtered
  )
  filtered_then_shuffled <- get_agent_report(agent2, arrange_by = "severity", display_table = FALSE)
  expect_identical(
    base[c(3, 1), ],
    filtered_then_shuffled
  )
  
  
})
